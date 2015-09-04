{-# LANGUAGE BangPatterns, GeneralizedNewtypeDeriving #-}
module Updater.Internal (
	Event (..),
	Behavior (..),
 	Updater (..),
--  	getEvent',
--  	getBehavior',
	newEvent',
	cacheStateless',
	cacheStateful',
 	runUpdater,
 	unsafeLiftIO,
	debug,
	debugCleanup,
	onCommit
	) where

import Control.Concurrent.MVar
--import GHC.Conc.Sync hiding (modifyMVar_)
import qualified Updater.List as List

import Control.Applicative
import Control.Monad
import Data.Monoid
-- import Control.Exception.Base
import Control.Monad.Fix
import System.Mem.Weak
import Debug.Trace
import Data.IORef
import System.IO.Unsafe

newtype Event a = Event { getEvent' :: Updater a } deriving (Functor, Applicative, Alternative, Monad)

instance Monoid (Event a) where
	mempty = empty
	mappend = (<|>)

newtype Behavior a = Behavior { getBehavior' :: Updater a } deriving (Functor, Applicative, Monad, MonadFix)

unsafeLiftIO :: IO a -> Behavior a
unsafeLiftIO = Behavior . liftIO

globalLock :: MVar ()
{-# NOINLINE globalLock #-}
globalLock = unsafePerformIO $ newMVar ()

signalNumVar :: MVar Int
{-# NOINLINE signalNumVar #-}
signalNumVar = unsafePerformIO $ newMVar 1

withGlobalLock :: IO a -> IO a
withGlobalLock io = do
	takeMVar globalLock
	res <- io
	putMVar globalLock ()
	return res



-- |
-- Just for some quick debugging
--
-- >putLine = onCommit . putStrLn
debug :: String -> Behavior ()
debug = unsafeLiftIO . putStrLn

debugCleanup :: String -> Behavior ()
debugCleanup string = Behavior $ Updater $ \restCalc downState -> do
	upState <- restCalc () downState
	return $ mempty { stateOnCleanup = putStrLn string } <> upState

onCommit :: IO () -> Behavior ()
onCommit io = Behavior $ Updater $ \restCalc downState -> do
	upState <- restCalc () downState
	return $ mempty { stateOnCommit = io } <> upState

--- START: SIGNALS ---

-- |
-- `Signal` is the portable Signal they can be exchanged between
-- any parts of your program. Internally, they are just a variable and a list of 
-- change hooks.
data Signal a = Signal {
	signalValue :: IORef a,
	signalListeners :: List.LinkedList (Weak (Signal a, a -> DownState -> IO UpState)),
	signalNum :: Int
	}

newSignal :: a -> IO (Signal a)
newSignal a = do
	value <- newIORef a
	listeners <- List.empty
	num <- modifyMVar signalNumVar $ \n -> return (n+1,n)
	putStrLn (show num ++ ": new signal")
	return (Signal value listeners num)

readSignal :: Signal a -> IO a
readSignal signal = readIORef $ signalValue signal

-- |
writeSignal :: Signal a -> a -> DownState -> IO UpState
writeSignal (Signal valueVar listeners num) value downState = do
	writeIORef valueVar value
	list <- List.toList listeners
	putStrLn (show num ++ ": length: " ++ show (length list))
	let f weakRef = do
			res <- deRefWeak weakRef
			case res of
				 (Just (_,listener)) -> listener value downState
				 _ -> return mempty
	upStates <- mapM f list
	return (foldl (<>) mempty upStates)

-- |
-- The return value will remove the listener.
-- IMPORTANT: If the remover gets garbage
-- collected the listener will be removed.
-- any references from the listener to the
-- remover don't count.
addListener :: Signal a -> (a -> DownState -> IO UpState) -> IO (IO ())
addListener signal listener = do
	let listener' a downState = putStrLn (show (signalNum signal) ++ ": runListener") >> listener a downState
	putStrLn $ (show $ signalNum signal) ++ ": add listener"
	weakRef <- newIORef (error "should not be readable")
	node <- List.append (unsafePerformIO $ readIORef weakRef) (signalListeners signal)
	-- next who lines are just so (signal, listeners) won't be collected
	key <- newIORef undefined
	let remove = (List.delete node) >> newIORef key >> return ()
	weak <- mkWeak key (signal, listener') $ Just $ do
		putStrLn $ show (signalNum signal) ++ ": cleaning up signal"
		remove
	writeIORef weakRef weak
	return (remove >> putStrLn ((show $ signalNum signal) ++": remove listener"))

--- END: SIGNALS ---

data DownState = DownState {
	}

data UpState = UpState {
	stateOnCleanup :: IO (),
	stateOnCommit :: IO ()
	}

instance Monoid UpState where
	mempty = UpState (return ()) (return ())
	(UpState c1 d1) `mappend` (UpState c2 d2) = UpState (c1 >> c2) (d1 >> d2)

-- |
-- This monad works very similar to a continuation monad on top of stm.
-- You can do any basic stm computation you want simply using `liftSTM`.
-- However, if you use `getEvent` everything after that call will be executed
-- everytime the `Signal` given to `getEvent` is changed.
--
-- You can also use the `Alternative` instance to make a union of events.
--
-- You can also use the `Applicative` instance to run two things \'parallel\'.
-- Parallel meaning that events on one side will not cause the other 
-- side to be reevaluated completely.
newtype Updater a = Updater { 
	runUpdater' :: (a -> DownState -> IO UpState) -> DownState -> IO UpState
	}

-- it is important this not be used for Updaters that can fire multiple times
-- it can only be used for Continuous
instance MonadFix Updater where
	mfix = fixUpdater

-- it is important this not be used for Updaters that can fire multiple times
-- it can only be used for Continuous
fixUpdater :: (a -> Updater a) -> Updater a
fixUpdater toUpdater = Updater $ \restCalc downState -> do
	inputVar <- newEmptyMVar
	runUpdater' (toUpdater $ unsafePerformIO $ takeMVar inputVar)
		(\x downState2 -> do
			putMVar inputVar x
			restCalc x downState2
			)
		downState

cacheStateful' :: Updater a -> Updater (Updater a)
cacheStateful' updater = Updater $ \restCalc downState->  do
	signal <- newSignal Nothing
	cleanup <- newIORef (return ())

	upstate1 <- restCalc (Updater $ \restCalc2 downState2 -> do
		res <- readSignal signal
		upState <- case res of
			 (Just res') -> do
				upState' <- restCalc2 res' downState2
				oldCleanup <- readIORef cleanup
				writeIORef cleanup (oldCleanup >> stateOnCleanup upState')
				return upState' { stateOnCleanup = join $ readIORef cleanup } 
			 Nothing -> return mempty
		removeListener <- addListener signal (\x downState3 -> case x of
			(Just x') -> putStrLn "restCalc2" >> restCalc2 x' downState3
			Nothing -> return mempty)
		return $ upState <> mempty { stateOnCleanup = removeListener }
		) downState

	upstate2 <- runUpdater' updater
		(\x downState' -> do
			join $ readIORef cleanup
			upState <- writeSignal signal (Just x) downState'
			writeIORef cleanup (stateOnCleanup upState)
			return upState { stateOnCleanup = join $ readIORef cleanup }
			)
		downState

	return (upstate1 <> upstate2)

cacheStateless' :: Updater a -> Updater (Updater a)
cacheStateless' updater = Updater $ \restCalc downState->  do
	signal <- newSignal (error "unreadable event")
	cleanup <- newIORef (return ())

	upstate1 <- restCalc (Updater $ \restCalc2 _ -> do
		removeListener <- addListener signal restCalc2
		return $ mempty { stateOnCleanup = removeListener }
		) downState

	upstate2 <- runUpdater' updater
		(\x downState' -> do
			join $ readIORef cleanup
			upState <- writeSignal signal x downState'
			writeIORef cleanup (stateOnCleanup upState)
			return upState { stateOnCleanup = join $ readIORef cleanup }
			)
		downState
	return (upstate1 <> upstate2)

newEvent' :: IO (Updater a, a -> IO ())
newEvent' = do
	signal <- newSignal (error "unreadable")
	cleanupVar <- newMVar (return () :: IO ())
	let
		updater = Updater $ \restCalc _ -> do
			removeListener <- addListener signal (\a downState2 -> restCalc a downState2)
			return mempty { stateOnCleanup = removeListener }
		button a = do
			takeMVar globalLock
			join $ takeMVar cleanupVar
			upState <- writeSignal signal a (error "no down state yet")
			stateOnCommit upState
			putMVar cleanupVar (stateOnCleanup upState)
			putMVar globalLock ()
	return (updater, button)

runUpdater :: Updater (Either (IO ()) res) -> IO res
runUpdater (Updater giveMeNext) = do
	resVar <- newEmptyMVar

	upState <- withGlobalLock $ do
		giveMeNext (\val _ -> do
			resMay <-isEmptyMVar resVar
			if resMay
				then case val of
					(Left io) -> return mempty { stateOnCommit = io }
					(Right res) -> putMVar resVar res >> return mempty
				else return mempty
			) DownState {}

	stateOnCommit upState

	res <- takeMVar resVar
	withGlobalLock $ stateOnCleanup upState
	return res



liftIO :: IO a -> Updater a
liftIO run = Updater (\restCalc state -> run >>= (\x -> restCalc x state))

--- START: INSTANCES ---

-- TODO: cleanup 
instance Applicative Updater where
	pure a = Updater $ \giveMeA -> giveMeA a
 	(Updater giveMeNext1) <*> (Updater giveMeNext2) = Updater $ \restCalc state -> do
		varF <- newIORef Nothing
		varX <- newIORef Nothing
		varCleanup <- newIORef $ return ()

		let update state' = do
				f' <- readIORef varF
				x' <- readIORef varX
				case (f', x') of
					(Just f, Just x) -> do
						join $ readIORef varCleanup
						upstateC <- restCalc (f x) state'
						writeIORef varCleanup $ stateOnCleanup upstateC
						return $ upstateC {
							stateOnCleanup = return ()
							}
					_ -> return mempty

		upState1 <- giveMeNext1 (\x state' -> writeIORef varF (Just x) >> update state') state
		upState2 <- giveMeNext2 (\x state' -> writeIORef varX (Just x) >> update state') state

		return $ upState1 `mappend` upState2 `mappend` mempty { 
			stateOnCleanup = join $ readIORef varCleanup
			}

instance Alternative Updater where
	empty = Updater $ \_ _ -> return mempty
	(Updater giveMeNext1) <|> (Updater giveMeNext2) = Updater $ \restCalc state -> do
		var <-newIORef (error "should not be accessed")
		varCleanup <- newIORef $ return ()

		let update state' = do
			val <- readIORef var
			join (readIORef varCleanup)
			upstate <- restCalc val state' 
			writeIORef varCleanup $ stateOnCleanup upstate
			return $ upstate {
				stateOnCleanup = return ()
				}

		cleanup1 <- giveMeNext1 (\x state' -> writeIORef var x >> update state') state
		cleanup2 <-giveMeNext2 (\x state' -> writeIORef var x >> update state') state

		return $ cleanup1 `mappend` cleanup2 `mappend` mempty {
			stateOnCleanup = join $ readIORef varCleanup
			}

instance Monad Updater where
	(Updater giveMeNext) >>= valueToNextUpd = Updater $ updater where
		updater end = 	giveMeNext $  \value -> runUpdater' (valueToNextUpd value) end
	return a = Updater $ \end -> end a
	fail _ = Updater $ \_ _ -> return mempty

instance Functor Updater where
	fmap f (Updater giveMeNext) = Updater (\next -> giveMeNext (next . f))

--- END: INSTANCES ---