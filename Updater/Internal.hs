{-# LANGUAGE BangPatterns, RankNTypes #-}
module Updater.Internal (
 	-- Signals
--  	Signal(),
  	newSignal,
-- 	newSignalIO,
--  	writeSignal,
-- 	readSignal,
  	addListener,
-- 	-- Updater
-- 	Updater(),
-- 	onCommit,
-- 	getEvent,
-- 	getBehavior,
-- 	runUpdater,
-- --	getCleanup,
-- 	liftIO,
-- 	onCleanup
	) where

import Control.Concurrent.MVar
import GHC.Conc.Sync hiding (modifyMVar_)
import qualified Updater.List as List

import Control.Applicative
import Control.Monad
import Data.Monoid
import Control.Exception.Base
import Control.Monad.Fix
import System.Mem.Weak
import Debug.Trace
import Data.IORef
import System.IO.Unsafe


globalLock :: MVar ()
{-# NOINLINE globalLock #-}
globalLock = unsafePerformIO $ newMVar ()

--- START: SIGNALS ---

-- |
-- `Signal` is the portable Signal they can be exchanged between
-- any parts of your program. Internally, they are just a variable and a list of 
-- change hooks.
data Signal s a = Signal {
	signalValue :: IORef a,
	signalListeners :: List.LinkedList (Weak (Signal s a ,a -> Updater s ()))
	}

newSignal :: a -> IO (Signal s a)
newSignal a = do
	value <- newIORef a
	listeners <- List.empty
	return (Signal value listeners)

readSignal :: Signal s a -> IO a
readSignal signal = readIORef $ signalValue signal

-- |
-- Writes the value to the variable inside the signal
-- and schedules the listeners to run.
-- The listeners will run in the same stm action
-- and with the value you gave.
-- However, they do not run immediately.
-- So you are guaranteed that writeSignal will
-- not have any immediate sideffects other then
-- writing the one single variable.
writeSignal :: Signal s a -> a -> Updater s ()
writeSignal (Signal valueVar listeners) value = do
	liftIO $ writeIORef valueVar value
	(liftIO $ List.start listeners) >>= recursion where
		recursion Nothing = return ()
		recursion (Just node) = do
			next <- liftIO $ List.next node
			res <- liftIO $ deRefWeak $ List.value node
			case res of
				 (Just (_,listener)) -> listener value
				 _ -> return ()
			recursion next

-- |
-- The return value will remove the listener.
-- IMPORTANT: If the remover gets garbage
-- collected the listener will be removed.
-- any references from the listener to the
-- remover don't count.
addListener :: Signal s a -> (a -> Updater s ()) -> IO (IO ())
addListener signal listener = do
	weakRef <- newIORef (error "should not be readable")
	node <- List.append (unsafePerformIO $ readIORef weakRef) (signalListeners signal)
	let !remove = List.delete node
	weak <- mkWeak remove (signal, listener) $ Just $ do
		print "cleaning up signal"
		remove
	writeIORef weakRef weak
	return remove

--- END: SIGNALS ---

data DownState = DownState {
	}

data UpState s = UpState {
	stateOnCleanup :: IO (),
	stateOnDone :: [Updater s ()]
	}

instance Monoid (UpState s) where
	mempty = UpState (return ()) []
	(UpState c1 d1) `mappend` (UpState c2 d2) = UpState (c1 >> c2) (d1 ++ d2)

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
newtype Updater s a = Updater { 
	runUpdater' :: (a -> DownState -> IO (UpState s)) -> DownState -> IO (UpState s)
	}

data RealWorld

type UpdaterIO = Updater RealWorld

-- -- |
-- -- Runs everything below it everytime its input signal is updated. 
-- getSignalEvent :: Signal a -> Updater a
-- getSignalEvent signal =  Updater $ \restCalc state->  do
-- 	upStateVar <- newIORef (return ())
-- 	removeListener <- addListener signal
-- 		(\value -> do
-- 			c <- stateOnCleanup <$> readIORef upStateVar
-- 			c
-- 			upState <- restCalc value state >>= writeIORef upStateVar
-- 		)
-- 	return (do
-- 		removeListener
-- 		join $ readIORef cleanupVar
-- 		)
-- 

--returnEndonCommitUpdater :: Updater ()  -> Updater ()
-- onCommitUpdater action = do
-- 	state <- getState
-- 	liftSTM $ modifyIORef (stateOnCommitUpdater state) (action:)

-- returnDone :: Updater a -> Updater a
-- returnDone (Updater giveMeRest) = Updater $ \restCalc _ ->
-- 	return (UpState  {
-- 		stateOnCleanup = [],
-- 		stateOnDone = [Updater $ \_ downState2 ->
-- 			giveMeRest restCalc downState2
-- 			]
-- 		})

-- |
-- executes rest after everything else
delay :: Updater s ()
delay = Updater $ \restCalc downState -> return $ UpState {
	stateOnCleanup = return (),
	stateOnDone = [Updater $ \restCalc' downState' -> restCalc () downState']
	}

newEvent :: Updater s a -> Updater s (Updater s a)
newEvent updater = Updater $ \restCalc downState->  do
	signal <- newSignal (error "unreadable")
	upstate <- restCalc (Updater $ \restCalc2 _ -> do
		removeListener <- addListener signal
			(\value -> Updater $ \_ state3 -> do
				restCalc2 value state3
				)
		return $ mempty { stateOnCleanup = do
			removeListener
			}
		) downState

	return upstate {
		stateOnDone = stateOnDone upstate ++ [ do
			val <- updater
			writeSignal signal val
			]
		}

newEventIO :: IO (UpdaterIO a, a -> IO ())
newEventIO = undefined
	
		
{-
-- |
-- Similar to `getEvent` except that it also fires an event immediately,
-- with the value of the current state.
--
-- >getBehavior signal = liftSTM (readSignal signal) <|> getEvent signal
getSignalBehavior :: Signal a -> Updater a
getSignalBehavior signal = liftIO (readSignal signal) <|> getSignalEvent signal-}

-- newEventIO :: IO (Updater a, a -> IO ())
-- newEventIO = do
-- 	signal <- newSignal (error "unreadable")
-- 	return (getSignalEvent signal, \x -> do
-- 		takeMVar globalLock
-- 		writeSignal signal x
-- 		)

-- |
-- This will evaluate the `Updater` Monad.
-- It will block until the first run reaches the end.
-- After that, it will return the result and free everything.
-- To prevent signals from reaching the end use `Updater.stop` or `getEvent` with some exit signal.
runUpdater :: (forall s . Updater s a) -> [a]
runUpdater updater = unsafePerformIO $ do
	(res, cleanup) <- runUpdater1 updater
	cleanup
	return res

runUpdater1 :: (Updater s a) -> IO ([a], IO ())
runUpdater1 (Updater giveMeNext') = do
	-- TODO: think if cleanup is really necessary
	resVar <- newMVar []

	let execOnDones (Updater giveMeNext)  = do
		upstate <- giveMeNext (\val state ->  return mempty) DownState {}
		cleanUps <- mapM execOnDones (stateOnDone upstate)
		return (stateOnCleanup upstate >>  sequence_ cleanUps)

	upstate <- giveMeNext' (\val state -> 
			modifyMVar_ resVar (return . (val:)) >> return mempty) DownState {}

	cleanup <- mapM execOnDones $ stateOnDone upstate

	res <- takeMVar resVar
	return (reverse res, stateOnCleanup upstate >> sequence_ cleanup)


newtype UpdaterRest a b = UpdaterRest { runUpdaterRest :: a -> ([b], UpdaterRest a b) }

-- runUpdater' :: a -> (forall s . Updater s a -> Updater s b) -> ([b], UpdaterRest a b)
-- runUpdater' val updater' = unsafePerformIO $ initialize where
-- 	initialize = do
-- 		valVar <- newIORef a
-- 		resVar <- newEmptyMVar
-- 		block <- newMVar ()
-- 
-- 		forkIO $ retursion valVar resVar block
-- -- 			modifyMVar_ res (return . (val:)) >> return mempty) DownState {}
-- 		upstate <- giveMeNext' (\val state -> 
-- 
-- 	let input = Updater $ \restCalc upState -> do
		

liftIO :: IO a -> Updater s a
liftIO run = Updater (\restCalc state -> run >>= (\x -> restCalc x state))

--- START: INSTANCES ---

-- TODO: cleanup 
instance Applicative (Updater s) where
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

instance Alternative (Updater s) where
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

instance Monad (Updater s) where
	(Updater giveMeNext) >>= valueToNextUpd = Updater $ updater where
		updater end = 	giveMeNext $  \value -> runUpdater' (valueToNextUpd value) end
	return a = Updater $ \end -> end a
	fail _ = Updater $ \_ _ -> return mempty

instance Functor (Updater s) where
	fmap f (Updater giveMeNext) = Updater (\next -> giveMeNext (next . f))

--- END: INSTANCES ---