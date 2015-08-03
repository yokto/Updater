module Updater.Internal (
 	-- Signals
 	Signal(),
 	newSignal,
	newSignalIO,
 	writeSignal,
	readSignal,
 	addListener,
	-- Updater
	Updater(),
	onCommit,
	getEvent,
	getBehavior,
	runUpdater,
--	getCleanup,
	liftSTM,
	onCleanup
	) where

import Control.Concurrent.STM
import qualified Updater.List as List

import Control.Applicative
import Control.Exception.Base
import Control.Monad.Fix

putLine :: String -> Updater ()
putLine = onCommit . putStrLn

--- START: SIGNALS ---

-- |
-- `Signal` is the portable Signal they can be exchanged between
-- any parts of your program. Internally, they are just a variable and a list of 
-- change hooks.
data Signal a = Signal {
	signalValue :: TVar a,
	signalListeners :: List.LinkedList (a -> Updater ())
	}

newSignal :: a -> STM (Signal a)
newSignal a = do
	value <- newTVar a
	listeners <- List.empty
	return (Signal value listeners)

newSignalIO :: a -> IO (Signal a)
newSignalIO a = do
	value <- newTVarIO a
	listeners <- List.emptyIO
	return (Signal value listeners)
	


readSignal :: Signal a -> STM a
readSignal signal = readTVar $ signalValue signal

-- |
-- Writes the value to the variable inside the signal
-- and schedules the listeners to run.
-- The listeners will run in the same stm action
-- and with the value you gave.
-- However, they do not run immediately.
-- So you are guaranteed that writeSignal will
-- not have any immediate sideffects other then
-- writing the one single variable.
writeSignal :: Signal a -> a -> Updater ()
writeSignal (Signal valueVar listeners) value = do
	liftSTM $ writeTVar valueVar value
	onCommitUpdater $ liftSTM (List.start listeners) >>= recursion where
		recursion Nothing = return ()
		recursion (Just node) = do
			List.value node value :: Updater ()
			liftSTM (List.next node) >>= recursion

-- |
-- executes listeners immediately.
-- can lead to breaking of semanitcs if not used carefully
writeSignalNow :: Signal a -> a -> Updater ()
writeSignalNow (Signal valueVar listeners) value = do
	listeners' <- liftSTM $ List.toList listeners
	liftSTM $ writeTVar valueVar value
	mapM_ ($ value) listeners'

-- |
-- the return value will remove the listener
-- use
-- 'fixm \remover -> someListener remover'
-- to add a listener that can remove itself
addListener :: Signal a -> (a -> Updater ()) -> STM (STM ())
addListener signal listener = do
	node <- List.append listener (signalListeners signal)
	return (List.delete node)

addSingletonListener :: Signal a -> (a -> Updater ()) -> STM (STM ())
addSingletonListener signal listener = mfix add where
	add remove = addListener signal (run remove)
	run remove value = liftSTM remove >> listener value

--- END: SIGNALS ---

data State = State {
	stateOnCommitUpdater :: TVar ([Updater ()]),
	stateOnCommitIO :: TVar ([IO ()]),
	stateCleanup :: Signal ()
}

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
newtype Updater a = Updater { runUpdater' :: (a -> State -> STM ()) -> State -> STM () }

getCleanup :: Updater (Signal ())
getCleanup = fmap stateCleanup getState

-- |
-- is executed right before getEvent ... fire the next event
onCleanup :: Updater () -> Updater ()
onCleanup cleanup = do
	cleanupE <- getCleanup
	liftSTM $ addSingletonListener cleanupE (const $ cleanup)
	return ()

-- |
-- IO actions given here will be executed once a signal update
-- has been completed. They keep the order in which they are inserted.
onCommit :: IO () -> Updater ()
onCommit action = do
	state <- getState
	liftSTM $ modifyTVar (stateOnCommitIO state) (action:)

onCommitUpdater :: Updater () -> Updater ()
onCommitUpdater action = do
	state <- getState
	liftSTM $ modifyTVar (stateOnCommitUpdater state) (action:)

getState :: Updater State
getState = Updater $ \restCalc state -> restCalc state state

putState :: State -> Updater ()
putState state = Updater $ \restCalc _ -> restCalc () state

-- |
-- Runs everything below it everytime its input signal is updated. 
getEvent :: Signal a -> Updater a
getEvent signal =  Updater $ \restCalc state->  do
	cleanupE <- newSignal ()
	removeListener <- addListener signal
		(\value -> do
			writeSignalNow cleanupE ()
			state' <- getState
			liftSTM $ restCalc value (state' { stateCleanup = cleanupE })
		)
	addSingletonListener (stateCleanup state) (const $ do
		liftSTM removeListener
		writeSignalNow cleanupE ()
		)
	return ()

-- |
-- Similar to `getEvent` except that it also fires an event immediately,
-- with the value of the current state.
--
-- >getBehavior signal = liftSTM (readSignal signal) <|> getEvent signal
getBehavior :: Signal a -> Updater a
getBehavior signal = liftSTM (readSignal signal) <|> getEvent signal
	

-- |
-- This will evaluate the `Updater` Monad.
-- It will block until the first run reaches the end.
-- After that, it will return the result and free everything.
-- To prevent signals from reaching the end use `Updater.stop` or `getEvent` with some exit signal.
runUpdater :: Updater a -> IO a
runUpdater updater' = wrapper where
	wrapper = do
		cleanupSignal <- atomically $ newSignal $ error "should not be accessible"
		onException
			(run updater' cleanupSignal)
			(run (writeSignalNow cleanupSignal ()) cleanupSignal)
		
	run updater cleanupSignal= do
		(resultVar, onCommitAction) <- atomically $ do
			onCommitVar <- newTVar []
			onCommitUpdaterVar <- newTVar []
			resultVar <- newEmptyTMVar
			runUpdater'
				( do
					res <- updater
					writeSignalNow cleanupSignal ()
					onCommit $ atomically $ putTMVar resultVar res)
				(const $ const $ return ()) 
				(State {
					stateCleanup = cleanupSignal,
					stateOnCommitUpdater = onCommitUpdaterVar,
					stateOnCommitIO = onCommitVar })
			let runOnCommitUpdater onCommitUpdaterVal = do
				onCommitUs <- newTVar []
				runUpdater' (onCommitUpdaterVal) (const $ const $ return ())  (State
					{ stateCleanup = error "should not be needed"
					, stateOnCommitUpdater = onCommitUs
					, stateOnCommitIO = onCommitVar
					})
				onCommitUs' <- readTVar onCommitUs
				mapM_ runOnCommitUpdater onCommitUs'
			readTVar onCommitUpdaterVar >>= mapM_ runOnCommitUpdater
			onCommitAction <- readTVar onCommitVar
			return (resultVar, onCommitAction)
		sequence_ $ reverse onCommitAction
		result <- atomically $ takeTMVar resultVar
		return result

liftSTM :: STM a -> Updater a
liftSTM run = Updater (\restCalc state -> run >>= (\x -> restCalc x state))

--- START: INSTANCES ---

instance Functor Updater where
	fmap f (Updater giveMeNext) = Updater (\next -> giveMeNext (next . f))

instance Applicative Updater where
	pure a = Updater $ \giveMeA -> giveMeA a
 	updater1 <*> updater2 = Updater $ updater where
 		updater restCalc state = do
 			signalF <- newSignal Nothing
 			signalX <- newSignal Nothing

 			runUpdater' (updater1 >>= writeSignalNow signalF . Just) (const $ const $ return ()) state
 			runUpdater' (updater2 >>= writeSignalNow signalX . Just) (const $ const $ return ()) state

			runUpdater' (do
				(Just f) <- getBehavior signalF
				(Just x) <- getBehavior signalX
				state' <- getState
				liftSTM $ restCalc (f x) state'
				) (const $ const $ return ()) state

 			return ()

instance Alternative Updater where
	empty = Updater $ \_ _ -> return ()
	updater1 <|> updater2 =Updater $ \restCalc state -> do
		signal <-newSignal (error "should not be accessed")
		cleanupSignal <- newSignal (error "should not be accessed")

		runUpdater' (do
			-- we don't want the next line to get cleaned up before
			-- both updates have had a chance to fire the initial signal
			event <- getEvent signal
			state' <- getState
			liftSTM $ restCalc event state'
			) (const $ const $ return ()) state

		runUpdater' (updater1 >>= writeSignalNow signal) (const $ const $ return ()) state
		runUpdater' (updater2 >>= writeSignalNow signal) (const $ const $ return ()) state
			
		addSingletonListener (stateCleanup state) (writeSignalNow cleanupSignal)
		return ()

instance Monad Updater where
	(Updater giveMeNext) >>= valueToNextUpd = Updater $ updater where
		updater end = 	giveMeNext $  \value -> runUpdater' (valueToNextUpd value) end
	return a = Updater $ \end -> end a
	fail _ = Updater $ \_ _ -> return ()

--- END: INSTANCES ---