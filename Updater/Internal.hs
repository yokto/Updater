module Updater.Internal (
 	-- Signals
 	Signal(),
 	newSignal,
 	getValue,
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

--- START: Signals ---

-- |
-- `Signal` is the portable Signal they can be exchanged between
-- any parts of your program. Internally, they are just a variable and a list of 
-- change hooks.
data Signal a = Signal {
	signalValue :: TVar (Maybe a),
	signalListeners :: List.LinkedList (a -> Updater ())
	}

newSignal :: STM (a -> Updater (), Signal a)
newSignal = do
	value <- newTVar (Nothing)
	listeners <- List.empty
	let runSignal a = do
		liftSTM $ writeTVar value $ Just a
		listeners' <- liftSTM $ List.toList listeners
		mapM_ ($ a) listeners'
	return (runSignal,Signal value  listeners)

getValue :: Signal a -> STM (Maybe a)
getValue signal = readTVar $ signalValue signal

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
--- END: Signals ---

data State = State {
	stateOnCommit :: TVar (IO ()),
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
-- doesn't really work yet
onCleanup :: Updater () -> Updater ()
onCleanup cleanup = do
	getCleanup >>= getEvent
	cleanup

-- |
-- IO actions given here will be executed once a signal update
-- has been completed. They keep the order in which they are inserted.
onCommit :: IO () -> Updater ()
onCommit action = do
	state <- getState
	liftSTM $ modifyTVar (stateOnCommit state) (>>action)

getState :: Updater State
getState = Updater $ \restCalc state -> restCalc state state

-- |
-- Runs everything below it everytime its input signal is updated. 
getEvent :: Signal a -> Updater a
getEvent signal =  Updater $ \restCalc state->  do 
--	cleanupHelper <- newCleanupHelper (stateCleanup state)
	let listener value = do
		(cleanupButton, cleanupSignal) <- liftSTM $newSignal
		liftSTM $ addSingletonListener signal (\value -> cleanupButton ())
		state' <-getState
		liftSTM $ restCalc value (state' { stateCleanup =  cleanupSignal })
		return ()

	removeListener <- addListener signal listener
	_ <- addSingletonListener (stateCleanup state) (const $ liftSTM removeListener)
	return ()

-- |
-- Similar to `getEvent` except that it also fires an event immediately,
-- if the input signal is already initialized. It can be created using `getEvent` and
-- `Alternative`
getBehavior :: Signal a -> Updater a
getBehavior sig = initial <|> getEvent sig where
	initial = do
		val' <- liftSTM $ getValue sig
		case val' of
			 Nothing -> empty
			 (Just val) -> return val

-- |
-- This will evaluate the `Updater` Monad.
-- It will block until the first run reaches the end.
-- After that, it will return the result and free everything.
-- To prevent signals from reaching the end use `Updater.stop` or `getEvent` with some exit signal.
runUpdater :: Updater a -> IO a
runUpdater updater' = wrapper where
	wrapper = do
		(cleanupButton, cleanupSignal) <- atomically $ newSignal
		onException
			(run updater' cleanupButton cleanupSignal)
			(run (cleanupButton ())  cleanupButton cleanupSignal)
		
	run updater cleanupButton cleanupSignal= do
		(resultVar, onCommitAction) <- atomically $ do
			onCommit' <- newTVar $ return ()
			resultVar <- newEmptyTMVar
			runUpdater'
				( do
					res <- updater
					cleanupButton ()
					onCommit $ atomically $ putTMVar resultVar res)
				(const $ const $ return ()) 
				(State {
					stateCleanup = cleanupSignal,
					stateOnCommit = onCommit' })
			onCommitAction <- readTVar onCommit'
			return (resultVar, onCommitAction)
		onCommitAction
		result <- atomically $ takeTMVar resultVar
		return result

liftSTM :: STM a -> Updater a
liftSTM run = Updater (\restCalc state -> run >>= (\x -> restCalc x state))

instance Functor Updater where
	fmap f (Updater giveMeNext) = Updater (\next -> giveMeNext (next . f))

instance Applicative Updater where
	pure a = Updater $ \giveMeA -> giveMeA a
 	updater1 <*> updater2 = Updater $ updater where
 		updater restCalc state = do
 			(buttonF, signalF) <- newSignal
 			(buttonX, signalX) <- newSignal

			runUpdater' (do
				f <- getBehavior signalF
				x <- getBehavior signalX
				state' <- getState
				liftSTM $ restCalc (f x) state'
				) (const $ const $ return ()) state

 			runUpdater' (updater1 >>= buttonF) (const $ const $ return ()) state
 			runUpdater' (updater2 >>= buttonX) (const $ const $ return ()) state
 			return ()

instance Alternative Updater where
	empty = Updater $ \_ _ -> return ()
	updater1 <|> updater2 = Updater $ updater where
		updater restCalc state = do
			(button,signal) <-newSignal

			runUpdater' (do
				event <- getEvent signal
				state' <- getState
				liftSTM $ restCalc event state'
				) (const $ const $ return ()) state

			runUpdater' (updater1 >>= button) (const $ const $ return ()) state
			runUpdater' (updater2 >>= button) (const $ const $ return ()) state
			return ()

instance Monad Updater where
	(Updater giveMeNext) >>= valueToNextUpd = Updater $ updater where
		updater end = 	giveMeNext $  \value -> runUpdater' (valueToNextUpd value) end
	return a = Updater $ \end -> end a