{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Updater (
	-- * Signals
 	Signal(),
 	newSignal,
	newSignalIO,
 	writeSignal,
	readSignal,
-- 	addListener,
	-- * Updater Monad
	Updater(),
	runUpdater,
	getEvent,
	onCommit,
	onCleanup,
	-- * Helpers
	stop,
	modifySignal,
	getBehavior,
	local,
	liftSTM,
	putLine,
	runGlobalUpdater
	) where

import Control.Concurrent
import Control.Applicative
import Updater.Internal hiding (newSignal, readSignal)
import qualified Updater.Internal as Internal
import System.IO.Unsafe

-- |
-- Creates a new signal. You can use this signal in any
-- context you want and share it freely between any
-- number of different Updater monads.
newSignal :: a -> Updater (Signal a)
newSignal = liftSTM . Internal.newSignal

-- |
-- Just a synonym for `empty` from `Alternative`.
-- It basically prevents signals from ever progressing beyond this point.
-- You can use this to make a filter for instance
--
-- >when (condition) stop
stop :: Updater a
stop = empty

-- |
-- Just for some quick debugging
--
-- >putLine = onCommit . putStrLn
putLine :: String -> Updater ()
putLine = onCommit . putStrLn

-- |
-- Returns immediately after registering the given computation.
-- However, events from inside will not spread outside, except for
-- the initial one.
--
-- It is implemented like this
--
-- >local computation = return () <|> (computation >> stop)
local :: Updater a -> Updater ()
local computation = return () <|> (computation >> stop)

-- |
-- Gets the current value.
readSignal :: Signal a -> Updater a
readSignal = liftSTM . Internal.readSignal

-- |
-- simple combination of readSignal and writeSignal
modifySignal :: Signal a -> (a -> a) -> Updater ()
modifySignal s f = readSignal s >>= writeSignal s . f

-- |
-- this is just a convenience for use in ghci
-- and in the test cases. It will just run
-- the updater it is given in it's own thread.
runGlobalUpdater :: Updater a -> IO ()
runGlobalUpdater u = runUpdater $ writeSignal globalUpdater (u >> return ())

globalUpdater :: Signal (Updater ())
{-# NOINLINE globalUpdater #-}
globalUpdater = unsafePerformIO $ do
	s <- newSignalIO $ return ()
	forkIO $ runUpdater $ do
		currentUpdater <-getBehavior s
		currentUpdater
		stop
	return s
	