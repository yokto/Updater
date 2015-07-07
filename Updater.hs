{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Updater (
	-- * Signals
 	Signal(),
 	newSignal,
 	getValue,
-- 	addListener,
	-- * Updater Monad
	Updater(),
	runUpdater,
	getEvent,
	onCommit,
	onCleanup,
	-- * Helpers
	stop,
	getBehavior,
	local,
	liftSTM,
	putLine
	) where

import Control.Concurrent.STM
import Control.Applicative
import Updater.Internal hiding (getValue, newSignal)
import qualified Updater.Internal as Internal

import Control.Concurrent


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
-- Return Nothing if the signal is uninitialized.
getValue :: Signal a -> Updater (Maybe a)
getValue = liftSTM . Internal.getValue

-- |
-- Creates a new signal and gives you a way to update it.
-- It is important to note that because the signal and the
-- update function are separate, you can easily have readonly,
-- writeonly permissions.
newSignal :: Updater (a -> Updater (), Signal a)
newSignal = liftSTM Internal.newSignal