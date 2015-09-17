{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Updater (
	Event (),
	Behavior (),
	newEvent,
	cacheStateful,
	cacheStateless,
	sample,
	foldEvent,
		runEvent,
	runGlobalEvent,
	debug,
	debugCleanup,
	hold,
	unsafeLiftIO
	) where

import Control.Concurrent
import Control.Applicative
--import Control.Concurrent.MVar
--import Data.Monoid
import Control.Monad
import Data.Monoid
import Control.Monad.Fix
import Updater.Internal
import System.IO.Unsafe
import Debug.Trace
import Foreign.StablePtr

instance Monoid (Event a) where
	mempty = empty
	mappend = (<|>)

newEvent :: IO (Event a, a -> IO ())
newEvent = do
	(ev,button) <- newEvent'
	return (Event ev, button)

-- | The input will only be evaluated once,
-- no matter how often the output 'Event' is used.
-- Since it is stateless, when the output 'Event' is used, it will first
-- have to wait for events.
cacheStateless :: Event a -> Behavior (Event a)
cacheStateless (Event u) = Behavior (Event `fmap` cacheStateless' u)

-- | The input will only be evaluated once,
-- no matter how often the ouput 'Event' is used.
-- Since it is stateful, when the output 'Event' is used, it will
-- immeduately continue with the last Event it received if
-- such an event exists.
cacheStateful :: Event a -> Behavior (Event a)
cacheStateful (Event d) = Behavior (Event `fmap` cacheStateful' d)

-- | This can be thought of as polling a behavior. It will only fire once.
sample :: Behavior a -> Event a
sample (Behavior c) = Event c

-- | This just only forwards the first event
-- It is probably most useful for Events crated using
-- 'cacheStateful'
hold :: Event a -> Behavior a
hold (Event e) = Behavior (justOne e)

-- | 'Left io' events will be executed.
-- The first 'Right res' event will end the function and return res.
runEvent :: Event (Either (IO ()) res) -> IO res
runEvent (Event u) = runUpdater u

-- | 
-- This can be implemented using mfix, cacheStateful, ...
-- 
-- If you get into trouble and really need multiple recursively defined
-- Events you can use mfix to do that.
-- You should however look at the implementation of 'foldEvent' and
-- the SlotMachine example first.
-- In particular, make sure you understande that you need to use
-- 'sample . hold' on the recursive signal in order to avoid infinite recursion.
foldEvent :: (b -> a -> b) -> b -> Event a -> Event b
foldEvent f b updater = join $ sample $ mfix $ \discrete -> cacheStateful $ return b <|> (do
	a' <- updater
	b' <- sample $ hold discrete
	return (f b' a'))

-- |
-- This is just a convenience for use in ghci
-- and in the test cases. It will just run
-- the Event it is given in it's own thread.
runGlobalEvent :: Event (IO ()) -> IO ()
{-# NOINLINE runGlobalEvent #-}
runGlobalEvent = unsafePerformIO $ do
	_ <- newStablePtr runGlobalEvent
	(ev, button) <- newEvent :: IO (Event (Event (IO ())), Event (IO ()) -> IO ())
	var <- newEmptyMVar
	forkIO $ (runEvent $ sample (onCommit (putMVar var ())) >> Left `fmap` join ev)
	takeMVar var
	return button