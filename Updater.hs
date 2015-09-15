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

cacheStateless :: Event a -> Behavior (Event a)
cacheStateless (Event u) = Behavior (Event `fmap` cacheStateless' u)

cacheStateful :: Event a -> Behavior (Event a)
cacheStateful (Event d) = Behavior (Event `fmap` cacheStateful' d)

sample :: Behavior a -> Event a
sample (Behavior c) = Event c

hold :: Event a -> Behavior a
hold (Event e) = Behavior (justOne e)

runEvent :: Event (Either (IO ()) res) -> IO res
runEvent (Event u) = runUpdater u

-- | 
-- This can be implemented using mfix, cacheStateful, ...
-- 
-- if you get into trouble and really need multiple recursively defined
-- you can use mfix to do that.
foldEvent :: (b -> a -> b) -> b -> Event a -> Event b
foldEvent f b updater = join $ sample $ mfix $ \discrete -> cacheStateful $ return b <|> (do
	a' <- updater
	b' <- sample $ hold discrete
	return (f b' a'))

-- |
-- this is just a convenience for use in ghci
-- and in the test cases. It will just run
-- the updater it is given in it's own thread.
runGlobalEvent :: Event (IO ()) -> IO ()
{-# NOINLINE runGlobalEvent #-}
runGlobalEvent = unsafePerformIO $ do
	_ <- newStablePtr runGlobalEvent
	(ev, button) <- newEvent :: IO (Event (Event (IO ())), Event (IO ()) -> IO ())
	var <- newEmptyMVar
	forkIO $ (runEvent $ sample (onCommit (putMVar var ())) >> Left `fmap` join ev)
	takeMVar var
	return button