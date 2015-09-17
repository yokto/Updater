{-# LANGUAGE RecursiveDo #-}
-- reimplementation from reactive-banana example
-- by HeinrichApfelmus
module Main where

import Updater

import System.Random
import System.IO
import Control.Concurrent
import Control.Applicative
import Data.Maybe
import Data.IORef
import Control.Monad
import Data.List (nub,intercalate)
import Foreign.StablePtr


helpMsg :: String
helpMsg = intercalate "\n" $
    "-----------------------------":
    "- THE REACTIVE SLOT MACHINE -":
    "------ WIN AN UPDATER ---------":
    "":
    "Commands are:":
    "   coin    - insert a coin":
    "   play    - play one game":
    "   quit    - quit the program":
    "":
    []

bandit :: Behavior Int -> Event String -> Event (Either (IO ()) ())
bandit random line = do
	let
		coin = do { "coin" <- line; return ()}
		tryPlay = do { "play" <- line; return ()}
		quit = do { "quit" <- line; return ()}
		help = do { l <- line; when (elem l ["coin","play","quit"]) empty}

	msg <- sample $ mdo
		noPlay <- cacheStateless $ do
			tryPlay
			0 <- sample $ hold credit
			return ()
		play <- cacheStateless $ do
			tryPlay
			c <- sample $ hold credit
			when (c == 0) empty
		reels <- cacheStateless $ do
			play
			replicateM 3 $ sample random
		let win = (<$>reels) $ \r ->  case (length $ nub r) of
			1 -> 20
			2 -> 3
			3 -> 0
 		credit <- cacheStateful $
 			return 0
 			<|> (do
 				coin
 				c <- sample $ hold credit
 				return (c + 1))
			<|> do
				play
				c <- sample $ hold credit
				return (c-1)
			<|> do
				w <- win
				c <- sample $ hold credit
				return $ w + c
		cacheStateless $
			return "Welcome to the One Armed Bandit"
			<|> (\c -> "Credit: " ++ show c) <$> credit
			<|> (\r -> "Reels: " ++ show r) <$> reels
			<|> (\w -> "You win: " ++ show w) <$> win
			<|> ("We are sorry to see you go" <$ quit)
			<|> (helpMsg <$ help)
			<|> ("Not enough credit" <$ noPlay)
	(Left . putStrLn <$>msg) <|> (Right () <$ quit)

main :: IO ()
main = do
	(line, lineTrigger) <- newEvent
	randomRef <- newStdGen >>= newIORef
	let randomBehavior = unsafeLiftIO $ do
		rng <- readIORef randomRef
		let (random, rng') = randomR (1,9) rng
		writeIORef randomRef rng'
		return random

	let loop = do
		putStr "> "
		hFlush stdout
		line <- getLine
		lineTrigger line
		loop

	let run = runEvent (bandit randomBehavior line)

	threadId <- forkIO loop

	myThreadId >>= newStablePtr
	run >> killThread threadId