-- reimplementation from reactive-banana example
-- by HeinrichApfelmus
module Main where

import Updater

import System.Random
import System.IO
import Control.Concurrent
import Data.Maybe
import Control.Monad
import Data.List (nub)


help :: IO ()
help = mapM_ putStrLn $
    "-----------------------------":
    "- THE REACTIVE SLOT MACHINE -":
    "------ WIN A BANANA ---------":
    "":
    "Commands are:":
    "   c(oin)    - insert a coin":
    "   p(lay)    - play one game":
    "   q(uit)    - quit the program":
    "":
    []

-- State of the reels, consisting of three numbers from 1-4. Example: "222"
type Reels = (Int,Int,Int)
-- A win consist of either double or triple numbers
data Win = Double | Triple

main :: IO ()
main = do
	rng <- newStdGen
	runUpdater (bandit rng)
	return ()

bandit :: StdGen -> Updater ()
bandit rng = do	

	-- make all the signals
	lineE <- newSignal ""
	creditB <- newSignal 0
	coinE <- newSignal ()
	playE <- newSignal ()
	quitB <- newSignal False
	rngB <- newSignal rng
	
	let
		getRandomInt :: Updater Int
		getRandomInt = do
			rng <- readSignal rngB
			let ( ret, nextRng) = randomR (1,9) $ rng
			writeSignal rngB nextRng
			return ret

	-- parse the input
	local $ do
		line <- getEvent lineE
		case line of
			 -- will just trigger an event in the corresponting signal
			 "coin" -> writeSignal coinE ()
			 "c" -> writeSignal coinE ()
			 "play" -> writeSignal playE ()
			 "p" -> writeSignal playE ()
			 "quit" -> writeSignal quitB True
			 "q" -> writeSignal quitB True
			 _ -> onCommit help

	-- coin event
	local $ do
		getEvent coinE
		modifySignal creditB (+ 1)

	-- credit change
	local $ do
		credit <- getEvent creditB
		onCommit $ putStrLn $ "credit " ++ show credit

	-- play
	local $ do
		getEvent playE
		credit <- readSignal creditB
		when (credit <= 0) (putLine "Not enough credit" >> stop)

		numbers <- replicateM 3 getRandomInt
		putLine "Your Numbers are:"
		putLine $ show numbers

		gain <- case (length $ nub numbers) of
			 1 -> putLine "*** Triple Win ***\nYou get 20 coins" >> return 20
			 2 -> putLine "** Double Win **\nYou get 3 coins" >> return 3
			 _ -> putLine "You lose. Better luck next time" >> return (-1)

		modifySignal creditB (+ gain)
		return ()


	let loop = do
		putStr "> "
		hFlush stdout
		line <- getLine
		quit <- runUpdater $ do
			writeSignal lineE line -- TODO
			readSignal quitB
		when (not quit) loop


	putLine "Welcome to the One Armed Bandit"
	onCommit (forkIO loop >> return ())
	-- onCleanup doesn't currently work
	-- onCleanup $  quitButton ()
	
	-- if False, monadic fail will be executed
	-- which is the same as stop
	True <- getBehavior quitB
	putLine "We are sorry to see you go"
	return ()

test = putStrLn "hello" >> hFlush stdout