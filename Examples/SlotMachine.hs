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
	(lineButton, lineSignal) <- newSignal
	(creditButton, creditSignal) <- newSignal
	(coinButton, coinSignal) <- newSignal
	(playButton, playSignal) <- newSignal
	(quitButton, quitSignal) <- newSignal
	(randomButton, randomSignal) <- newSignal

	-- initialize credit and random number generator
	creditButton 0
	
	let
		getRandomInt1to4 :: Updater Int
		getRandomInt1to4 = do
			rng' <- getValue randomSignal
			let ( ret, nextRng) = randomR (1,9) $ fromMaybe rng rng'
			randomButton nextRng
			return ret

	-- parse the input
	local $ do
		line <- getEvent lineSignal
		case line of
			 --executing ****Button () will just trigger an event in the corresponting signal
			 "coin" -> coinButton ()
			 "c" -> coinButton()
			 "play" -> playButton ()
			 "p" -> playButton ()
			 "quit" -> quitButton ()
			 "q" -> quitButton ()
			 _ -> onCommit help

	-- coin event
	local $ do
		-- important: use getBehaviour that we will get the initial value
		credit <- getBehavior creditSignal
		-- here we use getEvent because the coin is an event and there is no initial value
		getEvent coinSignal
		-- the execution will only come here when coinSignal is fired because we used getEvent
		-- there fore the press of creditButton will not create a loop
		creditButton (credit + 1)

	-- credit change
	local $ do
		credit <- getEvent creditSignal
		onCommit $ putStrLn $ "credit " ++ show credit

	-- play
	local $ do
		credit <- getBehavior creditSignal
		getEvent playSignal
		when (credit <= 0) (putLine "Not enough credit" >> stop)
		creditButton (credit - 1)
		
		numbers <- replicateM 3 getRandomInt1to4
		putLine "Your Numbers are:"
		putLine $ show numbers

		case (length $ nub numbers) of
			 1 -> putLine "*** Triple Win ***\nYou get 20 coins" >> creditButton (credit + 20)
			 2 -> putLine "** Double Win **\nYou get 3 coins" >> creditButton (credit + 3)
			 _ -> putLine "You lose. Better luck next time"
		return ()


	let loop = do
		putStr "> "
		hFlush stdout
		line <- getLine
		quit <- runUpdater $ do
			lineButton line -- TODODODODO
			getValue quitSignal
		when (isNothing quit) loop


	putLine "Welcome to the One Armed Bandit"
	onCommit (forkIO loop >> return ())
	-- onCleanup doesn't currently work
	-- onCleanup $  quitButton ()
	
	getBehavior quitSignal
	putLine "We are sorry to see you go"
	return ()

test = putStrLn "hello" >> hFlush stdout