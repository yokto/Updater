module Main where

import Graphics.Gloss.Interface.IO.Game hiding (Event)
import qualified Graphics.Gloss.Interface.IO.Game as Game

import Examples.FRPZoo.Button

import Updater
import Control.Applicative
import Control.Monad
import Data.IORef

out0 :: Event () -> Event () -> Event Int
out0 inc toggle = do
	isOn <- foldEvent (\old _ -> not old) True toggle
	if isOn
		then foldEvent (\old _ -> 1 + old) 0 inc
		else return (-1)

out5 :: Event () -> Event () -> Event Int
out5 inc toggle = do
	isOnEv <- sample $ cacheStateful $ foldEvent (\old _ -> not old) True toggle
	countEv <- sample $ cacheStateful $ foldEvent (\old _ -> 1 + old) 0 $
		do { True <- isOnEv ; inc }
	isOn <- isOnEv
	if isOn
		then countEv
		else return (-1)

out10 :: Event () -> Event () -> Event Int
out10 inc toggle = do
	countD <- sample $ cacheStateful $ foldEvent (\old _ -> 1 + old) 0 inc
	isOn <-foldEvent (\old _ -> not old) True toggle
	if isOn
		then countD
		else return (-1)

main :: IO ()
main = do
	outRef <- newIORef (0,0,0)

	-- fired when the button with the number is clicked
	(inc0, incB0) <- newEvent :: IO (Event (), () -> IO ())
	(inc5, incB5) <- newEvent
	(inc10, incB10) <- newEvent

	-- when toggle button is clicked
	(toggle0, toggleB0) <- newEvent
	(toggle5, toggleB5) <- newEvent
	(toggle10,toggleB10) <- newEvent

	runGlobalEvent $ do
		--sample $ cacheStateless $ inc0 >> sample (debug "print")
		state <- (,,) <$> (out0 inc0 toggle0) <*> (out5 inc5 toggle5) <*> (out10 inc10 toggle10)
		return $ writeIORef outRef state

	-- START BOILER PLATE
	let
		processEvent :: Game.Event -> IO ()
		processEvent e = do
			when (buttonC0 `isClickedBy` e) (incB0 ())
			when (buttonC5 `isClickedBy` e) (incB5 ())
			when (buttonC10 `isClickedBy` e) (incB10 ())
			when (buttonT0 `isClickedBy` e) (toggleB0 ())
			when (buttonT5 `isClickedBy` e) (toggleB5 ())
			when (buttonT10 `isClickedBy` e) (toggleB10 ())

	playIO (InWindow "Updater FRPZoo" (320, 240) (800, 200))
		white
		30
		()
		(\_ -> do
			(out0', out5', out10') <- readIORef outRef
			return $ renderButtons 
				out0'  Nothing
				out5'  Nothing
				out10' Nothing
			)
		(\e _ -> processEvent e)
		(\_ -> return)