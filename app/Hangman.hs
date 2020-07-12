-- Based on https://en.wikibooks.org/wiki/Haskell/Arrow_tutorial

{-# LANGUAGE Arrows #-}
{- HLINT ignore "Avoid lambda" -}

module Main where

import Control.Arrow
import Data.Maybe
import System.Random

import Circuit

attempts :: Int
attempts = 5

livesLeft :: Int -> String
livesLeft hung = "Lives: [" ++ replicate (attempts - hung) '#' ++ replicate hung ' ' ++ "]"

hangman :: StdGen -> Circuit String (Bool, [String])
hangman rng = proc userInput -> do
  word    <- getWord rng -< ()
  let letter = listToMaybe userInput
  guessed <- updateGuess -< (word, letter)
  hung    <- updateHung  -< (word, letter)
  end     <- delayedEcho True -< not (word == guessed || hung >= attempts)
  let result
        | word == guessed  = [guessed, "You won!"]
        | hung >= attempts = [guessed, livesLeft hung, "You died!"]
        | otherwise        = [guessed, livesLeft hung]
  returnA -< (end, result)
  where

    updateGuess :: Circuit (String, Maybe Char) String
    updateGuess = accumV (repeat '_') $ \ (word, letter) guess ->
      case letter of
        Just l  -> zipWith (\ w g -> if w == l then w else g) word guess
        Nothing -> take (length word) guess

    updateHung :: Circuit (String, Maybe Char) Int
    updateHung = proc (word, letter) -> do
      total -< case letter of
        Just l  -> if l `elem` word then 0 else 1
        Nothing -> 0


main :: IO ()
main = do
  rng <- getStdGen
  interact
    $ unlines                        -- Concatenate lines out output
    . ("Welcome to Arrow Hangman":)  -- Prepend a greeting to the output
    . concatMap snd . takeWhile fst  -- Take the [String]s as long as the first element of the tuples is True
    . runCircuit (hangman rng)       -- Process the input lazily
    . ("":)                          -- Act as if the user pressed ENTER once at the start
    . lines                          -- Split input into lines
