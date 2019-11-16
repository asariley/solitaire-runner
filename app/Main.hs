module Main where

import ClassyPrelude

import Types (SinglePlayer(..), sortedDeck)
import Data.Time.Clock (diffUTCTime)
import qualified Klondike
import Runner (winnable, winnableIO)
import qualified War
import Test.QuickCheck (shuffle, generate)

main :: IO ()
main = do
  decks <- generate . sequence . take 10 . repeat $ shuffle sortedDeck
  putStrLn $ "generated " <> tshow (length decks) <> " decks"
  results <- forM decks $ \ deck -> do
    begin <- getCurrentTime
    let result = klondikeResult . newGame $ deck
    putStrLn $ bool "LOSS" "WIN" result
    end <- getCurrentTime
    putStrLn $ "    in " <> tshow (end `diffUTCTime` begin) <> " sec"
    pure result
  putStrLn $ "WINS: " <> tshow (length $ filter id results)
  putStrLn $ "LOSSES: " <> tshow (length $ filter not results)

warResult :: War.War -> Bool
warResult = winnable

klondikeResult :: Klondike.SolitaireGame -> Bool
klondikeResult = winnable

klondikeResultLogged :: Klondike.SolitaireGame -> IO Bool
klondikeResultLogged = winnableIO
