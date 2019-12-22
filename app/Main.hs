module Main where

import ClassyPrelude

import Types (Deck, SinglePlayer(..), sortedDeck, exampleDeck)
import Data.Time.Clock (diffUTCTime)
import qualified Klondike
import Runner (winnable, winnableIO)
import qualified War
import Test.QuickCheck (shuffle, generate)

main :: IO ()
main = do
  decks <- profilingDeck
  putStrLn $ "trying " <> tshow (length decks) <> " decks"
  results <- forM decks $ \ deck -> do
    begin <- getCurrentTime
    -- putStrLn $ tshow deck
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

tenRandomDecks :: IO [Deck]
tenRandomDecks = generate . sequence . take 10 . repeat $ shuffle sortedDeck

profilingDeck :: IO [Deck]
profilingDeck = pure [exampleDeck]
