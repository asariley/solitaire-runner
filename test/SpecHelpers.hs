module SpecHelpers where

import ClassyPrelude
import Test.Hspec (shouldMatchList)
import Test.QuickCheck (Gen, shuffle)
import Klondike
import War
import Types (SinglePlayer(..), Card(Card), CardNumber(CardNumber), Deck, Suit(..), sortedDeck)

shuffledDeckGen :: Gen Deck
shuffledDeckGen = shuffle sortedDeck

genSinglePlayer :: SinglePlayer game => Gen game
genSinglePlayer = newGame <$> shuffledDeckGen

compileSolitaireDeck :: SolitaireGame -> Deck
compileSolitaireDeck (SolitaireGame {..})
  =  map (Card SuitHearts) [CardNumber 1 .. _gameScoreHearts]
  <> map (Card SuitSpades) [CardNumber 1 .. _gameScoreSpades]
  <> map (Card SuitDiamonds) [CardNumber 1 .. _gameScoreDiamonds]
  <> map (Card SuitClubs) [CardNumber 1 .. _gameScoreClubs]
  <> _gameFlipped <> _gameToFlip
  <> joinPair _gamePosition0 <> joinPair _gamePosition1 <> joinPair _gamePosition2
  <> joinPair _gamePosition3 <> joinPair _gamePosition4 <> joinPair _gamePosition5 <> joinPair _gamePosition6

compileWarDeck :: War -> Deck
compileWarDeck (War m o p) = m <> o <> p

joinPair :: Monoid m => (m, m) -> m
joinPair (f, s) = f <> s

shouldHaveAll52Cards :: Deck -> IO ()
shouldHaveAll52Cards deck = sortedDeck `shouldMatchList` deck
