module KlondikeSpec where

import ClassyPrelude
import Runner (winnable)
import Test.Hspec (Spec, describe, it, shouldSatisfy)
import SpecHelpers (compileSolitaireDeck, shouldHaveAll52Cards)
import Klondike
import Types (SinglePlayer(..), Card(Card), Suit(..), king, queen)

spec :: Spec
spec = do
  describe "Klondike Moves" $ do
    it "recognizes a won game" $ do
      shouldHaveAll52Cards . compileSolitaireDeck $ wonGame
      wonGame `shouldSatisfy` gameIsWon

    it "can win a nearly won game" $ do
      shouldHaveAll52Cards . compileSolitaireDeck $ nearlyWonGame
      nearlyWonGame `shouldSatisfy` winnable

    {- it "can move stacks" $ do
      pure ()

    it "can play up to stacks" $ do
      pure () -}



wonGame :: SolitaireGame
wonGame = SolitaireGame king king king king [] [] ([],[]) ([],[]) ([],[]) ([],[]) ([],[]) ([],[]) ([],[])

nearlyWonGame :: SolitaireGame
nearlyWonGame = SolitaireGame
  queen queen queen queen [Card SuitHearts king] [Card SuitClubs king]
  ([],[]) ([Card SuitDiamonds king],[Card SuitSpades king]) ([],[]) ([],[]) ([],[]) ([],[]) ([],[])
