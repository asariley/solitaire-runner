module SinglePlayerSpec where

import ClassyPrelude
import Data.List (nub)
import Test.Hspec (Spec, describe, it)
import Test.QuickCheck (forAll)
import SpecHelpers (genSinglePlayer, compileSolitaireDeck, compileWarDeck, shouldHaveAll52Cards)
import Types (Deck, SinglePlayer(..))

spec :: Spec
spec = do
  describe "SinglePlayer properties" $ do
    it "klondike always contains a full deck after dealing" $ forAll genSinglePlayer (deckMaintainedProp compileSolitaireDeck)
    it "war always contains a full deck after dealing" $ forAll genSinglePlayer (deckMaintainedProp compileWarDeck)

    it "klondike games after 3 moves contain a full deck" $ forAll genSinglePlayer (threeMovesDeepProp compileSolitaireDeck)
    it "war games after 3 moves contain a full deck" $ forAll genSinglePlayer (threeMovesDeepProp compileWarDeck)

deckMaintainedProp :: SinglePlayer game => (game -> Deck) -> game -> IO ()
deckMaintainedProp f = shouldHaveAll52Cards . f

threeMovesDeepProp :: (Eq game, SinglePlayer game) => (game -> Deck) -> game -> IO ()
threeMovesDeepProp f game = do
  let games = nub $ possibleMoves =<< possibleMoves =<< possibleMoves game
  void . sequence $ shouldHaveAll52Cards . f <$> games
