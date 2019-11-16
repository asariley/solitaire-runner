module War where

import ClassyPrelude
import Control.Lens.TH (makeLenses)
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.List.NonEmpty as NEL
import Types (SinglePlayer(..), Card(Card), Deck)


data War = War
  { _myDeck :: Deck
  , _otherDeck :: Deck
  , _warPot :: Deck
  } deriving (Eq, Ord, Show)

makeLenses ''War

instance SinglePlayer War where
  gameIsWon (War _ [] _) = True
  gameIsWon _ = False

  newGame deck = War (take 26 deck) (drop 26 deck) []


  possibleMoves (War [] _ _) = []
  possibleMoves (War _ [] _) = []
  possibleMoves (War (m:ms) (o:os) pot) = case (m, o) of
    (Card _ numM, Card _ numO) | numM > numO -> [War (ms <> [m, o] <> pot) os []]
    (Card _ numM, Card _ numO) | numM < numO -> [War ms (os <> [m, o] <> pot) []]
    _ ->
      case (drop 3 ms, drop 3 os) of
        ([], []) -> [War [NEL.last myStack] [NEL.last theirStack] (pot <> NEL.init myStack <> NEL.init theirStack)]
        ([], theirs) -> [War [NEL.last myStack] theirs (pot <> NEL.init myStack <> NEL.toList theirStack)]
        (mine, []) -> [War mine [NEL.last theirStack] (pot <> NEL.toList myStack <> NEL.init theirStack)]
        (mine, theirs) -> [War mine theirs (pot <> NEL.toList myStack <> NEL.toList theirStack)]
      where myStack = m :| take 3 ms
            theirStack = o :| take 3 os

