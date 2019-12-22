module Types where

import ClassyPrelude
import Control.Lens.TH (makeLenses)


data Color = ColorRed | ColorBlack deriving (Eq, Show)

toColor :: Suit -> Color
toColor SuitHearts   = ColorRed
toColor SuitSpades   = ColorBlack
toColor SuitDiamonds = ColorRed
toColor SuitClubs    = ColorBlack

data Suit = SuitHearts | SuitSpades | SuitDiamonds | SuitClubs
  deriving (Eq, Show, Generic)

instance Hashable Suit

allSuits :: [Suit]
allSuits = [SuitHearts, SuitSpades, SuitDiamonds, SuitClubs]

newtype CardNumber = CardNumber { unCardNumber :: Word8 } deriving (Eq, Ord, Hashable, Enum, Show)

allCardNumbers :: [CardNumber]
allCardNumbers = map CardNumber [1..13]

king :: CardNumber
king = CardNumber 13

queen :: CardNumber
queen = CardNumber 12

data Card = Card
  { _cardSuit :: Suit
  , _cardNumber :: CardNumber
  } deriving (Eq, Show)

instance Hashable Card where
  hashWithSalt salt (Card suit num) = hashWithSalt (hashWithSalt salt num) suit

makeLenses ''Card

type Deck = [Card]

sortedDeck :: Deck
sortedDeck = do
  suit <- allSuits
  number <- allCardNumbers
  pure $ Card suit number

class SinglePlayer game where
  gameIsWon :: game -> Bool
  newGame :: Deck -> game
  possibleMoves :: game -> [game]

exampleDeck :: Deck
exampleDeck =
  [ Card SuitSpades   (CardNumber 10)
  , Card SuitClubs    (CardNumber 1)
  , Card SuitHearts   (CardNumber 8)
  , Card SuitClubs    (CardNumber 8)
  , Card SuitClubs    (CardNumber 4)
  , Card SuitSpades   (CardNumber 5)
  , Card SuitDiamonds (CardNumber 9)
  , Card SuitDiamonds (CardNumber 8)
  , Card SuitClubs    (CardNumber 13)
  , Card SuitHearts   (CardNumber 12)
  , Card SuitHearts   (CardNumber 5)
  , Card SuitHearts   (CardNumber 9)
  , Card SuitSpades   (CardNumber 3)
  , Card SuitHearts   (CardNumber 11)
  , Card SuitClubs    (CardNumber 5)
  , Card SuitSpades   (CardNumber 9)
  , Card SuitHearts   (CardNumber 1)
  , Card SuitHearts   (CardNumber 6)
  , Card SuitDiamonds (CardNumber 12)
  , Card SuitHearts   (CardNumber 4)
  , Card SuitDiamonds (CardNumber 10)
  , Card SuitClubs    (CardNumber 7)
  , Card SuitSpades   (CardNumber 4)
  , Card SuitClubs    (CardNumber 6)
  , Card SuitClubs    (CardNumber 2)
  , Card SuitDiamonds (CardNumber 2)
  , Card SuitSpades   (CardNumber 12)
  , Card SuitDiamonds (CardNumber 5)
  , Card SuitHearts   (CardNumber 2)
  , Card SuitDiamonds (CardNumber 11)
  , Card SuitHearts   (CardNumber 3)
  , Card SuitClubs    (CardNumber 10)
  , Card SuitSpades   (CardNumber 13)
  , Card SuitHearts   (CardNumber 7)
  , Card SuitSpades   (CardNumber 1)
  , Card SuitDiamonds (CardNumber 4)
  , Card SuitHearts   (CardNumber 10)
  , Card SuitSpades   (CardNumber 8)
  , Card SuitClubs    (CardNumber 9)
  , Card SuitDiamonds (CardNumber 3)
  , Card SuitHearts   (CardNumber 13)
  , Card SuitDiamonds (CardNumber 6)
  , Card SuitDiamonds (CardNumber 13)
  , Card SuitSpades   (CardNumber 6)
  , Card SuitDiamonds (CardNumber 1)
  , Card SuitSpades   (CardNumber 2)
  , Card SuitClubs    (CardNumber 12)
  , Card SuitDiamonds (CardNumber 7)
  , Card SuitClubs    (CardNumber 11)
  , Card SuitSpades   (CardNumber 7)
  , Card SuitClubs    (CardNumber 3)
  , Card SuitSpades   (CardNumber 11)
  ]
