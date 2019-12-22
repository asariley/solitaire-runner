module Klondike where

import ClassyPrelude
import Control.Lens (ALens', cloneLens, view, storing, set, over, _2, each)
import Control.Lens.TH (makeLenses)
import Types (Card(Card), CardNumber(CardNumber), Suit(..), SinglePlayer, toColor)
import qualified Types as T


-- fst is the invisible cards, snd is the visible stack
type PositionStack = ([Card], [Card])

data SolitaireGame = SolitaireGame
  { _gameScoreHearts   :: CardNumber
  , _gameScoreSpades   :: CardNumber
  , _gameScoreDiamonds :: CardNumber
  , _gameScoreClubs    :: CardNumber
  , _gameFlipped       :: [Card]
  , _gameToFlip        :: [Card]
  , _gamePosition0     :: PositionStack
  , _gamePosition1     :: PositionStack
  , _gamePosition2     :: PositionStack
  , _gamePosition3     :: PositionStack
  , _gamePosition4     :: PositionStack
  , _gamePosition5     :: PositionStack
  , _gamePosition6     :: PositionStack
  } deriving (Eq, Ord, Show)

makeLenses ''SolitaireGame

instance SinglePlayer SolitaireGame where
  gameIsWon game
    =  view gameScoreHearts game == T.king
    && view gameScoreSpades game == T.king
    && view gameScoreDiamonds game == T.king
    && view gameScoreClubs game == T.king

  newGame deck =
    SolitaireGame
      scoreZero
      scoreZero
      scoreZero
      scoreZero
      []
      (drop 28 deck)
      (flipOverTopCard $ dealPosition 0)
      (flipOverTopCard $ dealPosition 1)
      (flipOverTopCard $ dealPosition 2)
      (flipOverTopCard $ dealPosition 3)
      (flipOverTopCard $ dealPosition 4)
      (flipOverTopCard $ dealPosition 5)
      (flipOverTopCard $ dealPosition 6)
    where scoreZero = CardNumber 0
          dealPosition :: Int -> PositionStack
          dealPosition i = (take (i + 1) . drop ((i * (i + 1)) `div` 2) $ deck, [])
          flipOverTopCard (c:cs, []) = (cs, [c])
          flipOverTopCard original = original

  possibleMoves game = scoreCardMoves game <> maybe [] (:[]) (flipCardMove game) <> stackCardMoves game

topCards :: SolitaireGame -> [(Card, ALens' SolitaireGame [Card])]
topCards game = helper lenses []
  where helper (f:fs) acc = case view (cloneLens f) game of
          [] -> helper fs acc
          c:_ -> helper fs $ (c, f) : acc
        helper [] acc = acc
        lenses = [ gameFlipped
                 , gamePosition0 . _2
                 , gamePosition1 . _2
                 , gamePosition2 . _2
                 , gamePosition3 . _2
                 , gamePosition4 . _2
                 , gamePosition5 . _2
                 , gamePosition6 . _2
                 ]

-- the available locations to place stacks
stackCards :: SolitaireGame -> [(Maybe Card, ALens' SolitaireGame [Card])]
stackCards game = helper1 lenses []
  where helper1 (f:fs) acc = case view (cloneLens f) game of
          [] -> helper2 fs $ (Nothing, f) : acc -- helper1 finds empty spaces
          c:_ -> helper1 fs $ (Just c, f) : acc
        helper1 [] acc = acc
        helper2 (f:fs) acc = case view (cloneLens f) game of -- we only need to know about the first available empty space. helper2 doesn't find any empty spaces
          [] -> helper2 fs acc
          c:_ -> helper2 fs $ (Just c, f) : acc
        helper2 [] acc = acc
        lenses = [ gamePosition0 . _2
                 , gamePosition1 . _2
                 , gamePosition2 . _2
                 , gamePosition3 . _2
                 , gamePosition4 . _2
                 , gamePosition5 . _2
                 , gamePosition6 . _2
                 ]

-- (remaining, moving, field)
movableStacks :: SolitaireGame -> [([Card], [Card], ALens' SolitaireGame [Card])]
movableStacks game = maybe id (:) flippedStackMoveMay $ revealingStackMoves <> scorableStackMoves 1 lenses
  where scorableStackMoves :: Int -> [ALens' SolitaireGame [Card]] -> [([Card], [Card], ALens' SolitaireGame [Card])]
        scorableStackMoves stackIndex (f:fs) = case splitAt stackIndex $ view (cloneLens f) game of
          ([], _) -> scorableStackMoves 1 fs
          (_, []) -> scorableStackMoves 1 fs
          (c:cs, stck) | c `elem` scorableCards game -> (c:cs, stck, f) : scorableStackMoves (succ stackIndex) (f:fs)
          _ -> scorableStackMoves (succ stackIndex) (f:fs)
        scorableStackMoves _ [] = []

        flippedStackMoveMay :: Maybe ([Card], [Card], ALens' SolitaireGame [Card])
        flippedStackMoveMay = case view gameFlipped game of
          (c:cs) -> Just (cs, [c], gameFlipped)
          [] -> Nothing

        revealingStackMoves = catMaybes
          [ revealingStack (gamePosition0 . _2) $ view gamePosition0 game
          , revealingStack (gamePosition1 . _2) $ view gamePosition1 game
          , revealingStack (gamePosition2 . _2) $ view gamePosition2 game
          , revealingStack (gamePosition3 . _2) $ view gamePosition3 game
          , revealingStack (gamePosition4 . _2) $ view gamePosition4 game
          , revealingStack (gamePosition5 . _2) $ view gamePosition5 game
          , revealingStack (gamePosition6 . _2) $ view gamePosition6 game
          ]
        revealingStack _ ([], _) = Nothing
        revealingStack _ (_, []) = Nothing
        revealingStack f (_, stck) = Just ([], stck, f)
        lenses = [ gamePosition0 . _2
                 , gamePosition1 . _2
                 , gamePosition2 . _2
                 , gamePosition3 . _2
                 , gamePosition4 . _2
                 , gamePosition5 . _2
                 , gamePosition6 . _2
                 ]

scorableCards :: SolitaireGame -> [Card]
scorableCards game = filter ((/= T.king) . view T.cardNumber) . over (each . T.cardNumber) succ $ topScores
  where topScores =
          [ Card SuitHearts (view gameScoreHearts game)
          , Card SuitSpades (view gameScoreSpades game)
          , Card SuitDiamonds (view gameScoreDiamonds game)
          , Card SuitClubs (view gameScoreClubs game)
          ]

-- enforce the invariant that if there are any invisible cards there is at least one visible card on the stack
revealStack :: ALens' SolitaireGame PositionStack -> SolitaireGame -> SolitaireGame
revealStack f game = over (cloneLens f) correctStack game
  where correctStack (c:cs, []) = (cs, [c])
        correctStack original = original

flipOverTopCards :: SolitaireGame -> SolitaireGame
flipOverTopCards
  = revealStack gamePosition0
  . revealStack gamePosition1
  . revealStack gamePosition2
  . revealStack gamePosition3
  . revealStack gamePosition4
  . revealStack gamePosition5
  . revealStack gamePosition6

flipCardMove :: SolitaireGame -> Maybe SolitaireGame
flipCardMove game = case (view gameToFlip game, length (view gameFlipped game)) of
  ([], l) | l < 3 -> Nothing
  ([], _)         -> Just . set gameToFlip (reverse $ view gameFlipped game) . set gameFlipped [] $ game
  (toFlip, _)     -> Just . over gameFlipped (reverse (take 3 toFlip) <>) . set gameToFlip (drop 3 toFlip) $ game

scoreCardMoves :: SolitaireGame -> [SolitaireGame]
scoreCardMoves game = catMaybes . map scoreCard $ topCards game
  where scoreCard :: (Card, ALens' SolitaireGame [Card]) -> Maybe SolitaireGame
        scoreCard (Card T.SuitHearts num, f)   = bool Nothing (Just $ updatedGame gameScoreHearts f) $ pred num == view gameScoreHearts game
        scoreCard (Card T.SuitSpades num, f)   = bool Nothing (Just $ updatedGame gameScoreSpades f) $ pred num == view gameScoreSpades game
        scoreCard (Card T.SuitDiamonds num, f) = bool Nothing (Just $ updatedGame gameScoreDiamonds f) $ pred num == view gameScoreDiamonds game
        scoreCard (Card T.SuitClubs num, f)    = bool Nothing (Just $ updatedGame gameScoreClubs f) $ pred num == view gameScoreClubs game
        updatedGame scoreF f = flipOverTopCards . over scoreF succ . over (cloneLens f) (drop 1) $ game

stackCardMoves :: SolitaireGame -> [SolitaireGame]
stackCardMoves game = do
  dst <- stackCards game
  src <- movableStacks game
  maybe [] (:[]) $ stackMove dst src game

stackMove :: (Maybe Card, ALens' SolitaireGame [Card]) -> ([Card], [Card], ALens' SolitaireGame [Card]) -> SolitaireGame -> Maybe SolitaireGame
stackMove (destCard, destF) (sourceRemaining, sourceMoving, sourceF) game =
  case aCanHoldB destCard <$> sourceBaseCard of
    Just True -> Just . flipOverTopCards . over (cloneLens destF) (sourceMoving <>) . storing sourceF sourceRemaining $ game
    _ -> Nothing
  where aCanHoldB (Just (Card suitA numA)) (Card suitB numB) = (toColor suitA /= toColor suitB) && (numA == succ numB)
        aCanHoldB Nothing (Card _ (CardNumber 13)) = True
        aCanHoldB _ _ = False
        sourceBaseCard = lastMay sourceMoving
