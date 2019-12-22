module Runner where

import ClassyPrelude
import qualified Data.HashSet as Set
import Types (SinglePlayer(..))


winnable :: (Eq game, Hashable game, SinglePlayer game) => game -> Bool
winnable game = snd $ gameTraversal Set.empty game

gameTraversal :: forall game. (Eq game, Hashable game, SinglePlayer game) => Set.HashSet game -> game -> (Set.HashSet game, Bool)
gameTraversal seenStates thisState =
  case (gameIsWon thisState, newMoves) of
    (True, _) -> (foundStates, True)
    (False, []) -> (foundStates, False)
    (False, nextMoves) -> foldr foldFn (foundStates, False) nextMoves
  where newMoves = filter (\m -> not $ Set.member m seenStates) $ possibleMoves thisState
        foundStates = Set.union seenStates $ Set.fromList newMoves
        foldFn _ (moveSet, True) = (moveSet, True)
        foldFn nextMove (moveSet, False) = gameTraversal moveSet nextMove


winnableIO :: (Eq game, Hashable game, SinglePlayer game) => game -> IO Bool
winnableIO game = snd <$> gameTraversalIO Set.empty game 0

gameTraversalIO :: forall game. (Eq game, Hashable game, SinglePlayer game) => Set.HashSet game -> game -> Int -> IO (Set.HashSet game, Bool)
gameTraversalIO seenStates thisState depth =
  case (gameIsWon thisState, newMoves) of
    (True, _) -> do
      putStrLn $ "WINNER found at depth " <> tshow depth
      pure (foundStates, True)
    (False, []) -> do
      putStrLn $ "LOSER found at depth " <> tshow depth
      pure (foundStates, False)
    (False, nextMoves) -> do
      putStrLn $ tshow (length nextMoves) <> " new moves found at depth " <> tshow depth
      putStrLn $ "    Set size: " <> tshow (Set.size foundStates)
      foldM foldFn (foundStates, False) nextMoves
  where newMoves = filter (\m -> not $ Set.member m seenStates) $ possibleMoves thisState
        foundStates = Set.union seenStates $ Set.fromList newMoves
        foldFn :: (Set.HashSet game, Bool) -> game -> IO (Set.HashSet game, Bool)
        foldFn (seen, True) _ = pure (seen, True)
        foldFn acc aMove = gameTraversalIO (fst acc) aMove (succ depth)

