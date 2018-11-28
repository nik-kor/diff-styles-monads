{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module Lib where

import           Prelude                 hiding ( take )
import qualified Data.Map.Strict               as M
import           Data.List                      ( transpose )
import           Data.List.Split                ( chunksOf )
import           Data.Maybe                     ( isJust )
import           Common

-- our monad's primitives

class Monad m => TicTacToe m where
    info :: Position -> m (Maybe Player)
    take :: Position -> m Taken
    -- fetch the current player
    who :: m Player
    -- tracking the moves in a game
    moves :: m Moves

class Monad m => Interactions m where
    showMessage :: String -> m ()
    getPlayerChoice :: m String


-- high-level API

takeIfNotTaken :: TicTacToe m => Position -> m Taken
takeIfNotTaken p = do
    i <- info p
    case i of
        Just _  -> return False
        Nothing -> take p

gameStatus :: TicTacToe m => Board -> m GameStatus
gameStatus b = return $ renderStatus b
  where
    wonBy :: Board -> Maybe Player
    wonBy b =
        let diagonals [[a1, _, b1], [_, c, _], [b2, _, a2]] =
                [[a1, c, a2], [b1, c, b2]]
            diagonals _ = error "cannot get diagnonals"
            ds      = diagonals b
            rows    = b
            cols    = transpose b
            winners = map
                (\case
                    [Right x, Right y, Right z] ->
                        if x == y && y == z then Just x else Nothing
                    _ -> Nothing
                )
                (ds ++ rows ++ cols)
        in  foldr (\a b -> if isJust a then a else b) Nothing winners

    draw b = null [ k | Left k <- concat b ]

    renderStatus board =
        maybe (if draw board then Draw else Active) Won (wonBy board)

gameBoard :: TicTacToe m => m Board
gameBoard = do
    ms <- moves
    return $ chunksOf 3 $ fmap
        (\p -> case M.lookup p ms of
            Just player -> Right player
            Nothing     -> Left p
        )
        allPositions
