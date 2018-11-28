module Common where

import qualified Data.Map.Strict               as M
import           Data.List                      ( elemIndex, intersperse )



data Player = O | X deriving (Eq, Show)

data GameStatus =
    Active
    | Draw
    | Won { by :: Player } deriving (Eq)

type Cell = Either Position Player
type Board = [[Cell]]
type Moves = M.Map Position Player

data Position = One | Two | Three | Four | Five | Six | Seven | Eight | Nine deriving (Eq, Ord)
allPositions :: [Position]
allPositions = [One, Two, Three, Four, Five, Six, Seven, Eight, Nine]

instance Show Position where
    show p = case elemIndex p allPositions of
        Just p' ->
            show $ p' + 1
        Nothing ->
            "Cannot get the value for position"

instance Show GameStatus where
    show Active = "is active"
    show Draw = "is draw"
    show (Won by) = "is won by " ++ show by

type Taken = Bool

renderBoard :: Board -> String
renderBoard = unlines . surround "+---+---+---+" . map
    (concat . surround "|" . map renderCell)
  where
    surround x xs = [x] ++ intersperse x xs ++ [x]

    renderCell :: Cell -> String
    renderCell =
        either (\n -> " " ++ show n ++ " ") (concat . replicate 3 . show)
