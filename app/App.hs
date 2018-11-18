{-# LANGUAGE FlexibleInstances #-}

module App (mainApp) where

import Lib
import           Prelude
import qualified Data.Map.Strict               as M
import           Control.Monad.Reader
import           Control.Monad.State.Lazy
import           Data.List                      ( intersperse
                                                , transpose
                                                , elemIndex
                                                )
import           Data.List.Split                ( chunksOf )
import           Data.List.Index                ( ifoldr )
import           Data.Maybe                     ( isJust
                                                , fromJust)

data AppState = AppState {
      _moves :: Moves
    , _currentPlayer :: Player
}

type AppIO = (StateT AppState IO)

-- interpretation for console game

instance TicTacToe AppIO where
    info p = M.lookup p . _moves <$> get

    take p = do
        l <- info p
        case l of
            Just p' -> do
                liftIO (putStrLn "already taken - try another one")
                return False
            Nothing -> do
                cp <- who
                ms <- moves
                put AppState { _moves = M.insert p cp ms, _currentPlayer = swapPlayer cp }

                return True
        where
            swapPlayer :: Player -> Player
            swapPlayer X = O
            swapPlayer O = X

    who = _currentPlayer <$> get
    moves = _moves <$> get



getPos :: String -> Maybe Position
getPos v = M.lookup v . M.fromList $ ifoldr
    (\i a b -> (show $ i + 1, a) : b)
    []
    allPositions

app :: AppIO ()
app = do
    b <- gameBoard
    liftIO $ putStrLn $ renderBoard b

    status <- gameStatus b

    case status of
        Draw   -> liftIO $ putStrLn "Draw!"
        Won by -> liftIO $ putStrLn $ "Won by " ++ show by
        Active -> do
            cp <- who
            liftIO $ putStrLn $ "Your choice " ++ show cp
            pos <- liftIO getLine
            case getPos pos of
                Just pos' -> do
                    isTaken <- takeIfNotTaken pos'
                    if not isTaken
                        then do
                            liftIO (putStrLn "already taken - try another one")
                            app
                        else app
                Nothing -> do
                    liftIO $ putStrLn "Incorrect cell number. Please try again"
                    app
  where
    renderBoard :: Board -> String
    renderBoard = unlines . surround "+---+---+---+" . map
        (concat . surround "|" . map renderCell)
      where
        surround x xs = [x] ++ intersperse x xs ++ [x]

        renderCell :: Cell -> String
        renderCell =
            either (\n -> " " ++ show n ++ " ") (concat . replicate 3 . show)

mainApp = fst <$> runStateT app (AppState M.empty X)
