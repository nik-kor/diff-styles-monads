{-# LANGUAGE FlexibleInstances #-}

module App
    ( mainApp
    )
where

import           Lib
import           Prelude
import qualified Data.Map.Strict               as M
import           Control.Monad.Reader
import           Control.Monad.State.Lazy
import           Data.List.Index                ( ifoldr )
import           Common

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
            Just _ -> do
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

instance Interactions AppIO where
    showMessage s = liftIO $ putStrLn s
    getPlayerChoice = liftIO getLine


getPos :: String -> Maybe Position
getPos v = M.lookup v . M.fromList $ ifoldr
    (\i a b -> (show $ i + 1, a) : b)
    []
    allPositions

app :: (TicTacToe m, Interactions m) => m ()
app = do
    b <- gameBoard
    showMessage $ renderBoard b

    status <- gameStatus b

    case status of
        Draw    -> showMessage "Draw!"
        Won _by -> showMessage $ "Won by " ++ show _by
        Active  -> do
            cp <- who
            showMessage $ "Your choice " ++ show cp
            pos <- getPlayerChoice
            case getPos pos of
                Just pos' -> do
                    isTaken <- takeIfNotTaken pos'
                    if not isTaken
                        then do
                            showMessage "already taken - try another one"
                            app
                        else app
                Nothing -> do
                    showMessage "Incorrect cell number. Please try again"
                    app

mainApp :: IO ()
mainApp = fst <$> runStateT app (AppState M.empty X)
