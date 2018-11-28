{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module FreeApp
    ( mainApp
    )
where

import           Prelude                 hiding ( take )
import           Control.Monad.Free             ( foldFree )

import qualified Data.Map.Strict               as M
import           Control.Monad.State.Lazy
import           Data.List                      ( transpose )
import           Data.List.Split                ( chunksOf )
import           Data.List.Index                ( ifoldr )
import           Data.Maybe                     ( isJust
                                                , isNothing
                                                )

import           Common
import           LibFree


takeIfNotTaken :: Position -> TicTacToeM Taken
takeIfNotTaken p = do
    i <- info p
    case i of
        Just _  -> return False
        Nothing -> take p

gameStatus :: Board -> TicTacToeM GameStatus
gameStatus b = return $ renderStatus b
  where
    wonBy :: Board -> Maybe Player
    wonBy _b =
        let diagonals [[a1, _, b1], [_, c, _], [b2, _, a2]] =
                [[a1, c, a2], [b1, c, b2]]
            diagonals _ = error "cannot get diagonals"
            ds      = diagonals _b
            rows    = _b
            cols    = transpose _b
            winners = map
                (\case
                    [Right x, Right y, Right z] ->
                        if x == y && y == z then Just x else Nothing
                    _ -> Nothing
                )
                (ds ++ rows ++ cols)
        in  foldr (\x y -> if isJust x then x else y) Nothing winners

    draw b = null [ k | Left k <- concat b ]

    renderStatus board =
        maybe (if draw board then Draw else Active) Won (wonBy board)

gameBoard :: TicTacToeM Board
gameBoard = do
    ms <- moves
    return $ chunksOf 3 $ fmap
        (\p -> case M.lookup p ms of
            Just player -> Right player
            Nothing     -> Left p
        )
        allPositions


data AppState = AppState {
      _moves :: Moves
    , _currentPlayer :: Player
}

type AppIO = (StateT AppState IO)

runGame' :: TicTacToeF a -> AppIO a
runGame' (Info p k) = do
    pl <- M.lookup p . _moves <$> get
    return (k pl)

runGame' (Take p k) = do
    pl <- M.lookup p . _moves <$> get

    case pl of
        Just _ -> do
            liftIO (putStrLn "already taken - try another one")
            return (k False)
        Nothing -> do
            cp <- _currentPlayer <$> get
            ms <- _moves <$> get
            put AppState
                { _moves         = M.insert p cp ms
                , _currentPlayer = swapPlayer cp
                }

            return (k True)
  where
    swapPlayer :: Player -> Player
    swapPlayer X = O
    swapPlayer O = X

runGame' (GetMoves k) = do
    ms <- _moves <$> get
    return (k ms)

runGame' (NextMove k) = do
    cp <- _currentPlayer <$> get
    liftIO (putStrLn $ "Your choice " ++ show cp)
    pos <- liftIO getLine

    let pos' = getPos pos
    when (isNothing pos') (liftIO $ putStrLn "Incorrect value. Try again")

    return $ k pos'
  where
    getPos :: String -> Maybe Position
    getPos v = M.lookup v . M.fromList $ ifoldr
        (\i a b -> (show $ i + 1, a) : b)
        []
        allPositions

runGame' (ShowBoard b k) = do
    liftIO $ putStrLn $ renderBoard b

    return k

runGame' (EndGame s k) = do
    liftIO $ print s

    return k

runGame :: TicTacToeM a -> AppIO a
runGame = foldFree runGame'

game :: TicTacToeM ()
game = do
    b <- gameBoard
    showBoard b
    status <- gameStatus b

    case status of
        Active -> do
            pos <- nextMove
            case pos of
                Just pos' -> do
                    _ <- takeIfNotTaken pos'
                    game
                Nothing -> game

            game
        _ -> endGame status

mainApp :: IO ()
mainApp = fst <$> runStateT (runGame game) (AppState M.empty X)
