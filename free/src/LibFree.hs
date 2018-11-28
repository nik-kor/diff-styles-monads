{-# LANGUAGE DeriveFunctor #-}

module LibFree where

import           Common
import           Control.Monad.Free             ( liftF
                                                , Free(..)
                                                )


data TicTacToeF r =
    Info Position (Maybe Player -> r)
    | Take Position (Taken -> r)
    | GetMoves (Moves -> r)
    | NextMove (Maybe Position -> r)
    | ShowBoard Board r
    | EndGame GameStatus r
    deriving (Functor)

type TicTacToeM = Free TicTacToeF

info :: Position -> TicTacToeM (Maybe Player)
info p = Free (Info p return)

take :: Position -> TicTacToeM Taken
take p = Free (Take p return)

moves :: TicTacToeM Moves
moves = Free (GetMoves return)

nextMove :: TicTacToeM (Maybe Position)
nextMove = Free (NextMove return)

showBoard :: Board -> TicTacToeM ()
showBoard b = liftF (ShowBoard b ())

endGame :: GameStatus -> TicTacToeM ()
endGame s = liftF (EndGame s ())
