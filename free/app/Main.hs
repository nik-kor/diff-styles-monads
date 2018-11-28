module Main where

import           FreeApp                        ( mainApp )

main :: IO ()
main = do
    putStrLn "Running TicTacToe in free monad"
    mainApp
