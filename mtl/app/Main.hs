module Main (main) where

import App

main :: IO ()
main = do
    putStrLn "Running TicTacToe in MTL style monad"
    mainApp
