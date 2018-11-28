{-# LANGUAGE FlexibleInstances #-}

import           Test.Hspec
import           Lib

import           Control.Monad.State
import qualified Data.Map.Strict               as M


data MockState = MockState {
      _moves :: Moves
    , _currentPlayer :: Player
} deriving (Show, Eq)

type MockedIO = State MockState

runMockIO :: MockedIO a -> MockState -> (a, MockState)
runMockIO = runState


instance TicTacToe MockedIO where
    info p = gets $ M.lookup p . _moves
    take p = do
        l <- info p
        case l of
            Just _ -> return False
            Nothing -> do
                ms <- moves
                cp <- who
                modify (const MockState { _moves = M.insert p cp ms, _currentPlayer = cp })
                return True
    who = gets _currentPlayer
    moves = gets _moves


main :: IO ()
main = hspec $ do
    describe "gameBoard" $ do
        it "should be empty if no moves" $ do
            let (res, _) = runMockIO gameBoard (MockState M.empty X)
            res
                `shouldBe` [ [Left One, Left Two, Left Three]
                           , [Left Four, Left Five, Left Six]
                           , [Left Seven, Left Eight, Left Nine]
                           ]

        it "should be with moves" $ do
            let
                (res, _) = runMockIO
                    gameBoard
                    (MockState (M.fromList [(Two, X), (Five, O)]) X)
            res
                `shouldBe` [ [Left One, Right X, Left Three]
                           , [Left Four, Right O, Left Six]
                           , [Left Seven, Left Eight, Left Nine]
                           ]

    describe "takeIfNotTaken" $ do
        it "should be possible to take empty cell" $ do
            let (res, _) = runMockIO (takeIfNotTaken One) (MockState M.empty X)
            res `shouldBe` True

        it "should not be possible to take already taken cell" $ do
            let
                (res, _) = runMockIO (takeIfNotTaken One)
                                     (MockState (M.fromList [(One, O)]) X)
            res `shouldBe` False

        it "should not be possible to take the same cell again" $ do
            let (res, _) = runMockIO
                    (takeIfNotTaken One >> takeIfNotTaken One)
                    (MockState (M.fromList []) X)
            res `shouldBe` False

        it "should be possible to take different cells" $ do
            let (res, _) = runMockIO
                    (takeIfNotTaken One >> takeIfNotTaken Two)
                    (MockState (M.fromList []) X)
            res `shouldBe` True

    describe "gameStatus" $ do
        it "should be Active with empty board" $ do
            let (res, _) = runMockIO (gameBoard >>= \b -> gameStatus b)
                                     (MockState (M.fromList []) X)

            res `shouldBe` Active

        it "should be Active with board partly busy" $ do
            let env = MockState
                    (M.fromList [(One, X), (Two, X), (Three, O), (Four, O)])
                    X
                test     = gameBoard >>= \b -> gameStatus b
                (res, _) = runMockIO test env

            res `shouldBe` Active

        -- x x o
        -- o o x
        -- x o x
        it "should be Draw" $ do
            let env = MockState
                    (M.fromList
                        [ (One  , X)
                        , (Two  , X)
                        , (Three, O)
                        , (Four , O)
                        , (Five , O)
                        , (Six  , X)
                        , (Seven, X)
                        , (Eight, O)
                        , (Nine , X)
                        ]
                    )
                    X
                test     = gameBoard >>= \b -> gameStatus b
                (res, _) = runMockIO test env

            res `shouldBe` Draw

        -- x x x
        -- o o x
        -- x o x
        it "should be Win by x" $ do
            let env = MockState
                    (M.fromList
                        [ (One  , X)
                        , (Two  , X)
                        , (Three, X)
                        , (Four , O)
                        , (Five , O)
                        , (Six  , X)
                        , (Seven, X)
                        , (Eight, O)
                        , (Nine , X)
                        ]
                    )
                    X
                test     = gameBoard >>= \b -> gameStatus b
                (res, _) = runMockIO test env

            res `shouldBe` Won X
