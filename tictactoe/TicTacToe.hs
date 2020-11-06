module TicTacToe where

import Data.List

data Player = PlayerX | PlayerO deriving (Eq)
data State  = Running | GameOver Cell
data Game   = Game Board Player State
data Board  = Board [[Cell]]

type Cell = Maybe Player

gameBoard :: Game -> Board
gameBoard (Game x _ _) = x

gamePlayer :: Game -> Player
gamePlayer (Game _ x _) = x

gameState :: Game -> State
gameState (Game _ _ x) = x

size :: Int
size = 3

initGame :: Game
initGame = Game (Board [[Nothing| i <- [1..size]] | i <- [1..size]]) PlayerX Running

switchPlayer :: Game -> Game
switchPlayer (Game x PlayerX z) = (Game x PlayerO z)
switchPlayer (Game x PlayerO z) = (Game x PlayerX z)

countEmptyCells :: Board -> Int
countEmptyCells (Board b) =  length $ filter (==Nothing) $ concat b

full :: [Cell] -> Maybe Player
full row
    | isSame    = head row
    | otherwise = Nothing
    where
        isSame = and $ map (== head row) (tail row)

winner :: Board -> Maybe Player
winner (Board cells)
    | null myWinner = Nothing
    | otherwise     = head myWinner
    where
        lines = [full row | row <- cells ++ transpose cells ++ diagonal]
        diagonal = [[cells !! y !! x | y <- [0..(size - 1)], x <- [0..(size - 1)], x == y], [cells !! y !! x | y <- [0..(size - 1)], x <- [0..(size - 1)], (x == 0 && y == (size - 1)) || (x == (size - 1) && y == 0) || (x == y && x == ((div size 2)))]]
        myWinner = filter (/= Nothing) lines

checkGameOver :: Game -> Game
checkGameOver g@(Game cells player state)
    | Just n <- winner cells     = (Game cells player (GameOver (Just n)))
    | 0 <- countEmptyCells cells = (Game cells player (GameOver Nothing))
    | otherwise                  = g

replaceAt :: Int -> a -> [a] -> [a]
replaceAt index item list = x ++ [item] ++ tail y
    where
        (x, y) = splitAt index list

playerTurn :: Game -> (Int,Int) -> Game
playerTurn g@(Game (Board cells) player state) (x, y)
    | validMove = switchPlayer $ checkGameOver (Game (Board newCells) player state)
    | otherwise = g
    where
        validMove = (not $ isGameOver g) && elem (x+1) [1..size] && elem (y + 1) [1..size] && (cells !! y !! x == Nothing)
        newCells  = replaceAt y (replaceAt x (Just player) (cells !! y)) cells

isGameOver :: Game -> Bool
isGameOver (Game _ _ (GameOver a)) = True
isGameOver _ = False