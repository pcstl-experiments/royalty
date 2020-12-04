{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Lib where
    -- ( someFunc
    -- , newGame
    -- , prettyPrintBoard
    -- , Board
    -- , Piece(..)
    -- , Color(..)
    -- , Place(..)
    -- ) where

import Data.Array (Array, (!))
import qualified Data.Array as Array
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.IO as TextIO

data Place
  = Occupied Color Piece
  | Free
  deriving (Show, Eq)

data Color
  = Black
  | White
  deriving (Show, Eq)

data Piece
  = King
  | Queen
  | Bishop
  | Knight
  | Rook
  | Pawn
  deriving (Show, Eq)

newtype Board = Board (Array (Int, Int) Place) deriving (Show, Eq)

showFancyPiece :: Color -> Piece -> Text
showFancyPiece White Pawn   = "♙"
showFancyPiece Black Pawn   = "♟"
showFancyPiece White Knight = "♘"
showFancyPiece Black Knight = "♞"
showFancyPiece White Bishop = "♗"
showFancyPiece Black Bishop = "♝"
showFancyPiece White Rook   = "♖"
showFancyPiece Black Rook   = "♜"
showFancyPiece White Queen  = "♕"
showFancyPiece Black Queen  = "♛"
showFancyPiece White King   = "♔"
showFancyPiece Black King   = "♚"

showFancyPlace :: Place -> Text
showFancyPlace Free                   = " "
showFancyPlace (Occupied color piece) = showFancyPiece color piece

boardToTextList :: Board -> [[Text]]
boardToTextList (Board boardArray) =
  chunksOf 8 $ [showFancyPlace $ boardArray ! (x, y) | y <- [8,7..1], x <- [1..8]]

prettyPrintBoard :: Board -> Text
prettyPrintBoard board =
  Text.unlines $ bracketPlaces <$> boardToTextList board

bracketPlaces :: [Text] -> Text
bracketPlaces =
  (Text.cons '|') . (flip Text.snoc $ '|') . (Text.intercalate "|")

startingBackRow :: Color -> [Place]
startingBackRow color =
  Occupied color <$> [Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook]

startingFrontRow color =
  Occupied color <$> (replicate 8 Pawn)

freeRow :: [Place]
freeRow = replicate 8 Free

startingRow :: Int -> [((Int, Int), Place)]
startingRow y = withIndices y $ getRow y

withIndices :: Int -> [a] -> [((Int, Int), a)]
withIndices y = zip ((,y) <$> [1..8])

getRow :: Int -> [Place]
getRow y
  | y == 1    = startingBackRow White
  | y == 2    = startingFrontRow White
  | y == 8    = startingBackRow Black
  | y == 7    = startingBackRow White
  | otherwise = freeRow

newGame :: Board
newGame = Board $ Array.array ((1, 1), (8, 8)) $
   concat $ startingRow <$> [8,7..1]

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n l = (take n l) : (chunksOf n (drop n l))

someFunc :: IO ()
someFunc = TextIO.putStrLn $ prettyPrintBoard newGame
