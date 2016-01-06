module Tetris where

import Control.Exception
import Control.Concurrent
import Control.Monad
import System.IO

import qualified TetrisGame as TG
import Terminal (colored, cursorToBottomLeft, cursorUp, newScreen)

newtype Game = Game { game :: TG.Game }
    deriving (Show)

blockDisplay :: Int -> [Char]
blockDisplay n = ("   " : [colored color "xxx" | color <- colors]) !! n
    where colors = ["red", "yellow", "green", "blue", "magenta", "cyan", "white"]

display :: Game -> IO ()
display g = boardDisplay (TG.boardView (game g))

boardDisplay :: [[Int]] -> IO ()
boardDisplay board = displayLines
     (["+" ++ replicate 30 '-' ++ "+"] ++
      (lineStrings board) ++
      ["+" ++ replicate 30 '-' ++ "+"])

lineStrings :: [[Int]] -> [[Char]]
lineStrings lines =
    if null lines
        then
            []
        else
            let line = head lines in
                ["|" ++ concat [blockDisplay x | x <- line] ++ "|",
                 "|" ++ concat [blockDisplay x | x <- line] ++ "|"] ++
                lineStrings (tail lines)

displayLines :: [String] -> IO()
displayLines lines = do
    cursorToBottomLeft
    cursorUp (length lines)
    mapM_ putStrLn lines

tick :: Game -> Int -> IO Game
tick g i = do
    display g
    inputReady <- hWaitForInput stdin 300
    c <- if inputReady then getChar else return ' '
    let newgame = TG.gameTick (game g) c
    return (Game newgame)

processMsg :: Game -> a -> Game
processMsg g _ = g

newGame :: Game
newGame = Game TG.newGame

mainIO :: IO ()
mainIO = foldM_ tick newGame [0..]

startApp = do
    putStrLn $ colored "red" "Let's play Tetris!"
    bracket_
        (do
            hSetBuffering stdin NoBuffering
            hSetEcho stdin False
            newScreen
            )
        (do
            hSetBuffering stdin LineBuffering
            hSetEcho stdin True
            newScreen
            putStrLn "bye"
            )
        mainIO
