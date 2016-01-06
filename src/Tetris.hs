module Tetris where

import Control.Exception
import Control.Concurrent
import Control.Monad
import System.IO

import TetrisGame (Game, Board, boardView, newGame, gameTick)
import Terminal (Color(..), colored, cursorToBottomLeft, cursorUp, newScreen)

blockDisplay :: Int -> String
blockDisplay n = ("   " : [colored color "xxx" | color <- colors]) !! n
    where colors = [Red, Yellow, Green, Blue, Magenta, Cyan, White]

display :: Game -> IO ()
display = boardDisplay . boardView

boardDisplay :: Board -> IO ()
boardDisplay board = displayLines $ ["+" ++ replicate 30 '-' ++ "+"] ++
                                    lineStrings board ++
                                    ["+" ++ replicate 30 '-' ++ "+"]

lineStrings :: Board -> [String]
lineStrings [] = []
lineStrings lines = [ "|" ++ concat [blockDisplay x | x <- line] ++ "|"
                    , "|" ++ concat [blockDisplay x | x <- line] ++ "|"
                    ] ++ lineStrings (tail lines)
                        where line = head lines

displayLines :: [String] -> IO()
displayLines lines = do
    cursorToBottomLeft
    cursorUp (length lines)
    mapM_ putStrLn lines

tick :: Game -> Int -> IO Game
tick g i = do
    display g
    let gameSpeed = 300
    inputReady <- hWaitForInput stdin gameSpeed
    c <- if inputReady then getChar else return ' '
    return (gameTick g c)

mainIO :: IO ()
mainIO = foldM_ tick newGame [0..]

startApp = do
    putStrLn $ colored Red "Let's play Tetris!"
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
