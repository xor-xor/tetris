module Terminal where

import System.IO

data Color = Red | Green | Yellow | Blue | Magenta | Cyan | White deriving Eq

colors :: [(Color, String)]
colors = [ (Red,     "\x1b[41m")
         , (Green,   "\x1b[42m")
         , (Yellow,  "\x1b[43m")
         , (Blue,    "\x1b[44m")
         , (Magenta, "\x1b[45m")
         , (Cyan,    "\x1b[46m")
         , (White,   "\x1b[47m\x1b[30m")
         ]

color:: Color -> String
color c = maybe "" id (lookup c colors)

colored :: Color -> String -> String
colored c s = color c ++ s ++ "\x1b[0m"

saveCursor = "\x1b[s"

restoreCursor = "\x1b[s"

hideCursor = "\x1b[?25l"

showCursor = "\x1b[?25h"

newScreen = do
    putStr "\x1b[2J"
    hFlush stdout

cursorToBottomLeft = do
    putStr "\x1b[1000D"
    putStr "\x1b[1000B"
    hFlush stdout

cursorUp n = do
    putStr $ "\x1b[" ++ show n ++ "A"
    hFlush stdout

{-
strAtSpot :: Spot -> String -> String
strAtSpot (x, y) s = "\x1b[" ++ show ( y + 1 ) ++ ";" ++ show (x + 1) ++ "H" ++ s

putStrAtSpot :: Spot -> String -> IO ()
putStrAtSpot (x, y) s = putStr $ saveCursor ++ strAtSpot (x, y) s ++ restoreCursor

moveX :: Char -> Int -> Int
moveX 'h' x = x - 1
moveX 'l' x = x + 1
moveX _ x = x

moveY :: Char -> Int -> Int
moveY 'j' y = y + 1
moveY 'k' y = y - 1
moveY _ y = y

mainloop :: Spot -> IO ()
mainloop (x, y) = do
    let face = "(◕‿◕)"
    putStrAtSpot (x, y) (colored "red" face)
    hFlush stdout
    c <- getChar
    let newSpot = ((moveX c x), (moveY c y))
    mainloop newSpot

stuff :: IO ()
stuff = do
    putStrLn $ colored "red" "type things!"
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False
    putStr hideCursor
    mainloop (1, 0)
-}
