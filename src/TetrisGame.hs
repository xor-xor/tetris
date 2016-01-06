module TetrisGame where

import Piece

data Game = G { board    :: [[Int]]
              , piece    :: Piece
              , location :: (Int, Int)
              , seed     :: Int
              } deriving (Show, Eq)

type Board = [[Int]]

type Delta = (Int, Int)

newGame :: Game
newGame = G b pieceT1 (3, 3) 12345
    where b = [replicate 10 0 | x <- [1..20]]

boardView :: Game -> Board
boardView game = withPiece (activePiece game) (board game)

activePiece :: Game -> Piece
activePiece game = pieceAtPos (piece game) (location game)

freezePiece :: Game -> Game
freezePiece game = let (newPiece, newSeed) = psuedoRandomPiece (seed game) in
                   G (boardView game) newPiece (0, 0) newSeed

pieceFits :: Game -> Bool
pieceFits game = not (any (spotBad (board game)) (spots p))
    where p = activePiece game

spotFilled :: Board -> Spot -> Bool
spotFilled board (x, y) = ((board !! y) !! x) /= 0

spotExists :: Board -> Spot -> Bool
spotExists board (x, y) = (y < length board) &&
                           x < length (head board) &&
                           y > -1 && x > -1

spotBad :: Board -> Spot -> Bool
spotBad board spot = not (spotExists board spot) || spotFilled board spot

dropPiece :: Game -> Game
dropPiece g =
    let (x, y) = location g in
        let newG = G (board g) (piece g) (x, y+1) (seed g) in
            if pieceFits newG
            then newG
            else removeLines (freezePiece g)

isFull :: [Int] -> Bool
isFull line = 0 `notElem` line

notFull :: [Int] -> Bool
notFull line = 0 `elem` line

removeLines :: Game -> Game
removeLines (G b p l s) =
    let left = filter notFull b in
        let newLines = replicate (length b - length left) (replicate (length (head b)) 0) in
            G (newLines ++ left) p l s

movePiece :: Game -> Int -> Game
movePiece g dx =
    let (x, y) = location g in
        let newG = G (board g) (piece g) (x + dx, y) (seed g) in
            if pieceFits newG
            then newG
            else g

rotatedPiece :: Game -> Game
rotatedPiece (G b p l s) = G b (rotate p) l s

rotatePiece :: Game -> Game
rotatePiece g = if pieceFits (rotatedPiece g)
                then rotatedPiece g
                else g

withBlock :: Int -> Spot -> Board -> Board -- XXX
withBlock t (x, y) board =
    [ if fst l == y
      then [if fst m == x then t else snd m | m <- zip [0..] (snd l)]
      else snd l | l <- zip [0..] board ]

withPiece :: Piece -> Board -> Board
withPiece piece board = foldr (withBlock (texture piece)) board (spots piece)

offset :: Delta -> Spot -> Spot
offset (dx, dy) (x, y) = (dx + x, dy + y)

pieceAtPos :: Piece -> Delta -> Piece
pieceAtPos p delta = P (map (offset delta) (spots p)) (texture p)

gameTick :: Game -> Char -> Game
gameTick game 'a' = movePiece game (-1)
gameTick game 'd' = movePiece game 1
gameTick game 'e' = rotatePiece game
gameTick game 'q' = rotatePiece game
gameTick game _ = dropPiece game

gamestates :: [Game]
gamestates = iterate dropPiece newGame
