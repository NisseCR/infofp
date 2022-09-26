{- This is a framework in which all functions to be written are "undefined".  -
 - Note that in most cases parameters, pattern-matching and guards have been  -
 - omitted! You will have to add those yourself.                              -}

{-# LANGUAGE TupleSections #-} {- A handy syntax extension. See:

    http://www.haskell.org/ghc/docs/7.6.3/html/users_guide/syntax-extns.html#tuple-sections

-}

module Assignment2 where -- Rename to "Main" if you want to compile the game.
                         -- Don't forget to rename it back when submitting!

import Control.Monad

import Data.Char
import Data.List
import Data.Maybe

import System.IO

-- | Rose trees

data Rose a = MkRose a [Rose a]
    deriving (Eq, Show)

-- Exercise 1
-- | input e.g. root (MkRose 1 [MkRose 4 [MkRose 100 [], MkRose 200 []], MkRose 5 [], MkRose 6 [], MkRose 7 []])
root :: Rose a -> a
root (MkRose a _)= a

children :: Rose a -> [Rose a]
children (MkRose _ children) = children

-- Exercise 2
size :: Rose a -> Int
size (MkRose _ [])     = 1
size (MkRose _ children) = 1 + sum (map size children)

leaves :: Rose a -> Int
leaves (MkRose _ [])       = 1
leaves (MkRose _ children) = sum (map leaves children)

-- | State representation

-- * Players

data Player = P1 | P2
    deriving (Eq, Ord)

instance Show Player where
    show P1 = "Player 1"
    show P2 = "Player 2"

-- Exercise 3

nextPlayer :: Player -> Player
nextPlayer P1 = P2
nextPlayer P2 = P1

-- * Board

data Field = X | O | B
    deriving (Eq, Ord)

instance Show Field where
    show X = "X"
    show O = "O"
    show B = " "

-- Exercise 4

symbol :: Player -> Field
symbol P1 = X
symbol P2 = O

type Row   = (Field, Field, Field)
type Board = (Row, Row, Row)

-- Exercise 5
fst' :: Row -> Field
fst' (a, _, _) = a

snd' :: Row -> Field
snd' (_, b, _) = b

trd' :: Row -> Field
trd' (_, _, c) = c

verticals :: Board -> (Row, Row, Row)
verticals brd = (vertical fst' brd, vertical snd' brd , vertical trd' brd)
  where
    vertical :: (Row -> Field) -> Board -> Row
    vertical f (a, b, c) = (f a, f b, f c)


diagonals :: Board -> (Row, Row)
diagonals (a, b, c) = ((fst' a, snd' b, trd' c), (trd' a, snd' b, fst' c))

-- Exercise 6

emptyBoard :: Board
emptyBoard = ((B, B, B), (B, B, B), (B, B, B))

-- Exercise 7
-- | input e.g. printBoard ((X, B, B), (B, B, X), (O, B, O))
printBoard :: Board -> String
printBoard (a, b, c)= printRow a ++ printLine ++ printRow b ++ printLine ++ printRow c
  where
    printLine :: String
    printLine = "-+-+-\n"

    printRow :: Row -> String
    printRow (a, b, c) = show a ++ "|" ++ show b ++ "|" ++ show c  ++ "\n"

-- | Move generation

-- Exercise 8
-- | input e.g. moves P1 ((X, B, B), (B, B, X), (O, B, O))

traverseFst :: (a -> [d]) -> (a, b, c) -> [(d, b, c)]
traverseFst f row@(a, b, c)= map (\r -> (r, b, c)) (f a)

traverseSnd :: (b -> [d]) -> (a, b, c) -> [(a, d, c)]
traverseSnd f row@(a, b, c)= map (\r -> (a, r, c)) (f b)

traverseTrd :: (c -> [d]) -> (a, b, c) -> [(a, b, d)]
traverseTrd f row@(a, b, c)= map (\r -> (a, b, r)) (f c)

traverseAll :: (a -> [a]) -> (a, a, a) -> [(a, a, a)]
traverseAll f row = traverseFst f row ++ traverseSnd f row ++ traverseTrd f row

movesField :: Player -> Field -> [Field]
movesField player x | x /= B = []
                    | player == P1 = [X]
                    | otherwise = [O]


movesRow :: Player -> Row -> [Row]
movesRow player r = traverseAll (movesField player) r

moves :: Player -> Board -> [Board]
moves player brd@(x, y, z) = map (\r -> (r, y, z)) (movesRow player x)
                             ++ map (\r -> (x, r, z)) (movesRow player y)
                             ++ map (\r -> (x, y, r)) (movesRow player z)

-- | Gametree generation

-- Exercise 9
-- | input e.g. hasWinner ((X, B, O), (B, X, X), (O, B, X))   // P1
-- | input e.g. hasWinner ((X, B, O), (B, X, X), (O, B, B))   // P2
hasWinner :: Board -> Maybe Player
hasWinner brd | w1 && w2 = Nothing
              | w1 = Just P1
              | w2 = Just P2
              | otherwise = Nothing
  where
    boardToList :: Board -> [Row]
    boardToList (a, b, c) = a : b : c : []

    diagonalsToList :: (Row, Row) -> [Row]
    diagonalsToList (a, b) = a : b : []

    rowChecks :: [Row]
    rowChecks = boardToList brd
              ++ boardToList (verticals brd)
              ++ diagonalsToList (diagonals brd)

    winnerRow :: Player -> Row -> Bool
    winnerRow player (a, b, c) = a == b && b == c && (symbol player) == a

    isWinner :: Player -> [Row] -> Bool
    isWinner _ [] = False
    isWinner player (x:xs) = winnerRow player x || isWinner player xs

    w1 :: Bool
    w1 = isWinner P1 rowChecks

    w2 :: Bool
    w2 = isWinner P2 rowChecks

-- Exercise 10
-- | input e.g. gameTree P1 emptyBoard
-- | input e.g. gameTree P1 ((O, O, B), (X, O, B), (X, B, X))
gameTree :: Player -> Board -> Rose Board
gameTree player brd | isJust(hasWinner brd) = MkRose brd []
                    | otherwise = MkRose brd children
  where
    children :: [Rose Board]
    children = map (gameTree (nextPlayer player)) (moves player brd)


-- | Game complexity

-- Exercise 11

gameTreeComplexity :: Int
gameTreeComplexity = leaves (gameTree P1 emptyBoard)

-- | Minimax

-- Exercise 12

minimax :: Player -> Rose Board -> Rose Int
minimax player (MkRose brd [])     | hasWinner brd == Just player = MkRose 1 []
                                   | hasWinner brd == Nothing = MkRose 0 []
                                   | otherwise = MkRose -1 []
minimax player (MkRose brd (x:xs)) =

-- * Lazier minimum and maximums

-- Exercise 13

minimum' :: [Int] -> Int
minimum' = undefined

maximum' :: [Int] -> Int
maximum' = undefined

-- | Gameplay

-- Exercise 14

makeMove :: Player -> Board -> Maybe Board
makeMove = undefined

-- | Main

data PlayerType = Human | Computer

instance Show PlayerType where
    show Human    = "H"
    show Computer = "C"

main :: IO ()
main = do
    typeOfP1 <- askFor "Should Player 1 be a (H)uman or a (C)omputer player?"
                       [Human, Computer]
    typeOfP2 <- askFor "Should Player 2 be a (H)uman or a (C)omputer player?"
                       [Human, Computer]

    let playerType :: Player -> PlayerType
        playerType P1 = typeOfP1
        playerType P2 = typeOfP2

        gameLoop :: Player -> Board -> IO ()
        gameLoop p b = do
            putStrLn ("\n" ++ printBoard b)
            case hasWinner b of
                Just p  -> putStrLn (show p ++ " has won!")
                Nothing -> do
                    putStr   ("It's " ++ show p ++ "'s turn. ")
                    mb' <- case playerType p of
                        Human    -> humanMove    p b
                        Computer -> computerMove p b
                    case mb' of
                        Nothing -> do putStr   "No more moves are possible. "
                                      putStrLn "It's a draw."
                        Just b' -> gameLoop (nextPlayer p) b'

        humanMove :: Player -> Board -> IO (Maybe Board)
        humanMove p b =
            case moves p b of
              [] -> return Nothing
              possibleMoves -> do
                putStrLn "Possible moves are:"
                putStrLn (listMoves possibleMoves)
                i <- askFor "Make your choice:" [1..length possibleMoves]
                return (Just (possibleMoves !! (i-1)))

        computerMove :: Player -> Board -> IO (Maybe Board)
        computerMove p b = do
            putStrLn "Thinking..."
            return (makeMove p b)

        listMoves :: [Board] -> String
        listMoves = intercalate "\n"
                    . map (intercalate "    ")
                    . transpose
                    . map lines
                    . map (\(i,b) -> "(" ++ show i ++ "): \n" ++ printBoard b)
                    . zip [1 :: Integer ..]

    gameLoop P1 emptyBoard

askFor :: Show a => String -> [a] -> IO a
askFor m xs = do
    putStr (m ++ " ")
    hFlush stdout
    i <- getLine
    case find ((map toLower i ==) . map toLower . show) xs of
        Nothing -> do putStrLn $ "I didn't understand you. Enter one of: "
                                 ++ intercalate ", " (map show xs) ++ "."
                      askFor m xs
        Just y  -> return y
