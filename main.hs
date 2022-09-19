module Main where

{- This is a framework in which all functions to be written are "undefined".  -
 - Note that in most cases parameters, pattern-matching and guards have been  -
 - omitted! You will have to add those yourself.                              -}

import Data.Char
import Data.List
import Data.Maybe

-- | Model

type Field = String
type Row   = [Field]
type Table = [Row]

-- | Main

main :: IO ()
main = interact (unlines . exercise . lines)

exercise :: [String] -> [String]
exercise = printTable
         . project ["last", "first", "salary"]
         . select "gender" "male"
         . parseTable

-- | Parsing

-- * Exercise 1
-- parseTable ["first last program money", "hank manney excel 10000"]
parseTable :: [String] -> Table
parseTable = map words

-- | Printing

-- * Exercise 2

-- | Return a string that represents a row delimiter
-- | input e.g. printLine [5, 5, 6]
printLine :: [Int] -> String
printLine []     = "+"
printLine (x:xs) = '+' : replicate x '-' ++ printLine xs

-- * Exercise 3

-- | Return a string that includes padding to either the left or the right, depending on the type.
-- | input e.g. printField 7 "1234"
printField :: Int -> String -> String
printField width content | all isDigit content = padField ++ content
                         | otherwise = content ++ padField
  where
    padField :: [Char]
    padField = replicate (width - length content) ' '

-- * Exercise 4

-- | Return a string that contains the data fields of a row, separated by '|'.
-- | input e.g. printRow [ (5, "Alice"),(6, "Allen"),(6, "female"),(6, "82000")]
printRow :: [(Int, String)] -> String
printRow fields = '|' : intercalate "|" (printFields fields) ++ "|"
  where
    printFields :: [(Int, String)] -> [String]
    printFields xs = map (uncurry printField) xs


-- * Exercise 5

-- | Return a list containing the maximum width of each column.
-- | input e.g. columnWidths [["Alice", "Allen", "female", "82000"], ["Bob", "Baker", "male", "70000"], ["Carol", "Clarke", "female", "50000"]]
columnWidths :: Table -> [Int]
columnWidths []  = []
columnWidths xss = map (maximum . map length) (transpose xss)

-- * Exercise 6

-- | Return a list containing the strings that represent the pretty print version of header, row and delimiter data.
-- | input e.g. printTable [["first", "last", "gender", "salary"], ["Alice", "Allen", "female", "82000"], ["Bob", "Baker", "male", "70000"], ["Carol", "Clarke", "female", "50000"]]
printTable :: Table -> [String]
printTable table = printTable' 0 table

printTable' :: Int -> Table -> [String]
printTable' _ []               = []
printTable' index table@(x:xs) | index == 0 = delimiter : (map toUpper row) : delimiter : printTable' (index + 1) xs
                               | otherwise = row : printTable' (index + 1) xs
  where
    widths = columnWidths table
    delimiter = printLine widths
    rowPairs = zip widths x
    row = printRow rowPairs

-- | Querying

-- * Exercise 7

-- | Apply a filter on the table that keeps rows that contain specified value in column
-- | input e.g. select "gender" "female" [["first", "last", "gender", "salary"], ["Alice", "Allen", "female", "82000"], ["Bob", "Baker", "male", "70000"], ["Carol", "Clarke", "female", "50000"]]
select :: Field -> Field -> Table -> Table
select column value table@(header:rows) = header : maybe rows select' (elemIndex column header)
  where
    -- | Check if element equals given predicate value, based on list index.
    predicate :: Int -> Field -> Row -> Bool
    predicate index value row = (row !! index) == value

    -- | Filter rows in the table that do not satisfy the predicate.
    select' :: Int -> Table
    select' index = filter (predicate index value) rows

-- * Exercise 8

project :: [Field] -> Table -> Table
project columns table@(header:_) =


{-
project ["last", "first", "salary"]

Todo∷
- non existent column name in input --> omit
- column name in table not in input --> omit
- sort columns

map on input
mapmaybe on table per input
predicate has !! --> nothing or filter?
transpose 2x

!!
elemIndex
maybeMap
map
transpose
-}
