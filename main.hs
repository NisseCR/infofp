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

-- | input e.g. exercise ["first last gender salary", "Alice Allen female 82000", "Bob Baker male 70000", "Carol Clarke female 50000"]
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
printTable table@(header : rows) = delimiter : map toUpper (rowToString header) : delimiter : map rowToString rows ++  delimiter : []
  where
    widths = columnWidths table
    delimiter = printLine widths

    rowToString :: Row -> String
    rowToString row = printRow (zip widths row)

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
-- | Sort columns, excluding those given in the input
-- | input e.g. project ["last", "salary", "gender", "testtesttest"] [["first", "last", "gender", "salary"], ["Alice", "Allen", "female", "82000"], ["Bob", "Baker", "male", "70000"], ["Carol", "Clarke", "female", "50000"]]
project :: [Field] -> Table -> Table
project columns table@(header:_) = transpose (map filterColumns columns)
  where
    tranposedTable = transpose table

    columnIndex :: Field -> Maybe Int
    columnIndex value = elemIndex value header

    filterColumns :: Field -> Row
    filterColumns columnConstraint = maybe [] (tranposedTable !!) (index)
      where
        index = columnIndex columnConstraint
