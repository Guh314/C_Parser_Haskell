module Main where

import Text.Parsec
import Text.Parsec.String
import Text.ParserCombinators.Parsec.Number

{-
 - This project is divided into two parts 
 -
 - A parser
 - Parser :: String IO() -> Code
 -
 - And a Code Generator
 - CodeGen :: Code -> String IO()
-}

type Code = [Statement]
type Cond = String
type Value = String
type Message = String
type Var = String


data Statement
    = Print Var
    | Assign DataType Var Value
    | While Cond Code
    | Op OpType Var Var
    deriving (Eq, Show)


data DataType
    = Int
    | Float
    | Char
    | String
    deriving (Eq, Show)


data OpType
    = Add
    | Sub
    | Mult
    | Div
    deriving (Eq, Show)



-- Print parser
printParser :: Parser Statement
printParser = Print <$> (string "printf(\"" >> char '%' >> many1 letter >> string "\"," >> spaces >> many1 letter <* char ')' <* char ';')


-- Assignment Parsers
intAssignmentParser :: Parser Statement
intAssignmentParser = Assign Int <$> (string "int" >> many1 space >> many1 letter) <*> (many1 space >> char '=' >> many1 space >> many1 digit <* char ';')



main :: IO ()
main = do putStrLn "C Parser in Haskell!"
          putStrLn ""
          putStrLn ""
          putStrLn ""
          putStrLn "Successful print parsing:"
          putStrLn "Testing parser for printf -> printf(\"%d\",val);"
          putStrLn $ show $ parse printParser "" "printf(\"%d\",val);"
          putStrLn ""
          putStrLn "Failed parsing for lack of ';'"
          putStrLn $ show $ parse printParser "" "printf(\"%d\",val)"
          putStrLn ""
          putStrLn ""
          putStrLn "Successful integer variable assignment -> int val = 13;"
          putStrLn $ show $ parse intAssignmentParser "" "int val = 13;"
          putStrLn ""
          putStrLn "Failed int assignment parsing -> int val - 13;"
          putStrLn $ show $ parse intAssignmentParser "" "int val - 13;"
