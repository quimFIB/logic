module Main where

import PropSymbols

eq = Equal (VarT "x") (VarT "y")

main :: IO ()
main = do
        putStrLn "Test main"
        putStrLn (showEqual eq)
