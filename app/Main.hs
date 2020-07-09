module Main where

import Lib

eq = Equal (VarT "x") (VarT "y")

main :: IO ()
main = do
        putStrLn "Test main"
        putStrLn (showEqual eq)
