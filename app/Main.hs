module Main where

import RPN

main :: IO ()
main = do
    putStrLn "Enter Expression in Reverse Polish Notation:"
    input <- getLine
    let result = processExpr input
    putStrLn $ "Result = " ++ show result


