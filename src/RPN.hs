module RPN
  ( processExpr
  ) where

import Data.Char (toUpper)
import Control.Monad (foldM, guard)

type Operator = String

unaryOperators :: [String]
unaryOperators = ["NEG", "SQUARE"]

binaryOperators :: [String]
binaryOperators = ["ADD", "SUB", "MUL", "DIV"]

isOperator :: String -> Bool
isOperator op = cOp `elem` operators
  where
    operators = unaryOperators ++ binaryOperators
    cOp = map toUpper op

operatorArity :: Operator -> Int
operatorArity op
  | op `elem` unaryOperators = 1
  | op `elem` binaryOperators = 2
  | otherwise = undefined

dispatch :: Operator -> [Int] -> Int
dispatch op = case op of
  "NEG"    -> negate . head
  "SQUARE" -> \xs -> head xs * head xs
  "ADD"    -> sum
  "SUB"    -> \xs -> head xs - last xs
  "MUL"    -> product
  "DIV"    -> \xs -> head xs `div` last xs
  _        -> undefined

eval :: [Int] -> Operator -> Maybe Int
eval stack op
  | length stack /= operatorArity cOp = Nothing
  | otherwise = Just computeExpr
  where
    cOp = map toUpper op
    computeExpr = dispatch op stack

processExpr :: String -> Maybe Int
processExpr "" = Nothing
processExpr s = case foldM step [] (words s) of
  Just [result] -> Just result
  _ -> Nothing
  where
    step :: [Int] -> String -> Maybe [Int]
    step stack token
      | isOperator token = do
          let arity = operatorArity token
          guard (length stack >= arity)
          let (args, rest) = splitAt arity stack
          result <- eval (reverse args) token
          return (result : rest)
      | otherwise = case reads token of
          [(n, "")] -> Just (n : stack)
          _ -> Nothing
