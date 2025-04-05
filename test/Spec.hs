module Main (main) where

import Test.HUnit
import RPN (processExpr)

-- Test cases
tests :: Test
tests = TestList
  [ "Simple addition" ~: processExpr "3 4 ADD" ~?= Just 7
  , "Nested operations" ~: processExpr "3 4 ADD 5 MUL" ~?= Just 35
  , "Unary NEG" ~: processExpr "5 NEG" ~?= Just (-5)
  , "Unary SQUARE" ~: processExpr "3 SQUARE" ~?= Just 9
  , "Division" ~: processExpr "10 2 DIV" ~?= Just 5
  , "Invalid operator" ~: processExpr "3 4 FOO" ~?= Nothing
  , "Not enough operands" ~: processExpr "ADD" ~?= Nothing
  , "Empty string" ~: processExpr "" ~?= Nothing
  , "Extra operands" ~: processExpr "1 2 3 ADD" ~?= Nothing
  ]

main :: IO ()
main = do
  _ <- runTestTT tests
  return ()