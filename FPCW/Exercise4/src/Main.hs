module Main where

import Ex4
import System.IO

testDofold :: IO ()
testDofold = do
  let intList = [1, 2, 3, 4, 5]
  let thingList = [([True, False], 3.0), ([False, True], 4.0)]  -- Assuming Thing is defined accordingly

  -- Test 1: Length Function
  putStrLn "Test 1: Length"
  print $ dofold lenTuple intList  -- Expected: length of intList

  -- Test 2: Summation Function
  putStrLn "Test 2: Summation"
  print $ dofold sumupTuple intList  -- Expected: sum of intList

  -- Test 3: Product Function
  putStrLn "Test 3: Product"
  print $ dofold prodTuple intList  -- Expected: product of intList

  -- Test 4: Concatenation Function
  putStrLn "Test 4: Concatenation"
  print $ dofold catTuple [thingList, thingList]  -- Expected: thingList concatenated with itself

testMdeval :: IO ()
testMdeval = do
  let dict = [("x", 10), ("y", 5), ("z", 0)]

  -- Test 1: Simple Value
  putStrLn "Test 1: Simple Value"
  printResult $ mdeval dict (Value 7)

  -- Test 2: Variable Lookup
  putStrLn "Test 2: Variable Lookup"
  printResult $ mdeval dict (Variable "x")
  printResult $ mdeval dict (Variable "a")

  -- Test 3: Division
  putStrLn "Test 3: Division"
  printResult $ mdeval dict (Div (Variable "x") (Variable "y"))
  printResult $ mdeval dict (Div (Variable "x") (Variable "z"))

  -- Test 4: Multiplication
  putStrLn "Test 4: Multiplication"
  printResult $ mdeval dict (MulBy (Variable "x") (Variable "y"))

  -- Test 5: Absolute Value
  putStrLn "Test 5: Absolute Value"
  printResult $ mdeval dict (AbsVal (Value (-7)))

  -- Test 6: Logical Not
  putStrLn "Test 6: Logical Not"
  printResult $ mdeval dict (Not (Variable "z"))
  printResult $ mdeval dict (Not (Variable "y"))

  -- Test 7: Equality
  putStrLn "Test 7: Equality"
  printResult $ mdeval dict (Eql (Variable "x") (Variable "y"))
  printResult $ mdeval dict (Eql (Variable "y") (Variable "y"))

  -- Test 8: IsNil
  putStrLn "Test 8: IsNil"
  printResult $ mdeval dict (IsNil (Variable "z"))
  printResult $ mdeval dict (IsNil (Variable "x"))

-- Helper function to print Maybe Float results
printResult :: Maybe Float -> IO ()
printResult (Just val) = print val
printResult Nothing = putStrLn "Error"

main :: IO ()
main = do

  putStrLn "Running tests for Q2 (dofold)"
  testDofold
  putStrLn "Tests for Q2 completed"
  
  putStrLn "Running tests for mdeval"
  testMdeval
  putStrLn "Tests for mdeval completed"

  -- Reading from 'input.dat'
  inputContent <- readFile "input.dat.txt"
  let numbers = map (fromIntegral . read) . lines $ inputContent :: [Integer]

  let opsCycled = cycle ops
  let results = zipWith ($) opsCycled numbers

  -- Writing to 'output.data'
  let outputContent = unlines . map show $ results
  writeFile "output.dat.txt" outputContent

  putStrLn "Processing complete. Results written to output.dat."

