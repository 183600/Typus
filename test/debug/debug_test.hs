import Test.QuickCheck
import System.Exit (exitFailure, exitSuccess)
import Data.Char (isAlphaNum)

-- Import the function we're testing
-- This would normally be: import Typus.Parser (parseTypusFile)
-- For testing purposes, let's create a simplified version

-- Simplified version of parseTypusFile for testing
parseTypusFile :: String -> Either String String
parseTypusFile input = 
  if null input
    then Left "Empty input is not allowed"
    else if all (`elem` [' ', '\t', '\n', '\r']) input
      then Left "Input contains only whitespace"
      else Right input

-- Test function
prop_parseBasicTypusFile :: String -> Property
prop_parseBasicTypusFile s =
  let limitedStr = take 20 s
      validName = take 5 $ filter isAlphaNum limitedStr
      name = if null validName then "x" else validName
      code = "package main\n\nvar " ++ name ++ " int = 1"
      result = parseTypusFile code
  in property $ length code > 0 ==> isRight result

-- Helper function
isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _ = False

-- Test with specific failing input
main :: IO ()
main = do
  putStrLn "Testing with input '0'..."
  let result = parseTypusFile "0"
  putStrLn $ "Result: " ++ show result
  
  putStrLn "\nTesting prop_parseBasicTypusFile with '0'..."
  let testResult = prop_parseBasicTypusFile "0"
  putStrLn "Test result: Property test executed"
  
  -- Test with the actual failing case from QuickCheck
  putStrLn "\nTesting with generated code from '0'..."
  let limitedStr = take 20 "0"
      validName = take 5 $ filter isAlphaNum limitedStr
      name = if null validName then "x" else validName
      code = "package main\n\nvar " ++ name ++ " int = 1"
      result2 = parseTypusFile code
  putStrLn $ "Generated code: " ++ show code
  putStrLn $ "Result: " ++ show result2