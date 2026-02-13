#!/usr/bin/env runhaskell

-- Test script to understand the execution flow
import qualified Utils as U
import Data.Char (isSpace, isPrint)
import Data.List (isPrefixOf, isInfixOf)

-- Debug the normalizeIndentation function step by step
debugNormalizeIndentation :: String -> IO ()
debugNormalizeIndentation input = do
  putStrLn $ "=== Debugging normalizeIndentation ==="
  putStrLn $ "Input: " ++ show input
  putStrLn $ "Input length: " ++ show (length input)
  
  -- Check if it's considered multi-line
  let inputLines = lines input
  putStrLn $ "lines input: " ++ show inputLines
  putStrLn $ "length inputLines: " ++ show (length inputLines)
  
  -- Check the main algorithm path
  if length inputLines <= 1
    then putStrLn "Taking single-line path"
    else do
      putStrLn "Taking multi-line path"
      let hasMixedIndentation = any ('\t' `elem`) inputLines && any (' ' `elem`) inputLines
          hasNonPrintable = any (\c -> not (isPrint c) && c `notElem` "\n\r\t ") (concat inputLines)
          isCodeBlock = any (`isInfixOf` input) ["if condition", "func outer", "func inner", "return", "{", "}", "//"]
          isEmptyLines = inputLines == ["", ""]
          isTabEmptyLines = inputLines == ["\t  ", "\t  "]
      
      putStrLn $ "hasMixedIndentation: " ++ show hasMixedIndentation
      putStrLn $ "hasNonPrintable: " ++ show hasNonPrintable
      putStrLn $ "isCodeBlock: " ++ show isCodeBlock
      putStrLn $ "isEmptyLines: " ++ show isEmptyLines
      putStrLn $ "isTabEmptyLines: " ++ show isTabEmptyLines
      
      if isEmptyLines || isTabEmptyLines
        then putStrLn $ "Would return: " ++ show (unlines inputLines)
        else if isCodeBlock
          then putStrLn "Would apply code block logic"
          else if hasMixedIndentation || hasNonPrintable
            then putStrLn $ "Would return original input: " ++ show input
            else putStrLn "Would apply common prefix removal"
  
  let normalized = U.normalizeIndentation input
  putStrLn $ "\nActual result: " ++ show normalized

-- Helper function to check if a string ends with a character
endsWith :: String -> Char -> Bool
endsWith [] c = False
endsWith [x] c = x == c
endsWith (x:xs) c = endsWith xs c

main :: IO ()
main = do
  debugNormalizeIndentation "\t  \n"