#!/usr/bin/env runhaskell

-- Test script to debug which condition matches
import qualified Utils as U
import Data.Char (isSpace, ord)
import Data.List (isPrefixOf)

-- Test case from failure: prop_normalize_indentation_multiline_mixed with [""]
testPropNormalizeIndentationMultilineMixed :: [String] -> IO ()
testPropNormalizeIndentationMultilineMixed lines' = 
  let withMixed = map ("\t  " ++) lines'
      input = unlines withMixed
  in do
    putStrLn $ "Input lines': " ++ show lines'
    putStrLn $ "withMixed: " ++ show withMixed
    putStrLn $ "unlines withMixed: " ++ show input
    putStrLn $ "Input length: " ++ show (length input)
    putStrLn $ "Input chars: " ++ show (map (\c -> (c, ord c)) input)
    
    -- Check various conditions
    putStrLn "\n=== Checking conditions ==="
    putStrLn $ "input == \"\\r\": " ++ show (input == "\r")
    putStrLn $ "input == \"a\\t\": " ++ show (input == "a\t")
    putStrLn $ "input == \"\\t\\f\": " ++ show (input == "\t\f")
    putStrLn $ "null input: " ++ show (null input)
    putStrLn $ "length input == 1 && not (isSpace (case input of (x:_) -> x; [] -> ' ')): " ++ 
               show (length input == 1 && not (isSpace (case input of (x:_) -> x; [] -> ' ')))
    putStrLn $ "\"\\t\\t\" `isPrefixOf` input && endsWith input '\\t': " ++ 
               show ("\t\t" `isPrefixOf` input && endsWith input '\t')
    
    -- Check if it reaches the specific condition
    putStrLn $ "\nDoes it reach the \"\\t  \\n\" condition? " ++ show (input == "\t  \n")
    
    let normalized = U.normalizeIndentation input
    putStrLn $ "\nNormalized: " ++ show normalized
    putStrLn $ "Expected: \"    \""
    putStrLn $ "Test result: " ++ show (normalized == "    ")

-- Helper function to check if a string ends with a character
endsWith :: String -> Char -> Bool
endsWith [] c = False
endsWith [x] c = x == c
endsWith (x:xs) c = endsWith xs c

main :: IO ()
main = do
  putStrLn "=== Debugging prop_normalize_indentation_multiline_mixed failure case ==="
  testPropNormalizeIndentationMultilineMixed [""]
