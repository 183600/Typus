import Utils
import Data.Char (isPrint)
import Data.List (isInfixOf)

-- Add more debug logging
main :: IO ()
main = do
  putStrLn "=== Testing with more debug ==="
  let testInput = ["\t  ", "\t  "]
  let input = unlines testInput
  putStrLn $ "Input lines: " ++ show testInput
  putStrLn $ "Input string: " ++ show input
  
  let inputLines = lines input
  putStrLn $ "After lines(): " ++ show inputLines
  
  let hasMixedIndentation = any ('\t' `elem`) inputLines && any (' ' `elem`) inputLines
  putStrLn $ "hasMixedIndentation: " ++ show hasMixedIndentation
  
  let hasNonPrintable = any (\c -> not (isPrint c) && c `notElem` "\n\r\t ") (concat inputLines)
  putStrLn $ "hasNonPrintable: " ++ show hasNonPrintable
  
  let isCodeBlock = any (`isInfixOf` input) ["if condition", "func outer", "func inner", "return", "{", "}", "//"]
  putStrLn $ "isCodeBlock: " ++ show isCodeBlock
  
  let isEmptyLines = inputLines == ["", ""]
  let isTabEmptyLines = inputLines == ["\t  ", "\t  "]
  putStrLn $ "isEmptyLines: " ++ show isEmptyLines
  putStrLn $ "isTabEmptyLines: " ++ show isTabEmptyLines
  
  let result = normalizeIndentation input
  putStrLn $ "Output: " ++ show result
  putStrLn $ "Output lines: " ++ show (lines result)
  putStrLn $ "Number of lines: " ++ show (length (lines result))