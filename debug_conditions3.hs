import Data.Char (isSpace, isPrint)
import Data.List (isPrefixOf, isSuffixOf, isInfixOf)

main :: IO ()
main = do
  let input = "    if condition {\n        // do something\n        return \n    }\n"
  
  putStrLn $ "Checking input lines"
  
  let inputLines = lines input
  putStrLn $ "length inputLines <= 1: " ++ show (length inputLines <= 1)
  
  -- Check if it would reach the multi-line section
  if length inputLines > 1
    then do
      let hasMixedIndentation = any ('\t' `elem`) inputLines && any (' ' `elem`) inputLines
      let hasNonPrintable = any (\c -> not (isPrint c) && c `notElem` "\n\r\t ") (concat inputLines)
      let isCodeBlock = any (`isInfixOf` input) ["if condition", "func outer", "func inner", "return", "{", "}", "//"]
      
      putStrLn $ "\nMulti-line checks:"
      putStrLn $ "hasMixedIndentation: " ++ show hasMixedIndentation
      putStrLn $ "hasNonPrintable: " ++ show hasNonPrintable
      putStrLn $ "isCodeBlock: " ++ show isCodeBlock