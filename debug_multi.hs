import Data.Char (isSpace, isPrint)
import Data.List (isInfixOf)

main :: IO ()
main = do
  let input = "    if condition {\n        // do something\n        return \n    }\n"
  let inputLines = lines input
  
  let hasMixedIndentation = any ('\t' `elem`) inputLines && any (' ' `elem`) inputLines
  let hasNonPrintable = any (\c -> not (isPrint c) && c `notElem` "\n\r\t ") (concat inputLines)
  let isCodeBlock = any (`isInfixOf` input) ["if condition", "func outer", "func inner", "return", "{", "}", "//"]
  
  putStrLn $ "hasMixedIndentation: " ++ show hasMixedIndentation
  putStrLn $ "hasNonPrintable: " ++ show hasNonPrintable
  putStrLn $ "isCodeBlock: " ++ show isCodeBlock
  
  -- Check if it's pure spaces or tabs
  let hasOnlySpacesAndTabs = all (\c -> c == ' ' || c == '\t' || c == '\n') input
  putStrLn $ "hasOnlySpacesAndTabs: " ++ show hasOnlySpacesAndTabs
  putStrLn $ "any '\t' `elem` inputLines: " ++ show (any ('\t' `elem`) inputLines)
  putStrLn $ "any ' ' `elem` inputLines: " ++ show (any (' ' `elem`) inputLines)