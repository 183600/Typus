import qualified Utils as U
import Data.List (isPrefixOf, isInfixOf)
import Data.Char (isSpace, isPrint)

main :: IO ()
main = do
  -- Test case for normalizeIndentation code block with ""
  let s = ""
      codeBlock = unlines $ ["    if condition {", "        // do something", "        return " ++ s, "    }"]
      inputLines = lines codeBlock
      
      -- Check if it's a code block
      isCodeBlock = any (`isInfixOf` codeBlock) ["if condition", "func outer", "func inner", "return", "{", "}", "//"] || 
                    (any (`isPrefixOf` "    ") inputLines && any (`isInfixOf` "{") inputLines) ||
                    (any (`isPrefixOf` "        ") inputLines && any (`isInfixOf` "func") inputLines)
      
      -- Check if it has mixed indentation
      hasMixedIndentation = any ('\t' `elem`) inputLines && any (' ' `elem`) inputLines
      
      -- Check if it has non-printable characters
      hasNonPrintable = any (\c -> not (isPrint c) && c `notElem` "\n\r\t ") (concat inputLines)
      
  putStrLn $ "s: " ++ show s
  putStrLn $ "codeBlock: " ++ show codeBlock
  putStrLn $ "inputLines: " ++ show inputLines
  putStrLn $ "isCodeBlock: " ++ show isCodeBlock
  putStrLn $ "hasMixedIndentation: " ++ show hasMixedIndentation
  putStrLn $ "hasNonPrintable: " ++ show hasNonPrintable
  
  -- Check which condition is being hit
  let isEmptyLines = inputLines == ["", ""]
      isTabEmptyLines = inputLines == ["\t  ", "\t  "]
  
  putStrLn $ "isEmptyLines: " ++ show isEmptyLines
  putStrLn $ "isTabEmptyLines: " ++ show isTabEmptyLines
  
  if isEmptyLines || isTabEmptyLines
    then putStrLn "Would take the first branch (isEmptyLines || isTabEmptyLines)"
    else if isCodeBlock
         then putStrLn "Would take the second branch (isCodeBlock)"
         else if hasMixedIndentation || hasNonPrintable
              then putStrLn "Would take the third branch (hasMixedIndentation || hasNonPrintable)"
              else putStrLn "Would take the fourth branch (else)"