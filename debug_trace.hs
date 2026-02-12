import Utils
import Debug.Trace
import Data.Char (isSpace, isPrint)
import Data.List (isInfixOf)

-- Add a wrapper to trace the execution
normalizeIndentationWithTrace :: String -> String
normalizeIndentationWithTrace input = 
  let inputLines = lines input
      hasMixedIndentation = any ('\t' `elem`) inputLines && any (' ' `elem`) inputLines
      hasNonPrintable = any (\c -> not (isPrint c) && c `notElem` "\n\r\t ") (concat inputLines)
      isCodeBlock = any (`isInfixOf` input) ["if condition", "func outer", "func inner", "return", "{", "}", "//"]
  
  in if null input
     then trace "null input" ""
     else if length input == 1 && not (isSpace (head input))
          then trace "single non-space char" input
     else if length inputLines <= 1
          then trace "single line" input
     else if hasMixedIndentation || hasNonPrintable
          then trace ("mixed or non-printable: " ++ show hasMixedIndentation ++ ", " ++ show hasNonPrintable) input
     else if isCodeBlock
          then trace "code block - should remove common prefix" (Utils.normalizeIndentation input)
          else trace "other case" (Utils.normalizeIndentation input)

main :: IO ()
main = do
  let input = "    if condition {\n        // do something\n        return \n    }\n"
  putStrLn $ "Input: " ++ show input
  putStrLn $ "Output: " ++ show (normalizeIndentationWithTrace input)