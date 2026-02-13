import qualified Utils as U
import Data.List (isInfixOf)

main :: IO ()
main = do
  -- Test case for normalizeIndentation code block with ""
  let s = ""
      codeBlock = unlines $ ["    if condition {", "        // do something", "        return " ++ s, "    }"]
      normalized = U.normalizeIndentation codeBlock
  putStrLn $ "s: " ++ show s
  putStrLn $ "codeBlock: " ++ show codeBlock
  putStrLn $ "normalized: " ++ show normalized
  
  -- Check if it's detected as a code block
  let inputLines = lines codeBlock
      isCodeBlock = any (`isInfixOf` codeBlock) ["if condition", "func outer", "func inner", "return", "{", "}", "//"]
  putStrLn $ "isCodeBlock: " ++ show isCodeBlock
  putStrLn $ "inputLines: " ++ show inputLines
  
  -- Check leading whitespace
  let leadingWhitespace str = takeWhile (\c -> c == ' ' || c == '\t') str
      allLeading = map leadingWhitespace inputLines
  putStrLn $ "allLeading: " ++ show allLeading
