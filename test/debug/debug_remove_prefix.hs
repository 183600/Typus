import qualified Utils as U
import Data.List (isPrefixOf, isInfixOf)
import Data.Char (isSpace)

main :: IO ()
main = do
  -- Test case for normalizeIndentation code block with ""
  let s = ""
      codeBlock = unlines $ ["    if condition {", "        // do something", "        return " ++ s, "    }"]
      inputLines = lines codeBlock
      leadingWhitespace str = takeWhile isSpace str
      allLeading = map leadingWhitespace inputLines
      minLength = minimum (map length allLeading)
      
      -- Find common prefix
      checkPrefix pos = 
        if pos >= minLength
          then False
          else let charAtPos = map (!! pos) allLeading
               in case charAtPos of
                    [] -> False
                    (firstChar:_) -> all (== firstChar) charAtPos && isSpace firstChar
      
      commonLength = length $ takeWhile checkPrefix [0..]
      commonPrefix = case inputLines of
                        [] -> ""
                        (x:_) -> take (minLength `min` commonLength) (leadingWhitespace x)
      
      -- Remove common prefix
      removeCommonPrefix line = 
        if commonPrefix `isPrefixOf` line
          then drop (length commonPrefix) line
          else line
      
      processedLines = map removeCommonPrefix inputLines
      
  putStrLn $ "s: " ++ show s
  putStrLn $ "codeBlock: " ++ show codeBlock
  putStrLn $ "commonPrefix: " ++ show commonPrefix
  putStrLn $ "processedLines: " ++ show processedLines
  putStrLn $ "unlines processedLines: " ++ show (unlines processedLines)
  
  -- Check the result
  let normalized = U.normalizeIndentation codeBlock
      normLines = lines normalized
      nonCommentLines = filter (not . isPrefixOf "//") normLines
  putStrLn $ "normalized: " ++ show normalized
  putStrLn $ "normLines: " ++ show normLines
  putStrLn $ "Lines with 4 spaces prefix: " ++ show (length (filter (isPrefixOf "    ") normLines))
  putStrLn $ "Total lines: " ++ show (length normLines)
  putStrLn $ "Test passes: " ++ show (length (filter (isPrefixOf "    ") normLines) < length normLines)