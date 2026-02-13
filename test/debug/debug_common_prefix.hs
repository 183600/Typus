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
      
  putStrLn $ "s: " ++ show s
  putStrLn $ "codeBlock: " ++ show codeBlock
  putStrLn $ "inputLines: " ++ show inputLines
  putStrLn $ "allLeading: " ++ show allLeading
  putStrLn $ "minLength: " ++ show minLength
  
  -- Check each position
  let checkPrefix pos = 
        if pos >= minLength
          then False
          else let charAtPos = map (!! pos) allLeading
               in case charAtPos of
                    [] -> False
                    (firstChar:_) -> all (== firstChar) charAtPos && isSpace firstChar
  
  putStrLn $ "checkPrefix results: " ++ show [checkPrefix pos | pos <- [0..minLength-1]]
  
  -- Find common prefix length
  let commonLength = length $ takeWhile checkPrefix [0..]
  putStrLn $ "commonLength: " ++ show commonLength
  
  -- Find common prefix
  let commonPrefix = case inputLines of
                        [] -> ""
                        (x:_) -> take (minLength `min` commonLength) (leadingWhitespace x)
  putStrLn $ "commonPrefix: " ++ show (show commonPrefix)