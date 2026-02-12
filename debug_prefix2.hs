import Data.List (isPrefixOf)
import Data.Char (isSpace)

main :: IO ()
main = do
  let inputLines = ["    if condition {", "        // do something", "        return ", "    }"]
  
  -- Calculate common prefix
  let leadingWhitespace str = takeWhile isSpace str
  let allLeading = map leadingWhitespace inputLines
  let minLength = minimum (map length allLeading)
  
  putStrLn $ "Input lines: " ++ show inputLines
  putStrLn $ "Leading whitespace: " ++ show allLeading
  putStrLn $ "Min length: " ++ show minLength
  
  let checkPrefix pos = 
        if pos >= minLength
          then False
          else let charAtPos = map (!! pos) allLeading
               in case charAtPos of
                    [] -> False
                    (firstChar:_) -> all (== firstChar) charAtPos && isSpace firstChar
  
  let commonLength = length $ takeWhile checkPrefix [0..]
  let commonPrefix = take (minLength `min` commonLength) (leadingWhitespace (head inputLines))
  
  putStrLn $ "Common length: " ++ show commonLength
  putStrLn $ "Common prefix: " ++ show commonPrefix
  
  -- Remove common prefix
  let removeCommonPrefix line = 
        if commonPrefix `isPrefixOf` line
          then drop (length commonPrefix) line
          else line
  
  let processedLines = map removeCommonPrefix inputLines
  putStrLn $ "Processed lines: " ++ show processedLines
  
  -- Check if all lines start with the common prefix
  let allStartWithPrefix = all (commonPrefix `isPrefixOf`) inputLines
  putStrLn $ "All lines start with common prefix: " ++ show allStartWithPrefix
  
  -- Check each line individually
  mapM_ (\line -> putStrLn $ "Line " ++ show line ++ " starts with prefix: " ++ show (commonPrefix `isPrefixOf` line)) inputLines