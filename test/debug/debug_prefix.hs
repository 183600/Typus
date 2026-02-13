import Data.List (isPrefixOf)
import Data.Char (isSpace)

main :: IO ()
main = do
  let lines' = ["    if condition {", "        // do something", "        return ", "    }"]
  let leadingWhitespace str = takeWhile isSpace str
  let allLeading = map leadingWhitespace lines'
  putStrLn $ "Lines: " ++ show lines'
  putStrLn $ "Leading whitespace: " ++ show allLeading
  
  let minLength = minimum (map length allLeading)
  putStrLn $ "Min length: " ++ show minLength
  
  let checkPrefix pos = 
        if pos >= minLength
          then False
          else let charAtPos = map (!! pos) allLeading
               in case charAtPos of
                    [] -> False
                    (firstChar:_) -> all (== firstChar) charAtPos && isSpace firstChar
  
  let commonLength = length $ takeWhile checkPrefix [0..]
  putStrLn $ "Common length: " ++ show commonLength
  
  let commonPrefix = take (minLength `min` commonLength) (leadingWhitespace (head lines'))
  putStrLn $ "Common prefix: " ++ show (length commonPrefix) ++ " spaces: " ++ show commonPrefix