import Utils
import Data.List (isPrefixOf, isInfixOf)
import Data.Char (isSpace)

main :: IO ()
main = do
  let input = "    if condition {\n        // do something\n        return \n    }\n"
  let inputLines = lines input
  
  putStrLn $ "Input: " ++ show input
  putStrLn $ "Input lines: " ++ show inputLines
  
  -- Check if it's detected as a code block
  let isCodeBlock = any (`isInfixOf` input) ["if condition", "func outer", "func inner", "return", "{", "}", "//"]
  putStrLn $ "Is code block: " ++ show isCodeBlock
  
  -- Calculate common prefix
  let leadingWhitespace str = takeWhile isSpace str
  let allLeading = map leadingWhitespace inputLines
  let minLength = minimum (map length allLeading)
  
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
  let result = unlines processedLines
  
  putStrLn $ "Processed lines: " ++ show processedLines
  putStrLn $ "Result: " ++ show result
  
  -- Test with actual function
  let normalized = Utils.normalizeIndentation input
  putStrLn $ "Actual normalized: " ++ show normalized