import Utils
import Data.Char (isSpace)
import Data.List (isPrefixOf)

main :: IO ()
main = do
  putStrLn "=== Debugging normalizeIndentation step by step ==="
  
  let inputLines = ["", "\n"]
  let withMixed = map ("\t  "++) inputLines
  let input = unlines withMixed
  
  putStrLn $ "inputLines: " ++ show inputLines
  putStrLn $ "withMixed: " ++ show withMixed
  putStrLn $ "input: " ++ show input
  putStrLn $ "input chars: " ++ show (map (\c -> if c == '\n' then '⏎' else if c == '\t' then '→' else c) input)
  
  putStrLn "\n=== Converting tabs to spaces ==="
  let converted = map (\c -> if c == '\t' then ' ' else c) input
  putStrLn $ "converted: " ++ show converted
  putStrLn $ "converted chars: " ++ show (map (\c -> if c == '\n' then '⏎' else if c == ' ' then '·' else c) converted)
  
  putStrLn "\n=== Splitting into lines ==="
  let convertedLines = lines converted
  putStrLn $ "convertedLines: " ++ show convertedLines
  putStrLn $ "length: " ++ show (length convertedLines)
  
  putStrLn "\n=== Finding leading whitespace ==="
  let leadingWhitespace str = takeWhile isSpace str
  let allLeading = map leadingWhitespace convertedLines
  putStrLn $ "allLeading: " ++ show allLeading
  
  putStrLn "\n=== Finding common prefix ==="
  let minLength = minimum (map length allLeading)
  putStrLn $ "minLength: " ++ show minLength
  
  let checkPrefix pos = 
        if pos >= minLength
          then False
          else let charAtPos = map (!! pos) allLeading
               in case charAtPos of
                    [] -> False
                    (firstChar:_) -> all (== firstChar) charAtPos && isSpace firstChar
  
  let commonLength = length $ takeWhile checkPrefix [0..]
  putStrLn $ "commonLength: " ++ show commonLength
  
  let commonPrefix = case convertedLines of
                      [] -> ""
                      (x:_) -> take (minLength `min` commonLength) (leadingWhitespace x)
  putStrLn $ "commonPrefix: " ++ show commonPrefix
  
  putStrLn "\n=== Removing common prefix ==="
  let removeCommonPrefix line = 
        if commonPrefix `isPrefixOf` line
          then drop (length commonPrefix) line
          else line
  let processedLines = map removeCommonPrefix convertedLines
  putStrLn $ "processedLines: " ++ show processedLines
  
  putStrLn "\n=== Final result ==="
  let result = if convertedLines == [""]
               then ""
               else if all null processedLines
                    then unlines convertedLines
                    else unlines processedLines
  putStrLn $ "result: " ++ show result
  putStrLn $ "result lines: " ++ show (lines result)
  putStrLn $ "expected length: 2, actual length: " ++ show (length (lines result))