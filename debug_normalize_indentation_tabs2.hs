import Utils
import Data.List (isPrefixOf)
import Data.Char (isSpace)

main :: IO ()
main = do
    let input = "\t\t\n\t"
    let inputLines = lines input
    putStrLn $ "input = " ++ show input
    putStrLn $ "inputLines = " ++ show inputLines
    putStrLn $ "length inputLines = " ++ show (length inputLines)
    
    -- Check if it's considered single line or multi line
    if length inputLines <= 1
      then putStrLn "Goes to single line branch"
      else do
        putStrLn "Goes to multi line branch"
        let nonEmptyLines = filter (not . null) inputLines
        putStrLn $ "nonEmptyLines = " ++ show nonEmptyLines
        
        let allLinesEmptyOrWhitespace = all (\line -> null line || all isSpace line) inputLines
        putStrLn $ "allLinesEmptyOrWhitespace = " ++ show allLinesEmptyOrWhitespace