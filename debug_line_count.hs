import Utils
import Data.Char (isSpace)

main :: IO ()
main = do
    -- Test case: input = "\t  \n"
    let input = "\t  \n"
    let inputLines = lines input
    putStrLn $ "input = " ++ show input
    putStrLn $ "inputLines = " ++ show inputLines
    putStrLn $ "length inputLines = " ++ show (length inputLines)
    putStrLn $ "hasTrailingNewline = " ++ show (not (null input) && last input == '\n')
    
    -- Check if it's considered single line or multi line
    if length inputLines <= 1
      then putStrLn "Goes to single line branch"
      else putStrLn "Goes to multi line branch"