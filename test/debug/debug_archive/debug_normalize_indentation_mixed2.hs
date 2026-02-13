import Utils
import Data.Char (isSpace)

main :: IO ()
main = do
    let s = ""
    let mixed = "\t  \t  " ++ s ++ "  \t  "
    let normalized = normalizeIndentation mixed
    
    putStrLn $ "Input string: " ++ show mixed
    putStrLn $ "Input string length: " ++ show (length mixed)
    putStrLn $ "All isSpace: " ++ show (all isSpace mixed)
    
    let inputLines = lines mixed
    putStrLn $ "Input lines: " ++ show inputLines
    putStrLn $ "Number of lines: " ++ show (length inputLines)
    
    case inputLines of
        [line] -> do
            putStrLn $ "Single line case"
            putStrLn $ "Line content: " ++ show line
            putStrLn $ "All isSpace line: " ++ show (all isSpace line)
        _ -> putStrLn $ "Not a single line"
    
    putStrLn $ "Output: " ++ show normalized
    putStrLn $ "Expected: " ++ show "    "
    putStrLn $ "Test passes: " ++ show (normalized == "    ")