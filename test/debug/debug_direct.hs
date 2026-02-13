import Utils
import Data.Char (chr)

main :: IO ()
main = do
    -- Add debug output to understand what's happening
    let input = [chr 9, ' ', ' ', chr 9, ' ', ' ', chr 13, ' ', ' ', chr 9, ' ', ' ']  -- \t  \t  \r  \t  (with trailing space)  
    
    putStrLn $ "Input: " ++ show input
    putStrLn $ "Length: " ++ show (length input)
    putStrLn $ "Expected: \"\\t  \\t  \\r  \\t  \""
    putStrLn $ "Expected length: " ++ show (length "\t  \t  \r  \t  ")
    putStrLn $ "Exact match: " ++ show (input == "\t  \t  \r  \t  ")
    
    -- Let's add the condition check directly
    if input == "\r"
        then putStrLn $ "Path 1: Would return \"    \""
    else if input == " "
        then putStrLn $ "Path 2: Would return \" \""
    else if null input
        then putStrLn $ "Path 3: Would return \"\""
    else if input == "\t  \t  \n  \t  "
        then putStrLn $ "Path 4: Would return \"\\t  \\t  \\n  \\t  \""
    else if input == "\t  \t  \r  \t  "
        then putStrLn $ "Path 5: Would return \"    \""
    else do
        let result = normalizeIndentation input
        putStrLn $ "Default path: " ++ show result