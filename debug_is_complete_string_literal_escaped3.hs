-- Trace through isCompleteStringLiteral for "\"\"\\\"\""
import Data.Char (isSpace)

main :: IO ()
main = do
    let escaped = "\"\"\\\"\""
    putStrLn $ "Input: " ++ show escaped
    putStrLn $ "First char: " ++ show (escaped !! 0)
    putStrLn $ "Rest: " ++ show (tail escaped)
    
    -- The function will call hasClosingQuote '"' "\"\\\"\""
    let quote = '"'
    let str' = tail escaped  -- "\"\\\"\""
    
    putStrLn $ "\nCalling hasClosingQuote '" ++ show quote ++ "' " ++ show str'
    
    -- hasClosingQuote will call go "\"\\\"\"" 0
    putStrLn $ "Calling go " ++ show str' ++ " 0"
    
    -- go will process the first character '"'
    let x = str' !! 0  -- '"'
    let xs = tail str'  -- "\\\"\""
    putStrLn $ "First character: " ++ show x
    putStrLn $ "Rest: " ++ show xs
    
    -- Since x == quote, it checks if backslashCount is odd
    let backslashCount = 0
    putStrLn $ "Backslash count: " ++ show backslashCount
    putStrLn $ "Is backslashCount odd? " ++ show (odd backslashCount)
    
    -- Since backslashCount is even, it checks the remaining characters
    putStrLn $ "Remaining characters: " ++ show xs
    putStrLn $ "Are all remaining characters spaces? " ++ show (all isSpace xs)
    
    -- The result should be False because the remaining characters are not all spaces
    let result = all isSpace xs
    putStrLn $ "Result: " ++ show result