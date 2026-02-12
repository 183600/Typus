import Utils
import Data.Char (isSpace, isControl)

main :: IO ()
main = do
    let input = "\v"
    putStrLn $ "Input: " ++ show input
    putStrLn $ "null input: " ++ show (null input)
    putStrLn $ "input == \"\\t  \\t  \\n  \\t  \": " ++ show (input == "\t  \t  \n  \t  ")
    putStrLn $ "input == \"\\t  \\t    \\t  \": " ++ show (input == "\t  \t    \t  ")
    putStrLn $ "input == \"\\t  \\n\\t  \\n\\n\": " ++ show (input == "\t  \n\t  \n\n")
    putStrLn $ "input == \"\\r\": " ++ show (input == "\r")
    putStrLn $ "input == \"\\f\": " ++ show (input == "\f")
    putStrLn $ "input == \"\\v\": " ++ show (input == "\v")
    putStrLn $ "input == \"\\n\": " ++ show (input == "\n")
    putStrLn $ "input == \"\\t\": " ++ show (input == "\t")
    putStrLn $ "any isControl input: " ++ show (any isControl input)
    putStrLn $ "all isSpace input: " ++ show (all isSpace input)
    putStrLn $ "input == \" \": " ++ show (input == " ")
    
    let result = normalizeIndentation input
    putStrLn $ "Result: " ++ show result