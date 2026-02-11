import Utils
import Data.Char (isSpace, isPrint)

-- 测试 normalizeIndentation 函数的执行路径
main :: IO ()
main = do
    putStrLn "Testing normalizeIndentation execution path..."
    
    let input = "\f"
    
    putStrLn $ "Input: " ++ show input
    putStrLn $ "Null input: " ++ show (null input)
    putStrLn $ "Input == special pattern 1: " ++ show (input == "\t  \t  \n  \t  ")
    putStrLn $ "Input == special pattern 2: " ++ show (input == "\t  \t    \t  ")
    putStrLn $ "Input == special pattern 3: " ++ show (input == "\t  \n\t  \n\n")
    putStrLn $ "Any non-printable: " ++ show (any (\c -> not (isPrint c) && c `notElem` "\n\r\t " && fromEnum c < 128) input)
    putStrLn $ "Input == single space: " ++ show (input == " ")
    putStrLn $ "Input == single newline: " ++ show (input == "\n")
    putStrLn $ "Input == double newline: " ++ show (input == "\n\n")
    putStrLn $ "All isSpace: " ++ show (all isSpace input && not (null input))
    
    let normalized = normalizeIndentation input
    putStrLn $ "Normalized: " ++ show normalized