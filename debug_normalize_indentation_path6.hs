import Utils
import Data.Char (isSpace, isPrint)

-- 测试 normalizeIndentation 函数的执行路径
main :: IO ()
main = do
    putStrLn "Testing normalizeIndentation execution path..."
    
    let input = "\t  \t  \v  \t  "
    
    putStrLn $ "Input: " ++ show input
    putStrLn $ "Null input: " ++ show (null input)
    putStrLn $ "All isSpace: " ++ show (all isSpace input && not (null input))
    putStrLn $ "Any non-printable: " ++ show (any (\c -> not (isPrint c) && c `notElem` "\n\r\t " && fromEnum c < 128 && c /= '\f') input)
    
    let normalized = normalizeIndentation input
    putStrLn $ "Normalized: " ++ show normalized