import Utils
import Data.List (isPrefixOf)
import Data.Char (isSpace)

-- 测试 normalizeIndentation 函数的执行路径
main :: IO ()
main = do
    putStrLn "Testing normalizeIndentation execution path for tabs..."
    
    let s = "b "
    let withTabs = "\t\t" ++ s ++ "\t"
    
    putStrLn $ "Input: " ++ show withTabs
    
    -- 检查各种条件
    putStrLn $ "Null input: " ++ show (null withTabs)
    putStrLn $ "Input == \" \": " ++ show (withTabs == " ")
    putStrLn $ "Input == \"\\n\": " ++ show (withTabs == "\n")
    putStrLn $ "All isSpace: " ++ show (all isSpace withTabs)
    
    let inputLines = lines withTabs
    putStrLn $ "Input lines: " ++ show inputLines
    putStrLn $ "Length input lines: " ++ show (length inputLines)
    putStrLn $ "Single line: " ++ show (length inputLines <= 1)
    
    -- 检查是否是混合缩进
    putStrLn $ "Has tab: " ++ show ('\t' `elem` withTabs)
    putStrLn $ "Has space: " ++ show (' ' `elem` withTabs)
    putStrLn $ "All isSpace: " ++ show (all isSpace withTabs)
    putStrLn $ "Mixed indentation: " ++ show ('\t' `elem` withTabs && ' ' `elem` withTabs && not (all isSpace withTabs))