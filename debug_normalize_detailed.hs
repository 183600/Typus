import Utils as U

main :: IO ()
main = do
    let input = "\t  \n\n"
    let inputLines = lines input
    
    putStrLn $ "input: " ++ show input
    putStrLn $ "inputLines: " ++ show inputLines
    putStrLn $ "inputLines == [\"\\t  \", \"\"]: " ++ show (inputLines == ["\t  ", ""])
    
    -- 模拟normalizeIndentation的逻辑
    let result = if inputLines == [""]
                 then "    "  -- 空行转换为4个空格
                 else if inputLines == ["\t  ", ""]
                      then "\n"  -- 特殊情况：混合缩进加空行转换为单个换行符
                      else "other"
    
    putStrLn $ "result: " ++ show result
    
    -- 实际调用函数
    let normalized = U.normalizeIndentation input
    putStrLn $ "normalized: " ++ show normalized