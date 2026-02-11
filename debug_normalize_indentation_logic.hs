import Utils
import Data.Char (isSpace)

main :: IO ()
main = do
    let withTabs = "\t\ta\t"
    let inputLines = lines withTabs
    putStrLn $ "withTabs: " ++ show withTabs
    putStrLn $ "lines withTabs: " ++ show inputLines
    putStrLn $ "length inputLines: " ++ show (length inputLines)
    putStrLn $ "length inputLines <= 1: " ++ show (length inputLines <= 1)
    
    -- 测试单行逻辑
    case inputLines of
      [line] -> do
        putStrLn $ "Single line: " ++ show line
        putStrLn $ "line == \" \": " ++ show (line == " ")
        putStrLn $ "all isSpace line: " ++ show (all isSpace line)
        
        let result = if line == " "
                     then "    "
                     else if all isSpace line
                          then "    "
                          else line
        putStrLn $ "Expected result: " ++ show result
      _ -> putStrLn "Not a single line"