import Utils
import Data.Char (isSpace)

main :: IO ()
main = do
    let lines' = [""]
    let withMixed = map ("\t  " ++) lines'
    putStrLn $ "lines': " ++ show lines'
    putStrLn $ "withMixed: " ++ show withMixed
    putStrLn $ "unlines withMixed: " ++ show (unlines withMixed)
    
    let normalized = normalizeIndentation (unlines withMixed)
    putStrLn $ "normalized: " ++ show normalized
    
    -- 测试多行逻辑
    let inputLines = [""]
    putStrLn $ "inputLines: " ++ show inputLines
    putStrLn $ "length inputLines: " ++ show (length inputLines)
    putStrLn $ "length inputLines <= 1: " ++ show (length inputLines <= 1)