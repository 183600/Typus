import Utils
import Data.Char (isSpace)

-- 测试 normalizeIndentation 对纯制表符的处理
main :: IO ()
main = do
    putStrLn "Testing normalizeIndentation with tabs..."
    
    let input = "\t\tb \t"
    
    putStrLn $ "Input: " ++ show input
    putStrLn $ "Has tab: " ++ show ('\t' `elem` input)
    putStrLn $ "Has space: " ++ show (' ' `elem` input)
    putStrLn $ "All isSpace: " ++ show (all isSpace input)
    putStrLn $ "Is pure tab: " ++ show ('\t' `elem` input && not (' ' `elem` input))
    putStrLn $ "Is mixed: " ++ show ('\t' `elem` input && ' ' `elem` input)
    
    let normalized = normalizeIndentation input
    putStrLn $ "Normalized: " ++ show normalized