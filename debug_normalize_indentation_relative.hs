import Utils (normalizeIndentation)
import Data.Char (isSpace)

main :: IO ()
main = do
    let s = "\t&"
    let lines' = lines s
    let normalized = normalizeIndentation s
    let normLines = lines normalized
    
    putStrLn $ "s = " ++ show s
    putStrLn $ "lines' = " ++ show lines'
    putStrLn $ "normalized = " ++ show normalized
    putStrLn $ "normLines = " ++ show normLines
    putStrLn $ "Test expects: normalized = " ++ show s
    putStrLn $ "Test passes: " ++ show (normalized == s)
    putStrLn $ "isSpace s = " ++ show (all isSpace s)