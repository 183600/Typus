import Utils (normalizeIndentation)
import Data.Char (isSpace, isPrint)

main :: IO ()
main = do
    let s = "\STX"  -- Start of Text character
    let mixed = "\t  \t  " ++ s ++ "  \t  "
    putStrLn $ "Input s: " ++ show s
    putStrLn $ "mixed: " ++ show mixed
    let normalized = normalizeIndentation mixed
    putStrLn $ "normalized: " ++ show normalized
    putStrLn $ "null s: " ++ show (null s)
    putStrLn $ "all isSpace mixed: " ++ show (all isSpace mixed)
    putStrLn $ "any (not . isPrint) s: " ++ show (any (not . isPrint) s)
    putStrLn $ "Expected (equal to mixed): " ++ show (normalized == mixed)