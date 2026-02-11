import Utils (normalizeIndentation)
import Data.Char (isSpace)

main :: IO ()
main = do
    let s = "\f"  -- form feed
    putStrLn $ "Input s: " ++ show s
    putStrLn $ "all isSpace s: " ++ show (all isSpace s)
    putStrLn $ "null s: " ++ show (null s)
    let normalized = normalizeIndentation s
    putStrLn $ "normalized: " ++ show normalized
    putStrLn $ "Expected: \"    \""