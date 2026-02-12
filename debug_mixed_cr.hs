import Utils
import Data.Char (chr)

main :: IO ()
main = do
    let input = [chr 9, ' ', ' ', chr 9, ' ', ' ', chr 13, ' ', ' ', chr 9, ' ', ' ']  -- \t  \t  \r  \t  (correct format)
    let result = normalizeIndentation input
    putStrLn $ "Input: " ++ show input
    putStrLn $ "Result: " ++ show result
    putStrLn $ "Expected: \"    \""
    putStrLn $ "Equal to expected: " ++ show (result == "    ")