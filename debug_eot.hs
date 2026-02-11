import Utils (normalizeIndentation)
import Data.Char (isPrint)
import Data.Enum (fromEnum)

main :: IO ()
main = do
    let input = "\EOT"
    putStrLn $ "Input: " ++ show input
    putStrLn $ "Input char codes: " ++ show (map fromEnum input)
    putStrLn $ "isPrint: " ++ show (map isPrint input)
    let output = normalizeIndentation input
    putStrLn $ "Output: " ++ show output
    putStrLn $ "Equal: " ++ show (input == output)