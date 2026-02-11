import Utils (removeLineComments)
import Data.Enum (fromEnum)

main :: IO ()
main = do
    let lines' = ["\nA"]
    let code = unlines lines'
    putStrLn $ "Input lines': " ++ show lines'
    putStrLn $ "code: " ++ show code
    putStrLn $ "code char codes: " ++ show (map fromEnum code)
    let processed = removeLineComments code
    putStrLn $ "processed: " ++ show processed
    putStrLn $ "processed char codes: " ++ show (map fromEnum processed)
    let procLines = lines processed
    putStrLn $ "procLines: " ++ show procLines
    putStrLn $ "length lines': " ++ show (length lines')
    putStrLn $ "length procLines: " ++ show (length procLines)