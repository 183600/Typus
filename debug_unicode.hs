import Utils (normalizeIndentation)
import Data.Enum (fromEnum)

main :: IO ()
main = do
    let lines' = ["\28683","\n"]
    let withMixed = map ("\t  " ++) lines'
    putStrLn $ "lines': " ++ show lines'
    putStrLn $ "withMixed: " ++ show withMixed
    let input = unlines withMixed
    putStrLn $ "input: " ++ show input
    putStrLn $ "input char codes: " ++ show (map fromEnum input)
    let normalized = normalizeIndentation input
    putStrLn $ "normalized: " ++ show normalized
    let normLines = lines normalized
    putStrLn $ "normLines: " ++ show normLines
    putStrLn $ "length lines': " ++ show (length lines')
    putStrLn $ "length normLines: " ++ show (length normLines)