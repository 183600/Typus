-- Debug script for normalizeIndentation with ["",""]
import Utils as U

-- Test normalizeIndentation with ["",""]
test1 :: IO ()
test1 = do
    let lines' = ["",""]
    let withMixed = map ("\t  " ++) lines'
    let unlinesInput = unlines withMixed
    let normalized = U.normalizeIndentation unlinesInput
    let normLines = lines normalized
    putStrLn $ "Input lines': " ++ show lines'
    putStrLn $ "With mixed: " ++ show withMixed
    putStrLn $ "Unlines input: " ++ show unlinesInput
    putStrLn $ "Normalized: " ++ show normalized
    putStrLn $ "Norm lines: " ++ show normLines
    putStrLn $ "Length normLines: " ++ show (length normLines)
    putStrLn $ "Expected length: " ++ show (length lines')
    putStrLn ""

-- Test with specific case
test2 :: IO ()
test2 = do
    let input = "\t  \n\t  \n"
    let normalized = U.normalizeIndentation input
    let normLines = lines normalized
    putStrLn $ "Input: " ++ show input
    putStrLn $ "Normalized: " ++ show normalized
    putStrLn $ "Norm lines: " ++ show normLines
    putStrLn $ "Length normLines: " ++ show (length normLines)
    putStrLn ""

main :: IO ()
main = do
    putStrLn "=== Test 1: normalizeIndentation with [\"\",\"\"] ==="
    test1
    
    putStrLn "=== Test 2: normalizeIndentation with specific input ==="
    test2