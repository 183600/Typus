import Utils
import Data.Char (isSpace)

-- Test normalizeIndentation with tab - more detailed
testTab :: IO ()
testTab = do
    let input = "\t"
    let result = normalizeIndentation input
    putStrLn $ "Input: " ++ show input
    putStrLn $ "Result: " ++ show result
    putStrLn $ "Length of result: " ++ show (length result)
    
    -- Let's trace through the logic
    let inputLines = lines input
    putStrLn $ "Input lines: " ++ show inputLines
    putStrLn $ "Length of inputLines: " ++ show (length inputLines)
    
    -- Check if all is space
    putStrLn $ "All isSpace: " ++ show (all isSpace input)
    
    -- Check the specific conditions
    putStrLn $ "Input == \"\\t\": " ++ show (input == "\t")
    putStrLn $ "All isSpace input: " ++ show (all isSpace input)
    putStrLn $ "Not (all isSpace input): " ++ show (not (all isSpace input))

main :: IO ()
main = do
    putStrLn "Test normalizeIndentation with tab"
    testTab