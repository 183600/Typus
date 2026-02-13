import Utils
import Data.Char (chr)

main :: IO ()
main = do
    -- Test the exact string
    let input1 = "\t  \t  \r  \t  "
    let result1 = normalizeIndentation input1
    putStrLn $ "Test 1 - Direct string:"
    putStrLn $ "  Input: " ++ show input1
    putStrLn $ "  Result: " ++ show result1
    putStrLn $ "  Expected: \"    \""
    putStrLn $ "  Success: " ++ show (result1 == "    ")
    
    -- Test with chr
    let input2 = [chr 9, ' ', ' ', chr 9, ' ', ' ', chr 13, ' ', ' ', chr 9, ' ', ' ']
    let result2 = normalizeIndentation input2
    putStrLn $ "\nTest 2 - With chr:"
    putStrLn $ "  Input: " ++ show input2
    putStrLn $ "  Result: " ++ show result2
    putStrLn $ "  Expected: \"    \""
    putStrLn $ "  Success: " ++ show (result2 == "    ")
    
    -- Check if they're equal
    putStrLn $ "\nInputs equal: " ++ show (input1 == input2)