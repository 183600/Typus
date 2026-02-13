import Utils (normalizeIndentation)
import Data.List (isPrefixOf)

main :: IO ()
main = do
    let s = ""
    let codeBlock = unlines ["    if condition {", "        // do something", "        return " ++ s, "    }"]
    let pattern = "    if condition {\n        // do something\n        return \n    }\n"
    let expected = "if condition {\n    // do something\n    return \n}"
    
    putStrLn $ "Input: " ++ show codeBlock
    putStrLn $ "Pattern: " ++ show pattern
    putStrLn $ "isPrefixOf: " ++ show (pattern `isPrefixOf` codeBlock)
    
    let result = normalizeIndentation codeBlock
    putStrLn $ "Result: " ++ show result
    putStrLn $ "Expected: " ++ show expected
    putStrLn $ "Equal: " ++ show (result == expected)