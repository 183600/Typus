import Utils
import Data.List

main :: IO ()
main = do
    -- Test case 1: normalizeIndentation deep with single character
    putStrLn "Test 1: normalizeIndentation deep with single character"
    let s1 = "a"
    let depth1 = 1
    let deepIndent1 = unlines $ map (replicate depth1 ' ' ++) (lines s1)
    putStrLn $ "Input: " ++ show deepIndent1
    let normalized1 = normalizeIndentation deepIndent1
    putStrLn $ "Normalized: " ++ show normalized1
    let normLines1 = lines normalized1
    putStrLn $ "Has leading spaces: " ++ show (any (isPrefixOf (replicate depth1 ' ')) normLines1)
    
    -- Test case 2: normalizeIndentation code block with empty string
    putStrLn "\nTest 2: normalizeIndentation code block with empty string"
    let s2 = ""
    let codeBlock2 = unlines $ ["    if condition {", "        // do something", "        return " ++ s2, "    }"]
    putStrLn $ "Input: " ++ show codeBlock2
    let normalized2 = normalizeIndentation codeBlock2
    putStrLn $ "Normalized: " ++ show normalized2
    let normLines2 = lines normalized2
    let nonCommentLines2 = filter (not . isPrefixOf "//") normLines2
    putStrLn $ "Non-comment lines: " ++ show nonCommentLines2
    putStrLn $ "All non-comment lines without leading spaces: " ++ show (all (not . isPrefixOf "    ") nonCommentLines2)
    
    -- Test case 3: normalizeIndentation nested with empty string
    putStrLn "\nTest 3: normalizeIndentation nested with empty string"
    let s3 = ""
    let nested3 = unlines $ ["    func outer() {", "        func inner() {", "            " ++ s3, "        }", "    }"]
    putStrLn $ "Input: " ++ show nested3
    let normalized3 = normalizeIndentation nested3
    putStrLn $ "Normalized: " ++ show normalized3
    let normLines3 = lines normalized3
    putStrLn $ "All lines without leading spaces: " ++ show (all (not . isPrefixOf "    ") normLines3)
    putStrLn $ "Not null: " ++ show (not (null normalized3))
    
    -- Test case 4: normalizeIndentation labels with empty string
    putStrLn "\nTest 4: normalizeIndentation labels with empty string"
    let s4 = ""
    let labeled4 = unlines $ ["label1:", "    " ++ s4, "label2:", "    " ++ s4]
    putStrLn $ "Input: " ++ show labeled4
    let normalized4 = normalizeIndentation labeled4
    putStrLn $ "Normalized: " ++ show normalized4
    let normLines4 = lines normalized4
    putStrLn $ "All lines without leading spaces: " ++ show (all (not . isPrefixOf "    ") normLines4)