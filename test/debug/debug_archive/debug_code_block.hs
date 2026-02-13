import Utils (normalizeIndentation)
import Data.List (isPrefixOf)

main :: IO ()
main = do
    let s = ""
    let codeBlock = unlines $ ["    if condition {", "        // do something", "        return " ++ s, "    }"]
    putStrLn "Input code block:"
    putStrLn $ show codeBlock
    putStrLn "\nInput lines:"
    mapM_ (putStrLn . ("  " ++)) $ lines codeBlock
    let normalized = normalizeIndentation codeBlock
    putStrLn "\nNormalized:"
    putStrLn $ show normalized
    putStrLn "\nNormalized lines:"
    mapM_ (putStrLn . ("  " ++)) $ lines normalized
    let nonCommentLines = filter (not . isPrefixOf "//") $ lines normalized
    putStrLn "\nNon-comment lines:"
    mapM_ (putStrLn . ("  " ++)) $ nonCommentLines
    putStrLn $ "All non-comment lines have no '    ' prefix: " ++ show (all (not . isPrefixOf "    ") nonCommentLines)
    putStrLn $ "Non-empty normalized: " ++ show (not $ null normalized)