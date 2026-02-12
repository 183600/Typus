import Utils (normalizeIndentation)
import Data.List (isPrefixOf)

main :: IO ()
main = do
    let s = ""
    let codeBlock = unlines ["    if condition {", "        // do something", "        return " ++ s, "    }"]
    let normalized = normalizeIndentation codeBlock
    let normLines = lines normalized
    let withFourSpaces = filter (isPrefixOf "    ") normLines
    
    putStrLn $ "s: " ++ show s
    putStrLn $ "codeBlock: " ++ show codeBlock
    putStrLn $ "normalized: " ++ show normalized
    putStrLn $ "normLines: " ++ show normLines
    putStrLn $ "withFourSpaces: " ++ show withFourSpaces
    putStrLn $ "length withFourSpaces: " ++ show (length withFourSpaces)
    putStrLn $ "length normLines: " ++ show (length normLines)
    putStrLn $ "Condition: " ++ show (length withFourSpaces < length normLines)