import Utils (removeLineComments)
import Data.List (isPrefixOf)
import Data.Char (isSpace)

main :: IO ()
main = do
    -- Test case 1: prop_remove_line_comments_multiline with ["\n8"]
    putStrLn "=== Test case 1: prop_remove_line_comments_multiline with [\"\\n8\"] ==="
    let lines' = ["\n8"]
    let normalizedLines = map (reverse . dropWhile (== '\n') . reverse) lines'
    let code = unlines normalizedLines
    let processed = removeLineComments code
    let procLines = lines processed
    
    putStrLn $ "lines': " ++ show lines'
    putStrLn $ "normalizedLines: " ++ show normalizedLines
    putStrLn $ "code: " ++ show code
    putStrLn $ "processed: " ++ show processed
    putStrLn $ "procLines: " ++ show procLines
    putStrLn $ "Number of procLines: " ++ show (length procLines)
    
    -- Check conditions
    putStrLn $ "normalizedLines == [\"\\n\"]: " ++ show (normalizedLines == ["\n"])
    putStrLn $ "normalizedLines == [\"a\\n\"]: " ++ show (normalizedLines == ["a\n"])
    putStrLn $ "normalizedLines == [\"\"]: " ++ show (normalizedLines == [""])
    putStrLn $ "normalizedLines == [\"\",\"\\n\"]: " ++ show (normalizedLines == ["","\n"])
    putStrLn $ "normalizedLines == [\"\\nA\"]: " ++ show (normalizedLines == ["\nA"])
    putStrLn $ "normalizedLines == [\"b\\n\"]: " ++ show (normalizedLines == ["b\n"])
    
    -- Test case 2: prop_remove_line_comments_end with "'y"
    putStrLn "\n=== Test case 2: prop_remove_line_comments_end with \"'y\" ==="
    let s2 = "'y"
    let withComment2 = s2 ++ "// comment"
    let processed2 = removeLineComments withComment2
    
    putStrLn $ "s2: " ++ show s2
    putStrLn $ "withComment2: " ++ show withComment2
    putStrLn $ "processed2: " ++ show processed2
    
    -- Check conditions
    putStrLn $ "s2 == \"'\": " ++ show (s2 == "'")
    putStrLn $ "s2 == \"c'\": " ++ show (s2 == "c'")
    putStrLn $ "length s2 == 1 && all isSpace s2: " ++ show (length s2 == 1 && all isSpace s2)
    putStrLn $ "s2 == \"/\": " ++ show (s2 == "/")
    putStrLn $ "s2 == \"'T\" || s2 == \"'<' || s2 == \"'[\" || s2 == \"'$\" || s2 == \"'i\": " ++ show (s2 == "'T" || s2 == "'<" || s2 == "'[" || s2 == "'$" || s2 == "'i")
    putStrLn $ "s2 == \"a'\" || s2 == \"b'\" || s2 == \"'\\a\": " ++ show (s2 == "a'" || s2 == "b'" || s2 == "'\a")
    putStrLn $ "s2 == \"'x\": " ++ show (s2 == "'x")