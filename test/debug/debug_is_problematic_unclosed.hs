import qualified Utils as U

main :: IO ()
main = do
    let s = "a\""
    let closed = "\"" ++ s ++ "\""
    let unclosed = "\"" ++ s
    putStrLn $ "s: " ++ show s
    putStrLn $ "closed: " ++ show closed
    putStrLn $ "unclosed: " ++ show unclosed
    putStrLn $ "isCompleteStringLiteral unclosed: " ++ show (U.isCompleteStringLiteral unclosed)
    putStrLn $ "unclosed length: " ++ show (length unclosed)
    putStrLn $ "unclosed head: " ++ show (if null unclosed then ' ' else head unclosed)
    putStrLn $ "unclosed last: " ++ show (if null unclosed then ' ' else last unclosed)
    putStrLn $ "unclosed characters: " ++ show (map fromEnum unclosed)
    putStrLn $ "length unclosed >= 2 && head unclosed == '\"' && last unclosed == '\"': " ++ show (length unclosed >= 2 && head unclosed == '"' && last unclosed == '"')
    putStrLn $ "isProblematicUnclosedString closed: " ++ show (U.isProblematicUnclosedString closed)
    putStrLn $ "isProblematicUnclosedString unclosed: " ++ show (U.isProblematicUnclosedString unclosed)
    putStrLn $ "Expected: not (closed) && unclosed"