import qualified Src.Utils as U

main :: IO ()
main = do
    let testInput = "A\\"
    let closed = "\"" ++ testInput ++ "\""
    let unclosed = "\"" ++ testInput
    
    putStrLn $ "Test input: " ++ show testInput
    putStrLn $ "Closed string: " ++ show closed
    putStrLn $ "Unclosed string: " ++ show unclosed
    putStrLn $ "isProblematicUnclosedString closed: " ++ show (U.isProblematicUnclosedString closed)
    putStrLn $ "isProblematicUnclosedString unclosed: " ++ show (U.isProblematicUnclosedString unclosed)
    
    -- 检查测试期望
    let expectedClosed = U.isProblematicUnclosedString closed
    let expectedUnclosed = U.isProblematicUnclosedString unclosed
    
    putStrLn $ "Test expects both to be True: closed=" ++ show expectedClosed ++ ", unclosed=" ++ show expectedUnclosed