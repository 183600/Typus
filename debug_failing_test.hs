import qualified Typus.Utils as U

main :: IO ()
main = do
    let failingInput = "a\\"
    putStrLn $ "Testing isProblematicUnclosedString with failing input: " ++ show failingInput
    putStrLn $ "isProblematicUnclosedString result: " ++ show (U.isProblematicUnclosedString failingInput)
    
    let closed = "\"" ++ failingInput ++ "\""
    let unclosed = "\"" ++ failingInput
    putStrLn $ "Closed string: " ++ show closed
    putStrLn $ "Unclosed string: " ++ show unclosed
    putStrLn $ "isProblematicUnclosedString closed: " ++ show (U.isProblematicUnclosedString closed)
    putStrLn $ "isProblematicUnclosedString unclosed: " ++ show (U.isProblematicUnclosedString unclosed)
    
    -- 测试 isCompleteStringLiteral 
    putStrLn $ "isCompleteStringLiteral closed: " ++ show (U.isCompleteStringLiteral closed)
    putStrLn $ "isCompleteStringLiteral unclosed: " ++ show (U.isCompleteStringLiteral unclosed)