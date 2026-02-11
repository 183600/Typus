-- Test the failing case
import qualified Utils as U

main :: IO ()
main = do
    let s = "a\\"
    let closed = "\"" ++ s ++ "\""
    let unclosed = "\"" ++ s
    
    putStrLn $ "Input s: " ++ show s
    putStrLn $ "Closed: " ++ show closed
    putStrLn $ "Unclosed: " ++ show unclosed
    
    putStrLn $ "U.isProblematicUnclosedString closed: " ++ show (U.isProblematicUnclosedString closed)
    putStrLn $ "U.isProblematicUnclosedString unclosed: " ++ show (U.isProblematicUnclosedString unclosed)
    
    -- The test expects:
    -- not (U.isProblematicUnclosedString closed) && U.isProblematicUnclosedString unclosed
    let expected = not (U.isProblematicUnclosedString closed) && U.isProblematicUnclosedString unclosed
    putStrLn $ "Test result: " ++ show expected