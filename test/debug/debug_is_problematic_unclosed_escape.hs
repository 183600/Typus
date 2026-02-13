import qualified Utils as U

main :: IO ()
main = do
    let s = "a"
        withEscape = "\"" ++ s ++ "\\\""
    putStrLn $ "s: " ++ show s
    putStrLn $ "withEscape: " ++ show withEscape
    putStrLn $ "withEscape characters: " ++ show (map fromEnum withEscape)
    putStrLn $ "isProblematicUnclosedString withEscape: " ++ show (U.isProblematicUnclosedString withEscape)
    putStrLn $ "isCompleteStringLiteral withEscape: " ++ show (U.isCompleteStringLiteral withEscape)