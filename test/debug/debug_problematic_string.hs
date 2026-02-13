import qualified Utils as U

main :: IO ()
main = do
    let testCases = ["\"", "%", "\SYN", "\SUB", "]", "#", "\1073968", " ", "+"]
    
    putStrLn "Testing isProblematicUnclosedString with escaped quotes:"
    mapM_ runTest testCases
  where
    runTest s = do
        let withEscape = "\"" ++ s ++ "\\\""
        let result = U.isProblematicUnclosedString withEscape
        putStrLn $ "Input: " ++ show s ++ ", WithEscape: " ++ show withEscape ++ ", Result: " ++ show result