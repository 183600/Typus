import Compiler.TypeChecker (extractCallExpressions, CallExpr(..))

main :: IO ()
main = do
    let code = "    s2 := s1\n    println(s1)"
    
    putStrLn "=== Code ==="
    putStrLn $ show code
    putStrLn ""
    
    putStrLn "=== Code with line numbers ==="
    putStrLn $ unlines $ zipWith (\i l -> show i ++ ": " ++ l) [0..] (lines code)
    putStrLn ""
    
    let calls = extractCallExpressions code
    putStrLn "=== Extracted calls ==="
    mapM_ print calls