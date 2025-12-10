import Compiler.TypeChecker (extractCallExpressions, CallExpr(..))

main :: IO ()
main = do
    let code = "    s2 := s1\n    println(s1)"
    
    putStrLn "=== Code ==="
    putStrLn code
    putStrLn ""
    
    let calls = extractCallExpressions code
    putStrLn "=== Extracted calls ==="
    mapM_ print calls