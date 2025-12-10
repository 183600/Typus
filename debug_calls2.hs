import Compiler.TypeChecker (extractCallExpressions, CallExpr(..))

main :: IO ()
main = do
    let code = unlines
            [ "    s1 := \"hello\""
            , "    s2 := s1"
            , "    _ = s2"
            , "    println(s1)"
            ]
    
    putStrLn "=== Code ==="
    putStrLn code
    putStrLn ""
    
    let calls = extractCallExpressions code
    putStrLn "=== Extracted calls ==="
    mapM_ print calls