import Compiler.TypeChecker (extractFunctionCalls)

main :: IO ()
main = do
    let code = unlines
            [ "func ownershipDemo() {"
            , "    s1 := \"hello\""
            , "    s2 := s1"
            , "    _ = s2"
            , "    println(s1)"
            , "}"
            ]
    
    putStrLn "=== Code ==="
    putStrLn code
    putStrLn ""
    
    let calls = extractFunctionCalls code
    putStrLn "=== Extracted calls ==="
    mapM_ print calls