import Compiler.TypeChecker (extractCallExpressions)

main :: IO ()
main = do
    let testCases = 
            [ "b()"
            , "a()"
            , "{ b() }"
            , "{ a() }"
            , "func a() { b() }"
            , "func b() { a() }"
            ]
    mapM_ (\testCase -> do
        let calls = extractCallExpressions testCase
        putStrLn $ "Input: " ++ show testCase
        putStrLn $ "Calls: " ++ show calls
        putStrLn ""
        ) testCases