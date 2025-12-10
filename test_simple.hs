import Compiler.TypeChecker (extractCallExpressions)

main :: IO ()
main = do
    let testCase = "b()"
    let calls = extractCallExpressions testCase
    putStrLn $ "Input: " ++ show testCase
    putStrLn $ "Calls: " ++ show calls
    putStrLn $ "Length: " ++ show (length calls)