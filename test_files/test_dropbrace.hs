import Compiler.TypeChecker (dropClosingBrace)

main :: IO ()
main = do
    let bodyLines = ["b() }"]
    let result = dropClosingBrace bodyLines
    putStrLn $ "Original: " ++ show bodyLines
    putStrLn $ "Result: " ++ show result