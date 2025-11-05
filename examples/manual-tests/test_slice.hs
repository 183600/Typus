import Ownership (parseProgram, lexAll)

main :: IO ()
main = do
    let testCode = "x := []int{1}"
    putStrLn $ "Test code: " ++ testCode
    putStrLn ""
    putStrLn "=== TOKENS ==="
    let tokens = lexAll testCode
    mapM_ print tokens
    putStrLn ""
    putStrLn "=== PARSED PROGRAM ==="
    let program = parseProgram tokens
    print program