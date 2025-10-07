import Ownership (parseProgram, lexAll)

main :: IO ()
main = do
    let testCode = "x := 42; y := x; z := x"
    putStrLn $ "Test code: " ++ testCode
    putStrLn ""
    putStrLn "=== TOKENS ==="
    let tokens = lexAll testCode
    mapM_ print tokens
    putStrLn ""
    putStrLn "=== PARSED PROGRAM ==="
    let program = parseProgram tokens
    print program