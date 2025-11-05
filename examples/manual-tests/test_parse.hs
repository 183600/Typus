import Ownership (parseProgram, lexAll)

main :: IO ()
main = do
    let testCode = "x := []int{1}; y := x; z := x"
    putStrLn $ "Test code: " ++ testCode
    putStrLn ""
    putStrLn "=== PARSED PROGRAM ==="
    let tokens = lexAll testCode
    let program = parseProgram tokens
    print program