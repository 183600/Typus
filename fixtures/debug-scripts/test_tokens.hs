import Ownership (lexAll)

main :: IO ()
main = do
    let testCode = "x := []int{1}; y := x; z := x"
    putStrLn $ "Test code: " ++ testCode
    putStrLn ""
    putStrLn "=== TOKENS ==="
    let tokens = lexAll testCode
    mapM_ print tokens