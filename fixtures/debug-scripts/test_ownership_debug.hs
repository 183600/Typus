import Ownership

main :: IO ()
main = do
    let code = "data := []int{1, 2, 3}\nf := func() {\n    fmt.Println(data)\n}\nf()\nfmt.Println(data)"
    let (errors, logs) = analyzeOwnershipDebug True code
    putStrLn "=== DEBUG LOGS ==="
    mapM_ putStrLn logs
    putStrLn "=== ERRORS ==="
    mapM_ print errors