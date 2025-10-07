import qualified Ownership

main :: IO ()
main = do
    -- Test 1: Basic immutable borrowing
    putStrLn "=== Test 1: Basic Immutable Borrowing ==="
    let code1 = unlines [
            "//! ownership: on",
            "",
            "package main",
            "",
            "func main() {",
            "    x := 42",
            "    y := &x",  -- Immutable borrow
            "    println(y)",
            "    println(x)",  -- Should be OK with immutable borrow
            "}"
            ]
    let errs1 = Ownership.analyzeOwnership code1
    putStrLn $ "Errors detected: " ++ show (length errs1)
    mapM_ (putStrLn . show) errs1
    putStrLn ""

    -- Test 2: Mutable borrowing
    putStrLn "=== Test 2: Mutable Borrowing ==="
    let code2 = unlines [
            "//! ownership: on",
            "",
            "package main",
            "",
            "func main() {",
            "    x := 42",
            "    y := &mut x",  -- Mutable borrow
            "    println(x)",  -- Should trigger UseWhileMutBorrowed error
            "}"
            ]
    let errs2 = Ownership.analyzeOwnership code2
    putStrLn $ "Errors detected: " ++ show (length errs2)
    mapM_ (putStrLn . show) errs2
    putStrLn ""

    -- Test 3: Borrowing lifecycle
    putStrLn "=== Test 3: Borrowing Lifecycle ==="
    let code3 = unlines [
            "//! ownership: on",
            "",
            "package main",
            "",
            "func main() {",
            "    x := 42",
            "    {",
            "        y := &mut x",  -- Mutable borrow in inner scope",
            "        println(y)",
            "    }",  -- y goes out of scope, borrow should be released
            "    println(x)",  -- Should be OK after borrow is released
            "}"
            ]
    let errs3 = Ownership.analyzeOwnership code3
    putStrLn $ "Errors detected: " ++ show (length errs3)
    mapM_ (putStrLn . show) errs3
    putStrLn ""

    -- Test 4: Multiple immutable borrows
    putStrLn "=== Test 4: Multiple Immutable Borrows ==="
    let code4 = unlines [
            "//! ownership: on",
            "",
            "package main",
            "",
            "func main() {",
            "    x := 42",
            "    y := &x",  -- First immutable borrow
            "    z := &x",  -- Second immutable borrow (should be OK)
            "    println(y, z)",
            "    println(x)",  -- Should be OK with immutable borrows
            "}"
            ]
    let errs4 = Ownership.analyzeOwnership code4
    putStrLn $ "Errors detected: " ++ show (length errs4)
    mapM_ (putStrLn . show) errs4
    putStrLn ""

    -- Test 5: Multiple mutable borrows (should fail)
    putStrLn "=== Test 5: Multiple Mutable Borrows (Should Fail) ==="
    let code5 = unlines [
            "//! ownership: on",
            "",
            "package main",
            "",
            "func main() {",
            "    x := 42",
            "    y := &mut x",  -- First mutable borrow
            "    z := &mut x",  -- Second mutable borrow (should fail)",
            "    println(y, z)",
            "}"
            ]
    let errs5 = Ownership.analyzeOwnership code5
    putStrLn $ "Errors detected: " ++ show (length errs5)
    mapM_ (putStrLn . show) errs5
    putStrLn ""