import qualified Parser as P
import qualified Compiler as C

-- Test with package declaration
testOwnershipWithClosuresFixed :: IO ()
testOwnershipWithClosuresFixed = do
  let input = "package main\n//! ownership: on\nfunc a() { s := NewMyString(\"hello\"); f := func() { fmt.Println(s.data) } }"
  putStrLn $ "Testing input: " ++ input
  case P.parseTypus input of
    Left err -> putStrLn $ "Parse failed: " ++ err
    Right parsed -> do
      putStrLn "Parse succeeded"
      case C.compile parsed of
        Left errs -> putStrLn $ "Compilation failed (expected): " ++ show errs
        Right _ -> putStrLn "ERROR: Compilation should have failed!"

testOwnershipWithGoroutinesFixed :: IO ()
testOwnershipWithGoroutinesFixed = do
  let input = "package main\n//! ownership: on\nfunc a() { s := NewMyString(\"hello\"); go func() { fmt.Println(s.data) }() }"
  putStrLn $ "Testing input: " ++ input
  case P.parseTypus input of
    Left err -> putStrLn $ "Parse failed: " ++ err
    Right parsed -> do
      putStrLn "Parse succeeded"
      case C.compile parsed of
        Left errs -> putStrLn $ "Compilation failed (expected): " ++ show errs
        Right _ -> putStrLn "ERROR: Compilation should have failed!"

testOwnershipWithDeferFixed :: IO ()
testOwnershipWithDeferFixed = do
  let input = "package main\n//! ownership: on\nfunc a() { s := NewMyString(\"hello\"); defer func() { fmt.Println(s.data) }() }"
  putStrLn $ "Testing input: " ++ input
  case P.parseTypus input of
    Left err -> putStrLn $ "Parse failed: " ++ err
    Right parsed -> do
      putStrLn "Parse succeeded"
      case C.compile parsed of
        Left errs -> putStrLn $ "Compilation failed (expected): " ++ show errs
        Right _ -> putStrLn "ERROR: Compilation should have failed!"

main :: IO ()
main = do
  putStrLn "=== Testing Ownership with Closures (Fixed) ==="
  testOwnershipWithClosuresFixed
  putStrLn "\n=== Testing Ownership with Goroutines (Fixed) ==="
  testOwnershipWithGoroutinesFixed
  putStrLn "\n=== Testing Ownership with Defer (Fixed) ==="
  testOwnershipWithDeferFixed