import qualified Parser as P
import qualified Compiler as C

-- Test specific failing cases
testOwnershipWithClosures :: IO ()
testOwnershipWithClosures = do
  let input = "//! ownership: on\nfunc a() { s := NewMyString(\"hello\"); f := func() { fmt.Println(s.data) } }"
  putStrLn $ "Testing input: " ++ input
  case P.parseTypus input of
    Left err -> putStrLn $ "Parse failed: " ++ err
    Right parsed -> do
      putStrLn "Parse succeeded"
      case C.compile parsed of
        Left errs -> putStrLn $ "Compilation failed (expected): " ++ show errs
        Right _ -> putStrLn "ERROR: Compilation should have failed!"

testOwnershipWithGoroutines :: IO ()
testOwnershipWithGoroutines = do
  let input = "//! ownership: on\nfunc a() { s := NewMyString(\"hello\"); go func() { fmt.Println(s.data) }() }"
  putStrLn $ "Testing input: " ++ input
  case P.parseTypus input of
    Left err -> putStrLn $ "Parse failed: " ++ err
    Right parsed -> do
      putStrLn "Parse succeeded"
      case C.compile parsed of
        Left errs -> putStrLn $ "Compilation failed (expected): " ++ show errs
        Right _ -> putStrLn "ERROR: Compilation should have failed!"

testSyntaxErrorHandling :: IO ()
testSyntaxErrorHandling = do
  let input = ""
  putStrLn $ "Testing input: '" ++ input ++ "'"
  case P.parseTypus input of
    Left err -> putStrLn $ "Parse failed (expected): " ++ err
    Right parsed -> do
      putStrLn "ERROR: Parse should have failed!"
      case C.compile parsed of
        Left errs -> putStrLn $ "Compilation failed: " ++ show errs
        Right _ -> putStrLn "Compilation succeeded"

main :: IO ()
main = do
  putStrLn "=== Testing Ownership with Closures ==="
  testOwnershipWithClosures
  putStrLn "\n=== Testing Ownership with Goroutines ==="
  testOwnershipWithGoroutines
  putStrLn "\n=== Testing Syntax Error Handling ==="
  testSyntaxErrorHandling