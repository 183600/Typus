import qualified Ownership as O

main :: IO ()
main = do
  -- Test closure ownership
  let closureInput = "package main\n//! ownership: on\nfunc a() { s := NewMyString(\"hello\"); f := func() { fmt.Println(s.data) } }"
  putStrLn $ "Testing closure input: " ++ closureInput
  let closureErrors = O.analyzeOwnership closureInput
  putStrLn $ "Closure errors: " ++ show closureErrors
  
  -- Test goroutine ownership
  let goroutineInput = "package main\n//! ownership: on\nfunc a() { s := NewMyString(\"hello\"); go func() { fmt.Println(s.data) }() }"
  putStrLn $ "\nTesting goroutine input: " ++ goroutineInput
  let goroutineErrors = O.analyzeOwnership goroutineInput
  putStrLn $ "Goroutine errors: " ++ show goroutineErrors
  
  -- Test defer ownership
  let deferInput = "package main\n//! ownership: on\nfunc a() { s := NewMyString(\"hello\"); defer func() { fmt.Println(s.data) }() }"
  putStrLn $ "\nTesting defer input: " ++ deferInput
  let deferErrors = O.analyzeOwnership deferInput
  putStrLn $ "Defer errors: " ++ show deferErrors