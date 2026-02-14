import qualified Ownership.Analyzer as OA

main :: IO ()
main = do
  -- Test closure ownership directly
  let closureInput = "package main\n//! ownership: on\nfunc a() { s := NewMyString(\"hello\"); f := func() { fmt.Println(s.data) } }"
  putStrLn $ "Testing closure input: " ++ closureInput
  let closureErrors = OA.heuristicOwnershipErrors closureInput
  putStrLn $ "Closure heuristic errors: " ++ show closureErrors
  
  -- Test goroutine ownership directly
  let goroutineInput = "package main\n//! ownership: on\nfunc a() { s := NewMyString(\"hello\"); go func() { fmt.Println(s.data) }() }"
  putStrLn $ "\nTesting goroutine input: " ++ goroutineInput
  let goroutineErrors = OA.heuristicOwnershipErrors goroutineInput
  putStrLn $ "Goroutine heuristic errors: " ++ show goroutineErrors
  
  -- Test defer ownership directly
  let deferInput = "package main\n//! ownership: on\nfunc a() { s := NewMyString(\"hello\"); defer func() { fmt.Println(s.data) }() }"
  putStrLn $ "\nTesting defer input: " ++ deferInput
  let deferErrors = OA.heuristicOwnershipErrors deferInput
  putStrLn $ "Defer heuristic errors: " ++ show deferErrors