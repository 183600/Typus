import qualified OwnershipAdvanced
import System.IO

main :: IO ()
main = do
    content <- readFile "test_function_boundaries.typus"
    let errors = OwnershipAdvanced.analyzeOwnership content
    putStrLn "=== Advanced Ownership Analysis Results ==="
    putStrLn $ OwnershipAdvanced.formatOwnershipErrors errors
    putStrLn $ "Total errors: " ++ show (length errors)
    
    -- Test individual cases
    putStrLn "\n=== Testing Individual Cases ==="
    
    -- Test 1: Basic function call with borrow
    let test1 = "//! ownership: on\nfunc main() { x := 42; y := &x; println(x); println(y) }"
    let errors1 = OwnershipAdvanced.analyzeOwnership test1
    putStrLn $ "Test 1 (Basic borrow): " ++ show (length errors1) ++ " errors"
    if null errors1 then putStrLn "✓ PASSED" else putStrLn $ "✗ FAILED: " ++ OwnershipAdvanced.formatOwnershipErrors errors1
    
    -- Test 2: Function parameter move
    let test2 = "//! ownership: on\nfunc test() { data := \"hello\"; take_value(data); println(data) }"
    let errors2 = OwnershipAdvanced.analyzeOwnership test2
    putStrLn $ "Test 2 (Parameter move): " ++ show (length errors2) ++ " errors"
    if not (null errors2) && any (\e -> case e of OwnershipAdvanced.UseAfterMove "data" -> True; _ -> False) errors2
      then putStrLn "✓ PASSED (correctly detected use after move)"
      else putStrLn $ "✗ FAILED: " ++ OwnershipAdvanced.formatOwnershipErrors errors2
    
    -- Test 3: Mutable borrow
    let test3 = "//! ownership: on\nfunc test() { data := \"hello\"; take_mut_borrow(&mut data); println(data) }"
    let errors3 = OwnershipAdvanced.analyzeOwnership test3
    putStrLn $ "Test 3 (Mutable borrow): " ++ show (length errors3) ++ " errors"
    if not (null errors3) && any (\e -> case e of OwnershipAdvanced.UseWhileMutBorrowed "data" -> True; _ -> False) errors3
      then putStrLn "✓ PASSED (correctly detected use while mut borrowed)"
      else putStrLn $ "✗ FAILED: " ++ OwnershipAdvanced.formatOwnershipErrors errors3
    
    -- Test 4: Cross-function ownership transfer
    let test4 = unlines
          [ "//! ownership: on"
          , "func process(s string) string { return s }"
          , "func main() {"
          , "  data := \"test\""
          , "  result := process(data)"
          , "  println(data)  // Should be error - data was moved"
          , "  println(result)"
          , "}"
          ]
    let errors4 = OwnershipAdvanced.analyzeOwnership test4
    putStrLn $ "Test 4 (Cross-function move): " ++ show (length errors4) ++ " errors"
    if not (null errors4) && any (\e -> case e of OwnershipAdvanced.UseAfterMove "data" -> True; _ -> False) errors4
      then putStrLn "✓ PASSED (correctly detected cross-function move)"
      else putStrLn $ "✗ FAILED: " ++ OwnershipAdvanced.formatOwnershipErrors errors4