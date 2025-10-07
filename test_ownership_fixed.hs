import qualified OwnershipFixed
import System.IO

main :: IO ()
main = do
    content <- readFile "test_function_boundaries.typus"
    let errors = OwnershipFixed.analyzeOwnership content
    putStrLn "=== Fixed Ownership Analysis Results ==="
    putStrLn $ OwnershipFixed.formatOwnershipErrors errors
    putStrLn $ "Total errors: " ++ show (length errors)
    
    -- Test individual cases
    putStrLn "\n=== Testing Individual Cases ==="
    
    -- Test 1: Basic function call with borrow
    let test1 = "//! ownership: on\nfunc main() { x := 42; y := &x; println(x); println(y) }"
    let errors1 = OwnershipFixed.analyzeOwnership test1
    putStrLn $ "Test 1 (Basic borrow): " ++ show (length errors1) ++ " errors"
    
    -- Test 2: Function parameter move
    let test2 = "//! ownership: on\nfunc test() { data := \"hello\"; take_value(data); println(data) }"
    let errors2 = OwnershipFixed.analyzeOwnership test2
    putStrLn $ "Test 2 (Parameter move): " ++ show (length errors2) ++ " errors"
    
    -- Test 3: Mutable borrow
    let test3 = "//! ownership: on\nfunc test() { data := \"hello\"; take_mut_borrow(&mut data); println(data) }"
    let errors3 = OwnershipFixed.analyzeOwnership test3
    putStrLn $ "Test 3 (Mutable borrow): " ++ show (length errors3) ++ " errors"