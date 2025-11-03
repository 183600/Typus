import qualified Ownership
import System.IO

main :: IO ()
main = do
    -- Test current function boundary handling
    putStrLn "=== Testing Function Boundary Handling ==="

    -- Read test file
    content <- readFile "test_simple_function.typus"
    putStrLn "\n--- Test Code ---"
    putStrLn content
    putStrLn "--- End Test Code ---"

    -- Analyze ownership
    let errors = Ownership.analyzeOwnership content
    putStrLn "\n--- Ownership Analysis Results ---"
    putStrLn $ "Errors found: " ++ show (length errors)
    mapM_ print errors
    putStrLn "--- End Analysis Results ---"

    -- Test more comprehensive function boundaries
    putStrLn "\n=== Testing Comprehensive Function Boundaries ==="
    content2 <- readFile "test_function_boundaries.typus"
    putStrLn "\n--- Comprehensive Test Code ---"
    putStrLn content2
    putStrLn "--- End Comprehensive Test Code ---"

    let errors2 = Ownership.analyzeOwnership content2
    putStrLn "\n--- Comprehensive Analysis Results ---"
    putStrLn $ "Errors found: " ++ show (length errors2)
    mapM_ print errors2
    putStrLn "--- End Comprehensive Analysis Results ---"