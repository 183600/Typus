import qualified Ownership

main :: IO ()
main = do 
    content <- readFile "fixtures/reference/test_ownership_with_builtin.typus"
    putStrLn "=== Testing ownership with built-in functions ==="
    let errors = Ownership.analyzeOwnership content
    putStrLn $ "Errors found: " ++ show (length errors)
    if null errors 
        then putStrLn "✓ No ownership errors detected" 
        else putStrLn $ "✗ Errors: " ++ Ownership.formatOwnershipErrors errors
    
    putStrLn "\n=== Expected: Should detect UseAfterMove error ==="
    putStrLn "Expected error: Use after move: data"