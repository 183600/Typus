import qualified Ownership

main :: IO ()
main = do 
    content <- readFile "fixtures/reference/test_ownership_valid.typus"
    let errors = Ownership.analyzeOwnership content
    putStrLn $ "Errors: " ++ show (length errors)
    if null errors 
        then putStrLn "✓ PASSED - No ownership errors" 
        else putStrLn $ "✗ FAILED: " ++ Ownership.formatOwnershipErrors errors