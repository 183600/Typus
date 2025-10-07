import qualified OwnershipAdvanced
import System.IO

main :: IO ()
main = do 
    content <- readFile "test_ownership_valid.typus"
    let errors = OwnershipAdvanced.analyzeOwnership content
    putStrLn $ "Errors: " ++ show (length errors)
    if null errors 
        then putStrLn "✓ PASSED - No ownership errors" 
        else putStrLn $ "✗ FAILED: " ++ OwnershipAdvanced.formatOwnershipErrors errors