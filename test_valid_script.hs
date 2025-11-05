import qualified OwnershipAdvanced
import System.IO
import qualified Ownership as OwnershipAdvanced

main :: IO ()
main = do 
    content <- readFile "fixtures/reference/test_ownership_valid.typus"
    let errors = OwnershipAdvanced.analyzeOwnership content
    putStrLn $ "Errors: " ++ show (length errors)
    if null errors 
        then putStrLn "✓ PASSED - No ownership errors" 
        else putStrLn $ "✗ FAILED: " ++ OwnershipAdvanced.formatOwnershipErrors errors