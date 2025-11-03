import qualified Ownership
import System.IO

main :: IO ()
main = do 
    content <- readFile "closure_test.typus"
    putStrLn "=== Testing with debug mode ON ==="
    let (errors, debugLog) = Ownership.analyzeOwnershipDebug True content
    putStrLn $ "Errors found: " ++ show (length errors)
    if null errors 
        then putStrLn "✓ No ownership errors detected" 
        else putStrLn $ "✗ Errors: " ++ Ownership.formatOwnershipErrors errors
    
    putStrLn "\n=== Debug Log ==="
    mapM_ putStrLn debugLog
    
    putStrLn "\n=== Testing with debug mode OFF ==="
    let errors2 = Ownership.analyzeOwnership content
    putStrLn $ "Errors found: " ++ show (length errors2)
    if null errors2 
        then putStrLn "✓ No ownership errors detected" 
        else putStrLn $ "✗ Errors: " ++ Ownership.formatOwnershipErrors errors2