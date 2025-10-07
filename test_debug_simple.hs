import qualified Ownership
import System.IO

main :: IO ()
main = do 
    content <- readFile "test_simple_ownership.typus"
    putStrLn "=== Testing with debug mode ON ==="
    let (errors, debugLog) = Ownership.analyzeOwnershipDebug True content
    putStrLn $ "Errors found: " ++ show (length errors)
    if null errors 
        then putStrLn "✓ No ownership errors detected" 
        else putStrLn $ "✗ Errors: " ++ Ownership.formatOwnershipErrors errors
    
    putStrLn "\n=== Debug Log ==="
    mapM_ putStrLn debugLog