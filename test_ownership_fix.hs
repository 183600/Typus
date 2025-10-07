import qualified Ownership
import System.IO

main :: IO ()
main = do
    content <- readFile "test_function_boundaries.typus"
    let errors = Ownership.analyzeOwnership content
    putStrLn "=== Ownership Analysis Results ==="
    putStrLn $ Ownership.formatOwnershipErrors errors
    putStrLn $ "Total errors: " ++ show (length errors)