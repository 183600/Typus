import qualified Ownership

main :: IO ()
main = do
    content <- readFile "fixtures/reference/test_function_boundaries.typus"
    let errors = Ownership.analyzeOwnership content
    putStrLn "=== Ownership Analysis Results ==="
    putStrLn $ Ownership.formatOwnershipErrors errors
    putStrLn $ "Total errors: " ++ show (length errors)