import Parser
import SourceLocation (locatedValue)

main :: IO ()
main = do
    let testInput = unlines [
            "//! ownership: on",
            "//! dependent_types: off",
            "",
            "package main",
            "",
            "func main() {",
            "    println(\"Hello\")",
            "}"
            ]
    
    case parse testInput of
        Left err -> putStrLn $ "Parse error: " ++ err
        Right ast -> do
            putStrLn "Parsed successfully!"
            print ast
            let dirs = tfDirectives ast
                ownershipVal = fmap locatedValue (fdOwnership dirs)
                dependentVal = fmap locatedValue (fdDependentTypes dirs)
            putStrLn $ "Ownership directive: " ++ show ownershipVal
            putStrLn $ "Dependent types directive: " ++ show dependentVal
