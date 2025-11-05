import qualified Ownership

main :: IO ()
main = do
    -- Simple test to see borrow variable generation
    let code = unlines [
            "//! ownership: on",
            "",
            "package main",
            "",
            "func main() {",
            "    x := 42",
            "    y := &x",
            "}"
            ]
    let errs = Ownership.analyzeOwnership code
    putStrLn $ "Errors: " ++ show errs
    putStrLn "--- Analysis Complete ---"