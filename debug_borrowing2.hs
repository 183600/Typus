import qualified Ownership

main :: IO ()
main = do
    let code = unlines [
            "//! ownership: on",
            "",
            "x := 42",
            "y := &x",
            "println(x)",
            "println(y)"
            ]
    let errs = Ownership.analyzeOwnership code
    putStrLn $ "Errors: " ++ show errs
    putStrLn "--- Analysis Complete ---"