import qualified Ownership

main :: IO ()
main = do
    let code = unlines [
            "//! ownership: on",
            "",
            "package main",
            "",
            "func main() {",
            "    x := 42",
            "    y := x",  -- This should be a move
            "    println(x)",  -- This should be a use-after-move error
            "}"
            ]
    let errs = Ownership.analyzeOwnership code
    putStrLn $ "Number of errors detected: " ++ show (length errs)
    mapM_ (putStrLn . show) errs