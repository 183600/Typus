import Ownership (analyzeOwnership)

main :: IO ()
main = do
    let code = "//! ownership: on\npackage main\nfunc consume(x string) string { return x }\nfunc main() {\n    data := \"hello\"\n    consume(data)\n    println(data)\n}"
    let errors = analyzeOwnership code
    putStrLn "Ownership errors:"
    mapM_ print errors