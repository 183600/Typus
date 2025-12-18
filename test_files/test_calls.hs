import Compiler.TypeChecker (extractCallExpressions, CallExpr(..))

main :: IO ()
main = do
    let source = "package main\nfunc main() {\n    println(undefinedVar)\n}"
    let calls = extractCallExpressions source
    putStrLn $ "Found calls: " ++ show calls