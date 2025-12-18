import Compiler.TypeChecker (extractVarDeclarations)

main :: IO ()
main = do
    let source = unlines
          [ "package main"
          , "func calculate(x int, y int) int {"
          , "    if x > y {"
          , "        return x * 2 + y"
          , "    } else {"
          , "        return y * 3 - x"
          , "    }"
          , "}"
          , "func main() {"
          , "    result := calculate(10, 5)"
          , "    println(result)"
          , "}"
          ]
    let varDecls = extractVarDeclarations source
    putStrLn $ "Found variable declarations: " ++ show (length varDecls)
    mapM_ print varDecls