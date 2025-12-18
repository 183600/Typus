import qualified Compiler
import qualified Parser

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
    putStrLn $ "Testing source: " ++ source
    case Parser.parseTypus source of
        Left err -> putStrLn $ "Parse error: " ++ err
        Right parsed -> do
            putStrLn "Parse succeeded"
            case Compiler.compile parsed of
                Left err -> putStrLn $ "Compile error: " ++ Compiler.renderCompilationError err
                Right code -> putStrLn $ "Compile succeeded: " ++ take 100 code