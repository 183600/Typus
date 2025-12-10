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
    case Parser.parseTypus source of
        Left err -> putStrLn $ "parse error: " ++ err
        Right parsed -> do
            case Compiler.compile parsed of
                Left err -> putStrLn $ "compile error: " ++ Compiler.renderCompilationError err
                Right code -> putStrLn $ "Success: " ++ take 100 code