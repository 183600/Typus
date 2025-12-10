import qualified Compiler
import qualified Parser
import qualified Compiler.IR as IR
import Compiler.GoAst (GoModule(..))
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
    putStrLn $ "Testing source: " ++ source
    let varDecls = extractVarDeclarations source
    putStrLn $ "Found variable declarations: " ++ show (length varDecls)
    mapM_ print varDecls
    case Parser.parseTypus source of
        Left err -> putStrLn $ "Parse error: " ++ err
        Right parsed -> do
            putStrLn "Parse succeeded"
            case IR.moduleFromTypus parsed of
                Left err -> putStrLn $ "Module error: " ++ show err
                Right goModule -> do
                    putStrLn $ "Module succeeded: " ++ show (length (gmDecls goModule)) ++ " declarations"
                    case Compiler.compile parsed of
                        Left err -> putStrLn $ "Compile error: " ++ Compiler.renderCompilationError err
                        Right code -> putStrLn $ "Compile succeeded: " ++ take 100 code