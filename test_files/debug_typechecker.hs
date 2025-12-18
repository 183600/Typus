import qualified Parser
import qualified Compiler.TypeChecker as TC
import qualified Compiler.IR as IR

main :: IO ()
main = do
    let source = unlines
          [ "package main"
          , "func add(x int, y int) int {"
          , "    return x + y"
          , "}"
          , "func main() {"
          , "    add(\"oops\", 2)"
          , "}"
          ]
    case Parser.parseTypus source of
        Left err -> putStrLn $ "Parse error: " ++ err
        Right typusFile -> do
            putStrLn "Parse successful"
            case TC.diagnoseTypeErrors typusFile of
                Left errs -> putStrLn $ "Type errors: " ++ show errs
                Right diagnostics -> putStrLn $ "Diagnostics: " ++ show diagnostics