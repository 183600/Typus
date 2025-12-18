import Test.HUnit
import Compiler
import Parser
import qualified Data.Text as T

main :: IO ()
main = do
    let source = unlines
          [ "package main"
          , "func a() {"
          , "    b()"
          , "}"
          , "func b() {"
          , "    a()"
          , "}"
          , "func main() {"
          , "    a()"
          , "}"
          ]
    let typusFile = Parser.TypusFile (T.pack source) "test.typus"
    case Parser.parseTypus source of
        Left err -> assertFailure ("parseTypus failed: " ++ err)
        Right parsed -> 
            case Compiler.compile parsed of
                Left err -> do
                    let rendered = Compiler.renderCompilationError err
                    putStrLn $ "Compilation error: " ++ rendered
                    assertBool "should detect circular dependency" ("circular" `isInfixOf` rendered || "cycle" `isInfixOf` rendered)
                Right _ -> assertFailure "expected circular dependency error"