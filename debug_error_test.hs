import Test.HUnit
import qualified Compiler
import Parser

test :: Test
test = TestCase $ do
    let source = unlines
          [ "package main"
          , "func expectInt(x int) int { return x }"
          , "func main() {"
          , "    expectInt(\"string\")"
          , "}"
          ]
    case Parser.parseTypus source of
        Left err -> assertFailure ("parseTypus failed: " ++ err)
        Right parsed -> 
            case Compiler.compile parsed of
                Left err -> do
                    let rendered = Compiler.renderCompilationError err
                    putStrLn "=== Error output ==="
                    putStrLn rendered
                    putStrLn "=== End error output ==="
                    assertBool "should mention expected type" ("expected.*int" `isInfixOf` rendered)
                    assertBool "should mention actual type" ("got.*string" `isInfixOf` rendered)
                Right _ -> assertFailure "expected type mismatch error"

main :: IO ()
main = do
    result <- runTestTT test
    return ()