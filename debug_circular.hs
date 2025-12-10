import Parser
import Compiler

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
    case Parser.parseTypus source of
        Left err -> putStrLn $ "Parse error: " ++ err
        Right typusFile -> do
            case Compiler.compile typusFile of
                Left err -> do
                    let rendered = Compiler.renderCompilationError err
                    putStrLn $ "Compilation error: " ++ rendered
                    putStrLn $ "Contains 'circular': " ++ show ("circular" `isInfixOf` rendered)
                    putStrLn $ "Contains 'cycle': " ++ show ("cycle" `isInfixOf` rendered)
                Right goCode -> putStrLn $ "Compilation succeeded: " ++ goCode