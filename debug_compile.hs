import Compiler (compile, renderCompilationError)
import Parser (parseTypus)

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
    case parseTypus source of
        Left err -> putStrLn $ "Parse error: " ++ err
        Right parsed -> do
            case compile parsed of
                Left err -> do
                    putStrLn "Compilation failed:"
                    putStrLn $ renderCompilationError err
                Right goCode -> do
                    putStrLn "Compilation succeeded:"
                    putStrLn goCode