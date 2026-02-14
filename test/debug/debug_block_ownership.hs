import qualified Parser as P

main :: IO ()
main = do
    let testInput = "package main\nfunc a() { {//! ownership: on\n  } }"
    putStrLn $ "Testing input: " ++ testInput
    case P.parseTypus testInput of
        Right result -> putStrLn $ "Success: " ++ show result
        Left err -> putStrLn $ "Error: " ++ err