import Parser

main :: IO ()
main = do
    let sourceWithErrors = unlines $ concat
          [ ["package main"]
          , ["func undefined() {}"]  
          , ["func main() {"]
          , ["    undefined("]       -- Unclosed parenthesis will cause parsing error
          , ["}"]
          ]
    case Parser.parseTypus sourceWithErrors of
        Left err -> putStrLn $ "Parsing failed as expected: " ++ err
        Right _ -> putStrLn "Parsing unexpectedly succeeded"