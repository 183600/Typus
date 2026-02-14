import Parser

main :: IO ()
main = do
    let result = parseTypus "func () -> int where { }"
    putStrLn $ show result