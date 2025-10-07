import Data.List (isInfixOf)

main :: IO ()
main = do
    let content = "package main\n\nimport (\n\t\"fmt\"\n\t\"runtime\"\n)\n\nfunc main() {\n\tfmt.Println(\"Hello, world!\")\n\tfmt.Println(\"Runtime:\", runtime.GOOS)\n}"
    
    let hasRuntime = "runtime." `isInfixOf` content || "\"runtime\"" `isInfixOf` content
    
    putStrLn "Content:"
    putStrLn content
    putStrLn "\nHas runtime.:"
    putStrLn $ show $ "runtime." `isInfixOf` content
    putStrLn "Has \"runtime\":"
    putStrLn $ show $ "\"runtime\"" `isInfixOf` content
    putStrLn "HasRuntime:"
    putStrLn $ show hasRuntime