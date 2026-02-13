-- This script tests why hasRuntime is False in actual typus

import Data.List (isInfixOf)

main :: IO ()
main = do
    -- This is what the entire file looks like
    let fullContent = "package main\n\nimport (\n\t\"fmt\"\n\t\"runtime\"\n)\n\nfunc main() {\n\tfmt.Println(\"Hello, world!\")\n\tfmt.Println(\"Runtime:\", runtime.GOOS)\n}"
    
    -- This is what the code blocks look like (without the import block)
    let codeBlocksContent = "func main() {\n\tfmt.Println(\"Hello, world!\")\n\tfmt.Println(\"Runtime:\", runtime.GOOS)\n}"
    
    putStrLn "Full content:"
    putStrLn fullContent
    putStrLn "\nCode blocks content:"
    putStrLn codeBlocksContent
    
    let hasRuntimeInFull = "runtime." `isInfixOf` fullContent || "\"runtime\"" `isInfixOf` fullContent
    let hasRuntimeInBlocks = "runtime." `isInfixOf` codeBlocksContent || "\"runtime\"" `isInfixOf` codeBlocksContent
    
    putStrLn "\nHas runtime in full content:"
    putStrLn $ show hasRuntimeInFull
    putStrLn "Has runtime in code blocks content:"
    putStrLn $ show hasRuntimeInBlocks