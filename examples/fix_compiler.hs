import System.IO

main :: IO ()
main = do
    -- Read the original file
    contents <- readFile "/home/qwe12345678/typus2/src/Compiler.hs"
    
    -- Find and replace the problematic line
    let oldLine = "          trimmed == \"\\\"strings\\\"\" || trimmed == \"\\\"math\\\"\")"
    let newLine = "          trimmed == \"\\\"strings\\\"\" || trimmed == \"\\\"math\\\"\" || trimmed == \"\\\"runtime\\\"\")"
    
    let newContents = unlines $ map 
            (\line -> if line == oldLine then newLine else line
            ) 
            (lines contents)
    
    -- Write the modified content back
    writeFile "/home/qwe12345678/typus2/src/Compiler.hs" newContents
    
    putStrLn "Fixed Compiler.hs"