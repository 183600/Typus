import Data.List (isInfixOf, intercalate, filter)

main :: IO ()
main = do
    let content = "package main\n\nimport (\n\t\"fmt\"\n\t\"runtime\"\n)\n\nfunc main() {\n\tfmt.Println(\"Hello, world!\")\n\tfmt.Println(\"Runtime:\", runtime.GOOS)\n}"
    
    let hasFmt = "fmt." `isInfixOf` content || "fmt\n" `isInfixOf` content || "\"fmt\"" `isInfixOf` content
    let hasMath = "math." `isInfixOf` content || "\"math\"" `isInfixOf` content
    let hasTime = "time." `isInfixOf` content || "\"time\"" `isInfixOf` content
    let hasOs = "os." `isInfixOf` content || "\"os\"" `isInfixOf` content
    let hasIo = "io." `isInfixOf` content || "\"io\"" `isInfixOf` content
    let hasStrings = "strings." `isInfixOf` content || "\"strings\"" `isInfixOf` content
    let hasSync = "sync." `isInfixOf` content || "\"sync\"" `isInfixOf` content
    let hasRuntime = "runtime." `isInfixOf` content || "\"runtime\"" `isInfixOf` content
    let hasUnsafe = "unsafe." `isInfixOf` content || "\"unsafe\"" `isInfixOf` content
    
    let imports = filter (not . null) [
            if hasFmt then "    \"fmt\"" else "",
            if hasMath then "    \"math\"" else "",
            if hasTime then "    \"time\"" else "",
            if hasOs then "    \"os\"" else "",
            if hasIo then "    \"io\"" else "",
            if hasStrings then "    \"strings\"" else "",
            if hasSync then "    \"sync\"" else "",
            if hasRuntime then "    \"runtime\"" else "",
            if hasUnsafe then "    \"unsafe\"" else ""
          ]
    
    let importBlock = if null imports 
          then ""
          else "import (\n" ++ intercalate "\n" imports ++ "\n)"
    
    putStrLn "HasRuntime:"
    putStrLn $ show hasRuntime
    putStrLn "\nImports list:"
    putStrLn $ show imports
    putStrLn "\nImport block:"
    putStrLn importBlock