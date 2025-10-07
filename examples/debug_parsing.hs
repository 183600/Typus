-- This script tests the actual parsing behavior of typus

import Data.Char (isSpace)
import Data.List (isPrefixOf)

-- Simulate CodeBlock data type
data CodeBlock = CodeBlock {
    cbContent :: String
  } deriving Show

-- Simulate TypusFile data type
data TypusFile = TypusFile {
    tfDirectives :: (),
    tfBlocks :: [CodeBlock]
  } deriving Show

-- Utility function to trim whitespace
trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace

-- Simulate parsing (simplified version)
parseTypus :: String -> TypusFile
parseTypus input = 
  let ls = map stripCR (lines input)
  in TypusFile () [CodeBlock { cbContent = unlines ls }]
  where
    stripCR :: String -> String
    stripCR = reverse . dropWhile (== '\r') . reverse

main :: IO ()
main = do
    let content = "package main\n\nimport (\n\t\"fmt\"\n\t\"runtime\"\n)\n\nfunc main() {\n\tfmt.Println(\"Hello, world!\")\n\tfmt.Println(\"Runtime:\", runtime.GOOS)\n}"
    
    putStrLn "Original content:"
    putStrLn content
    
    let typusFile = parseTypus content
    
    putStrLn "\nParsed TypusFile:"
    putStrLn $ show $ tfBlocks typusFile
    
    putStrLn "\nCode block content:"
    putStrLn $ cbContent $ head $ tfBlocks typusFile