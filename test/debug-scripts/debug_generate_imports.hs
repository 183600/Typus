
module Main where

import Data.List (intercalate)
import Data.Char (isSpace)

-- Simulate the generateImports function
generateImports content = 
  let
    hasFmt = "fmt." `isInfixOf` content || "fmt
" `isInfixOf` content || "\"fmt\"" `isInfixOf` content
    hasMath = "math." `isInfixOf` content || "\"math\"" `isInfixOf` content
    hasTime = "time." `isInfixOf` content || "\"time\"" `isInfixOf` content
    hasOs = "os." `isInfixOf` content || "\"os\"" `isInfixOf` content
    hasIo = "io." `isInfixOf` content || "\"io\"" `isInfixOf` content
    hasStrings = "strings." `isInfixOf` content || "\"strings\"" `isInfixOf` content
    hasSync = "sync." `isInfixOf` content || "\"sync\"" `isInfixOf` content
    hasRuntime = "runtime." `isInfixOf` content || "\"runtime\"" `isInfixOf` content
    hasUnsafe = "unsafe." `isInfixOf` content || "\"unsafe\"" `isInfixOf` content
    imports = filter (not . null) [
        if hasFmt then "    "fmt"" else "",
        if hasMath then "    "math"" else "",
        if hasTime then "    "time"" else "",
        if hasOs then "    "os"" else "",
        if hasIo then "    "io"" else "",
        if hasStrings then "    "strings"" else "",
        if hasSync then "    "sync"" else "",
        if hasRuntime then "    "runtime"" else "",
        if hasUnsafe then "    "unsafe"" else ""
      ]
  in
    if null imports 
      then ""
      else "import (\n" ++ intercalate "\n" imports ++ "\n)"

main :: IO ()
main = do
  -- Read the simple_test.typus file
  content <- readFile "examples/simple_test.typus"
  putStrLn "=== ORIGINAL CONTENT ==="
  putStrLn content
  putStrLn ""
  
  putStrLn "=== ANALYSIS ==="
  putStrLn $ "hasFmt: " ++ show ("fmt." `isInfixOf` content || "fmt
" `isInfixOf` content || "\"fmt\"" `isInfixOf` content)
  putStrLn $ "hasRuntime: " ++ show ("runtime." `isInfixOf` content || "\"runtime\"" `isInfixOf` content)
  putStrLn ""
  
  putStrLn "=== GENERATED IMPORTS ==="
  putStrLn $ generateImports content
