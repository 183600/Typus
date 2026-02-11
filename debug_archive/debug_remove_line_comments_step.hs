import Utils (removeLineComments)
import Data.List (lines, unlines)

-- Step by step debugging of removeLineComments
main :: IO ()
main = do
  putStrLn "=== Step by step debugging of removeLineComments ==="
  
  let input = "\n\n"
  putStrLn $ "Input: " ++ show input
  
  putStrLn "\n--- Step 1: Check if '\n' is in input ---"
  let hasNewline = '\n' `elem` input
  putStrLn $ "'\n' `elem` input: " ++ show hasNewline
  
  if hasNewline
    then do
      putStrLn "\n--- Step 2: Split into lines ---"
      let inputLines = lines input
      putStrLn $ "inputLines = lines input: " ++ show inputLines
      putStrLn $ "length inputLines: " ++ show (length inputLines)
      
      putStrLn "\n--- Step 3: Process each line with removeSingleLineComments ---"
      -- We need to test what removeSingleLineComments does to empty strings
      let testEmpty = removeLineComments ""
      putStrLn $ "removeLineComments \"\": " ++ show testEmpty
      
      -- Since we can't directly access removeSingleLineComments, let's test with single lines
      let testLine1 = removeLineComments ""
      let testLine2 = removeLineComments ""
      putStrLn $ "removeLineComments \"\" (line 1): " ++ show testLine1
      putStrLn $ "removeLineComments \"\" (line 2): " ++ show testLine2
      
      putStrLn "\n--- Step 4: Check conditions ---"
      let nullInputLines = null inputLines
      putStrLn $ "null inputLines: " ++ show nullInputLines
      
      let singleEmptyLine = inputLines == [""]
      putStrLn $ "inputLines == [\"\"]: " ++ show singleEmptyLine
      
      let allNullInputLines = all null inputLines
      putStrLn $ "all null inputLines: " ++ show allNullInputLines
      
      putStrLn "\n--- Step 5: Check trailing newline ---"
      let hasTrailingNewline = not (null input) && last input == '\n'
      putStrLn $ "not (null input) && last input == '\n': " ++ show hasTrailingNewline
      
      putStrLn "\n--- Step 6: Final result ---"
      let result = removeLineComments input
      putStrLn $ "Final result: " ++ show result
      
    else do
      putStrLn "Input doesn't contain newline, would go to single line processing"
      
  putStrLn "\n=== Additional test: What does unlines [\"\",\"\"] produce? ==="
  let testUnlines = unlines [""]
  putStrLn $ "unlines [\"\",\"\"]: " ++ show testUnlines
  
  putStrLn "\n=== Additional test: What does lines \"\\n\\n\" produce? ==="
  let testLines = lines "\n\n"
  putStrLn $ "lines \"\\n\\n\": " ++ show testLines