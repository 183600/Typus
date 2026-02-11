import Utils (removeLineComments)
import Data.List (lines, unlines, all, intercalate, isInfixOf, break)
import Data.Char (isSpace)

main :: IO ()
main = do
  let input = "\n\n"
  putStrLn $ "Input: " ++ show input
  
  putStrLn "\n--- Step-by-step analysis ---"
  let hasNewline = '\n' `elem` input
  putStrLn $ "Has newline: " ++ show hasNewline
  
  if hasNewline
    then do
      let inputLines = lines input
      putStrLn $ "inputLines: " ++ show inputLines
      
      let processedLines = map removeSingleLineComments inputLines
      putStrLn $ "processedLines: " ++ show processedLines
      
      let nullInputLines = null inputLines
      putStrLn $ "nullInputLines: " ++ show nullInputLines
      
      let singleEmptyLine = inputLines == [""]
      putStrLn $ "singleEmptyLine: " ++ show singleEmptyLine
      
      let allNullInputLines = all null inputLines
      putStrLn $ "allNullInputLines: " ++ show allNullInputLines
      
      let hasTrailingNewline = not (null input) && last input == '\n'
      putStrLn $ "hasTrailingNewline: " ++ show hasTrailingNewline
      
      let result = if nullInputLines
                   then ""  
                   else if singleEmptyLine
                        then "\n"  
                        else if allNullInputLines
                             then "\n"  
                             else if hasTrailingNewline
                                  then unlines processedLines
                                  else intercalate "\n" processedLines
      putStrLn $ "Calculated result: " ++ show result
    else
      putStrLn "No newline found"
      
  putStrLn "\n--- Original function ---"
  let originalResult = removeLineComments input
  putStrLn $ "Original result: " ++ show originalResult

-- Simplified version of removeSingleLineComments
removeSingleLineComments :: String -> String
removeSingleLineComments [] = []
removeSingleLineComments ('"':xs) = '"' : removeSingleLineComments xs
removeSingleLineComments ('\'':xs) = '\'' : removeSingleLineComments xs
removeSingleLineComments ('/':'/':xs) = ""
removeSingleLineComments (c:cs) = c : removeSingleLineComments cs