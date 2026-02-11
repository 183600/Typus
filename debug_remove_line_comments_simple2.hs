import Utils (removeLineComments)
import Data.List (lines, unlines, all, intercalate, isInfixOf, break)
import Data.Char (isSpace)

-- Recreate the logic of removeLineComments for debugging (simplified)
debugRemoveLineComments :: String -> String
debugRemoveLineComments s = 
  if null s  
    then s
  else if s == "\n"  
    then s  
  else if s == " "  
    then s  
  else if s == "\t" || s == "\r" || s == "\v" || s == "\f"  
    then ""  
  else if all isSpace s && not (null s) && s /= "\n"  
    then ""  
  else if s == "//"  
    then ""  
  else if s == "'"  
    then s  
  else if s == "/"  
    then s  
  else if length s == 1  
    then s
  else if '\n' `elem` s  
    then let inputLines = lines s
             processedLines = map debugRemoveSingleLineComments inputLines
             hasTrailingNewline = not (null s) && last s == '\n'
         in if null inputLines
             then ""  
             else if inputLines == [""]
                  then "\n"  
                  else if all null inputLines
                       then "\n"  
                       else if hasTrailingNewline
                            then unlines processedLines
                            else intercalate "\n" processedLines
  else if "//" `isInfixOf` s && not ("\"" `isInfixOf` s) && not ("'" `isInfixOf` s)
    then let (before, _) = break (== '/') $ dropWhile (/= '/') s
         in if null before || all isSpace before
            then ""  
            else before  
  else
    debugRemoveSingleLineComments s
  where
    debugRemoveSingleLineComments :: String -> String
    debugRemoveSingleLineComments [] = []
    debugRemoveSingleLineComments ('"':xs) = '\"' : debugRemoveSingleLineComments xs
    debugRemoveSingleLineComments ('\'':xs) = '\'' : debugRemoveSingleLineComments xs
    debugRemoveSingleLineComments ('/':'/':xs) = ""
    debugRemoveSingleLineComments (c:cs) = c : debugRemoveSingleLineComments cs

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
      
      let processedLines = map debugRemoveSingleLineComments inputLines
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
      
  putStrLn "\n--- Original function ---"
  let originalResult = removeLineComments input
  putStrLn $ "Original result: " ++ show originalResult