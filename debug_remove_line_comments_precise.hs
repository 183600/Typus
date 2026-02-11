import Utils (removeLineComments)
import Data.List (lines, unlines, all, intercalate, isInfixOf)
import Data.List.Extra (breakOn)
import Data.Char (isSpace)

-- Recreate the logic of removeLineComments for debugging
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
    then let (before, _) = breakOn "//" s
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

-- Import required functions

main :: IO ()
main = do
  let input = "\n\n"
  putStrLn $ "Input: " ++ show input
  
  putStrLn "\n--- Debug recreate logic ---"
  let debugResult = debugRemoveLineComments input
  putStrLn $ "Debug result: " ++ show debugResult
  
  putStrLn "\n--- Original function ---"
  let originalResult = removeLineComments input
  putStrLn $ "Original result: " ++ show originalResult
  
  putStrLn $ "Results match: " ++ show (debugResult == originalResult)