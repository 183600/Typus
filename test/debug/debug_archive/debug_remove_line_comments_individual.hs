import Data.List (intercalate)

main :: IO ()
main = do
  let s = "code\n// comment1\n// comment2\nmore code"
      inputLines = lines s
      removeSingleLineComments [] = []
      removeSingleLineComments ('"':xs) = '\"' : removeSingleLineComments xs
      removeSingleLineComments ('\'':xs) = '\'' : removeSingleLineComments xs
      removeSingleLineComments ('/':'/':xs) = ""
      removeSingleLineComments (c:cs) = c : removeSingleLineComments cs
      processedLines = map removeSingleLineComments inputLines
      hasTrailingNewline = not (null s) && last s == '\n'
      result = if null inputLines || inputLines == [""]
               then ""
               else if hasTrailingNewline
                    then unlines processedLines
                    else intercalate "\n" processedLines
  putStrLn $ "s: " ++ show s
  putStrLn $ "inputLines: " ++ show inputLines
  putStrLn $ "processedLines: " ++ show processedLines
  putStrLn $ "hasTrailingNewline: " ++ show hasTrailingNewline
  putStrLn $ "intercalate \"\\n\" processedLines: " ++ show (intercalate "\n" processedLines)
  putStrLn $ "result: " ++ show result
  
  -- Test each line individually
  mapM_ (\line -> putStrLn $ "removeSingleLineComments " ++ show line ++ " = " ++ show (removeSingleLineComments line)) inputLines