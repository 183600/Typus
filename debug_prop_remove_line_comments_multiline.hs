import Utils (removeLineComments)
import Data.List (isPrefixOf)

main :: IO ()
main = do
  -- Test the specific failing case
  let s1 = ""
      s2 = ""
      valid1 = not ('\"' `elem` s1) && not ('\'' `elem` s1) && not ('\\' `elem` s1)
      valid2 = not ('\"' `elem` s2) && not ('\'' `elem` s2) && not ('\\' `elem` s2)
  putStrLn $ "valid1: " ++ show valid1
  putStrLn $ "valid2: " ++ show valid2
  
  if not (valid1 && valid2)
    then putStrLn "Skipping due to invalid characters"
    else do
      let line1 = s1 ++ "// comment1"
          line2 = s2 ++ "// comment2"
          multiline = line1 ++ "\n" ++ line2
          result = removeLineComments multiline
          linesResult = lines result
          hasContent = any (not . null) [s1, s2]
      putStrLn $ "line1: " ++ show line1
      putStrLn $ "line2: " ++ show line2
      putStrLn $ "multiline: " ++ show multiline
      putStrLn $ "result: " ++ show result
      putStrLn $ "linesResult: " ++ show linesResult
      putStrLn $ "hasContent: " ++ show hasContent
      putStrLn $ "result == \"\\n\": " ++ show (result == "\n")
      
      if hasContent
        then putStrLn $ "length linesResult >= 1: " ++ show (length linesResult >= 1) ++ 
                       ", not (any (\"//\" `isPrefixOf`) linesResult): " ++ show (not (any ("//" `isPrefixOf`) linesResult))
        else putStrLn $ "result == \"\\n\": " ++ show (result == "\n")