import Utils (removeLineComments)

-- Test the failing case
main :: IO ()
main = do
  let s1 = ""
      s2 = ""
      line1 = s1 ++ "// comment1"
      line2 = s2 ++ "// comment2"
      multiline = line1 ++ "\n" ++ line2
      
  putStrLn $ "s1: " ++ show s1
  putStrLn $ "s2: " ++ show s2
  putStrLn $ "line1: " ++ show line1
  putStrLn $ "line2: " ++ show line2
  putStrLn $ "multiline: " ++ show multiline
  
  let result = removeLineComments multiline
  putStrLn $ "result: " ++ show result
  putStrLn $ "result == \"\\n\": " ++ show (result == "\n")
  
  let linesResult = lines result
  putStrLn $ "linesResult: " ++ show linesResult
  putStrLn $ "length linesResult: " ++ show (length linesResult)