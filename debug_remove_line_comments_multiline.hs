import Utils (removeLineComments)

main :: IO ()
main = do
  let s1 = ""
      s2 = ""
      line1 = s1 ++ "// comment1"
      line2 = s2 ++ "// comment2"
      multiline = line1 ++ "\n" ++ line2
      result = removeLineComments multiline
      linesResult = lines result
      hasContent = any (not . null) [s1, s2]
  
  putStrLn $ "s1: " ++ show s1
  putStrLn $ "s2: " ++ show s2
  putStrLn $ "line1: " ++ show line1
  putStrLn $ "line2: " ++ show line2
  putStrLn $ "multiline: " ++ show multiline
  putStrLn $ "result: " ++ show result
  putStrLn $ "linesResult: " ++ show linesResult
  putStrLn $ "hasContent: " ++ show hasContent
  putStrLn $ "null result: " ++ show (null result)
  putStrLn $ "all null linesResult: " ++ show (all null linesResult)
  putStrLn $ "linesResult == [\"\"]: " ++ show (linesResult == [""])