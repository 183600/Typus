import Utils (removeLineComments)

main :: IO ()
main = do
  let input = "code\n// comment1\n// comment2\nmore code"
      result = removeLineComments input
  putStrLn $ "input: " ++ show input
  putStrLn $ "result: " ++ show result
  putStrLn $ "expected: \"code\\n\\n\\nmore code\""
  
  -- Test step by step
  let inputLines = lines input
  putStrLn $ "inputLines: " ++ show inputLines
  
  -- Test each line with removeLineComments
  mapM_ (\line -> putStrLn $ "removeLineComments " ++ show line ++ " = " ++ show (removeLineComments line)) inputLines