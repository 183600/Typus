import Utils (removeLineComments)

main :: IO ()
main = do
  let input1 = "// comment1"
      input2 = "code"
      result1 = removeLineComments input1
      result2 = removeLineComments input2
  putStrLn $ "input1: " ++ show input1
  putStrLn $ "result1: " ++ show result1
  putStrLn $ "input2: " ++ show input2
  putStrLn $ "result2: " ++ show result2
  
  let inputLines = ["code", "// comment1", "// comment2", "more code"]
      processedLines = map removeLineComments inputLines
  putStrLn $ "inputLines: " ++ show inputLines
  putStrLn $ "processedLines: " ++ show processedLines
  putStrLn $ "unlines processedLines: " ++ show (unlines processedLines)