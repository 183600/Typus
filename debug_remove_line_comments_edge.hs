import Utils (removeLineComments)

main :: IO ()
main = do
  let input = "code\n// comment1\n// comment2\nmore code"
      result = removeLineComments input
      inputLines = lines input
      resultLines = lines result
  putStrLn $ "input: " ++ show input
  putStrLn $ "inputLines: " ++ show inputLines
  putStrLn $ "result: " ++ show result
  putStrLn $ "resultLines: " ++ show resultLines
  putStrLn $ "expected: \"code\\n\\n\\nmore code\""