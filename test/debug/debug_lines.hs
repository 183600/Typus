main :: IO ()
main = do
  let input = "\t  \n7\n"
  let inputLines = lines input
  putStrLn $ "Input: " ++ show input
  putStrLn $ "Input lines: " ++ show inputLines
  putStrLn $ "Number of input lines: " ++ show (length inputLines)
  
  let output = "   \n7\n"
  let outputLines = lines output
  putStrLn $ "Output: " ++ show output
  putStrLn $ "Output lines: " ++ show outputLines
  putStrLn $ "Number of output lines: " ++ show (length outputLines)