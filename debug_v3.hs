main :: IO ()
main = do
  let input = "\t\t\v\t"
  let middle = drop 2 (init input)
  let result = "  " ++ middle ++ "\t"
  
  putStrLn $ "Input: " ++ show input
  putStrLn $ "init input: " ++ show (init input)
  putStrLn $ "drop 2 (init input): " ++ show middle
  putStrLn $ "Result: " ++ show result