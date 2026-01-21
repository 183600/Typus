main :: IO ()
main = do
  let input = "code /* comment */ more code"
  putStrLn $ "Input: " ++ show input
  putStrLn $ "Length: " ++ show (length input)
  putStrLn $ "Chars with indices:"
  mapM_ (\(i, c) -> putStrLn $ show i ++ ": " ++ show c) (zip [0..] input)
  
  putStrLn "\n--- Analysis ---"
  putStrLn $ "code (0-3): " ++ show (take 4 input)
  putStrLn $ "space (4): " ++ show (input !! 4)
  putStrLn $ "/* (5-6): " ++ show (take 2 (drop 5 input))
  putStrLn $ " comment  (7-15): " ++ show (take 9 (drop 7 input))
  putStrLn $ "*/ (16-17): " ++ show (take 2 (drop 16 input))
  putStrLn $ " space (18): " ++ show (input !! 18)
  putStrLn $ "more code (19-): " ++ show (drop 19 input)