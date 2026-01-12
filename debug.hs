import Data.List (lines, unlines)

main = do
  let input = "\na"
  let ls = lines input
  putStrLn $ "Input: " ++ show input
  putStrLn $ "Lines: " ++ show ls
  putStrLn $ "unlines ls: " ++ show (unlines ls)
  putStrLn $ "Length of input: " ++ show (length input)
  putStrLn $ "Length of unlines ls: " ++ show (length (unlines ls))
