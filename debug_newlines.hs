import Data.List

main :: IO ()
main = do
  let input = "\n "
      processedLines = ["", ""]
      result1 = unlines processedLines
      result2 = intercalate "\n" processedLines
  
  putStrLn $ "Input: " ++ show input
  putStrLn $ "Input lines: " ++ show (lines input)
  putStrLn $ "Processed lines: " ++ show processedLines
  putStrLn $ "unlines result: " ++ show result1
  putStrLn $ "unlines result lines: " ++ show (lines result1)
  putStrLn $ "intercalate result: " ++ show result2
  putStrLn $ "intercalate result lines: " ++ show (lines result2)
  putStrLn $ "Input ends with newline: " ++ show (not (null input) && last input == '\n')
  putStrLn $ "Input length: " ++ show (length input)