import Data.Char (isSpace)

-- Test the exact bytes
main :: IO ()
main = do
  let input = "\t  \n\t  \n"
  let expected = "\t  \n\t  \n"
  
  putStrLn $ "Input bytes: " ++ show (map fromEnum input)
  putStrLn $ "Expected bytes: " ++ show (map fromEnum expected)
  putStrLn $ "Input == Expected: " ++ show (input == expected)
  
  -- Check if there are any hidden characters
  putStrLn $ "Input length: " ++ show (length input)
  putStrLn $ "Expected length: " ++ show (length expected)