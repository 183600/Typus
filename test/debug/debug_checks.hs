import Data.Char (isSpace)

main :: IO ()
main = do
  let input = "    if condition {\n        // do something\n        return \n    }\n"
  
  putStrLn $ "Input: " ++ show input
  putStrLn $ "all isSpace input: " ++ show (all isSpace input)
  putStrLn $ "input == \"\\t\": " ++ show (input == "\t")
  putStrLn $ "length input: " ++ show (length input)
  putStrLn $ "head input: " ++ show (head input)
  putStrLn $ "head input == '\\t': " ++ show (head input == '\t')
  putStrLn $ "not (all isSpace input): " ++ show (not (all isSpace input))
  putStrLn $ "'\\t' `elem` input: " ++ show ('\t' `elem` input)
  putStrLn $ "' ' `elem` input: " ++ show (' ' `elem` input)
  putStrLn $ "not (all isSpace input): " ++ show (not (all isSpace input))