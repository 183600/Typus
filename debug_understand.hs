import Data.Char (ord)

-- 测试理解问题
main :: IO ()
main = do
  let input = "//\""
  putStrLn $ "Input: " ++ show input
  putStrLn $ "Length: " ++ show (length input)
  putStrLn $ "Chars: " ++ show (map (\c -> (c, show (ord c))) input)
  
  putStrLn "\nPattern matching:"
  putStrLn $ "startsWith //? " ++ show (take 2 input == "//")
  putStrLn $ "Third char is \\? " ++ show (if length input > 2 then input !! 2 == '\\' else False)
  putStrLn $ "Fourth char is \" ? " ++ show (if length input > 3 then input !! 3 == '\"' else False)