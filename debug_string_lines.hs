import Data.List (lines)

main :: IO ()
main = do
  let s = "\r\n"
  putStrLn $ "Input s: " ++ show s
  putStrLn $ "Length of s: " ++ show (length s)
  putStrLn $ "Chars in s: " ++ show (zip [0..] s)
  putStrLn $ "lines s: " ++ show (lines s)
  
  -- 测试期望
  putStrLn $ "\nTest expects lines s to return [\"\"]"