import Data.Char (isSpace)

main :: IO ()
main = do
  let input1 = "    a\n    b\n    c\n"
  
  putStrLn $ "Test 1 checks:"
  putStrLn $ "null input1: " ++ show (null input1)
  putStrLn $ "length input1 == 1: " ++ show (length input1 == 1)
  putStrLn $ "length input1 == 1 && not (isSpace (head input1)): " ++ show (length input1 == 1 && not (isSpace (head input1)))
  
  let inputLines1 = lines input1
  putStrLn $ "length inputLines1 <= 1: " ++ show (length inputLines1 <= 1)
  putStrLn $ "inputLines1: " ++ show inputLines1