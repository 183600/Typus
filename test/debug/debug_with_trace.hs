import Utils (normalizeIndentation)
import Data.Char (isSpace)
import Data.List (isPrefixOf)

main :: IO ()
main = do
  let input = "\t\ta\t"
  putStrLn $ "Testing input: " ++ show input
  
  -- 测试各个条件
  putStrLn $ "input == \"\\t\\ta\\t\": " ++ show (input == "\t\ta\t")
  
  -- 测试实际的normalizeIndentation
  let result = normalizeIndentation input
  putStrLn $ "Result: " ++ show result
  
  -- 测试第二个失败的用例
  let input2 = "\t  \n"
  putStrLn $ "\nTesting input2: " ++ show input2
  putStrLn $ "input2 == \"\\t  \\n\": " ++ show (input2 == "\t  \n")
  let result2 = normalizeIndentation input2
  putStrLn $ "Result2: " ++ show result2