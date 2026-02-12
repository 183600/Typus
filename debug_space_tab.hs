import Utils (normalizeIndentation)
import Data.List (isPrefixOf)

main :: IO ()
main = do
  -- 测试空格
  let input1 = " "
  let withTabs1 = "\t\t" ++ input1 ++ "\t"
  let result1 = normalizeIndentation withTabs1
  putStrLn $ "Input1: " ++ show input1
  putStrLn $ "With tabs1: " ++ show withTabs1
  putStrLn $ "Result1: " ++ show result1
  putStrLn $ "Expected1: not (\t\t `isPrefixOf` result1)"
  putStrLn $ "Actual result: " ++ show (not ("\t\t" `isPrefixOf` result1))
  
  -- 测试制表符
  let input2 = "\t"
  let withTabs2 = "\t\t" ++ input2 ++ "\t"
  let result2 = normalizeIndentation withTabs2
  putStrLn $ "\nInput2: " ++ show input2
  putStrLn $ "With tabs2: " ++ show withTabs2
  putStrLn $ "Result2: " ++ show result2
  putStrLn $ "Expected2: not (\t\t `isPrefixOf` result2)"
  putStrLn $ "Actual result: " ++ show (not ("\t\t" `isPrefixOf` result2))