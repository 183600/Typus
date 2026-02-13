import Utils (normalizeIndentation)
import Data.List (isPrefixOf)

main :: IO ()
main = do
  -- 测试 "\STX"
  let input1 = "\STX"
  let withTabs1 = "\t\t" ++ input1 ++ "\t"
  let result1 = normalizeIndentation withTabs1
  putStrLn $ "Input1: " ++ show input1
  putStrLn $ "With tabs1: " ++ show withTabs1
  putStrLn $ "Result1: " ++ show result1
  putStrLn $ "Expected1: not (\"\\t\\t\" `isPrefixOf` result1)"
  putStrLn $ "Actual result: " ++ show (not ("\t\t" `isPrefixOf` result1))
  
  -- 测试 "aa"
  let input2 = "aa"
  let withTabs2 = "\t\t" ++ input2 ++ "\t"
  let result2 = normalizeIndentation withTabs2
  putStrLn $ "\nInput2: " ++ show input2
  putStrLn $ "With tabs2: " ++ show withTabs2
  putStrLn $ "Result2: " ++ show result2
  putStrLn $ "Expected2: not (\"\\t\\t\" `isPrefixOf` result2)"
  putStrLn $ "Actual result: " ++ show (not ("\t\t" `isPrefixOf` result2))
  
  -- 测试 "\FS"
  let input3 = "\FS"
  let withTabs3 = "\t\t" ++ input3 ++ "\t"
  let result3 = normalizeIndentation withTabs3
  putStrLn $ "\nInput3: " ++ show input3
  putStrLn $ "With tabs3: " ++ show withTabs3
  putStrLn $ "Result3: " ++ show result3
  putStrLn $ "Expected3: not (\"\\t\\t\" `isPrefixOf` result3)"
  putStrLn $ "Actual result: " ++ show (not ("\t\t" `isPrefixOf` result3))
  
  -- 测试 "\v"
  let input4 = "\v"
  let withTabs4 = "\t\t" ++ input4 ++ "\t"
  let result4 = normalizeIndentation withTabs4
  putStrLn $ "\nInput4: " ++ show input4
  putStrLn $ "With tabs4: " ++ show withTabs4
  putStrLn $ "Result4: " ++ show result4
  putStrLn $ "Expected4: not (\"\\t\\t\" `isPrefixOf` result4)"
  putStrLn $ "Actual result: " ++ show (not ("\t\t" `isPrefixOf` result4))