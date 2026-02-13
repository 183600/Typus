import Utils (normalizeIndentation)
import Data.List (isPrefixOf)

main :: IO ()
main = do
  -- 测试新的失败用例 "b"
  let input1 = "b"
  let withTabs1 = "\t\t" ++ input1 ++ "\t"
  let result1 = normalizeIndentation withTabs1
  putStrLn $ "Input1: " ++ show input1
  putStrLn $ "With tabs1: " ++ show withTabs1
  putStrLn $ "Result1: " ++ show result1
  putStrLn $ "Expected1: not (\"\\t\\t\" `isPrefixOf` result1)"
  putStrLn $ "Actual result: " ++ show (not ("\t\t" `isPrefixOf` result1))
  
  -- 测试新的失败用例 ["\n"]
  let input2 = ["\n"]
  let withMixed2 = map ("\t  " ++) input2
  let normalized2 = normalizeIndentation (unlines withMixed2)
  putStrLn $ "\nInput2: " ++ show input2
  putStrLn $ "With mixed2: " ++ show withMixed2
  putStrLn $ "Unlines with mixed2: " ++ show (unlines withMixed2)
  putStrLn $ "Normalized2: " ++ show normalized2
  putStrLn $ "Expected2: \"\\n\""
  putStrLn $ "Actual result: " ++ show (normalized2 == "\n")