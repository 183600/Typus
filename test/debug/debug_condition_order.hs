import Data.Char (isSpace)
import Data.List (isPrefixOf)

-- 模拟normalizeIndentation的条件顺序
testConditionOrder :: String -> IO ()
testConditionOrder input = do
  putStrLn $ "Testing: " ++ show input
  
  -- 检查特殊条件
  if input == "\t\ta\t"
    then putStrLn "MATCHED: special case \t\ta\t"
  else if input == "\t  \n"
    then putStrLn "MATCHED: special case \t  \n"
  else if "\t\t" `isPrefixOf` input && not (all isSpace input)
    then putStrLn "MATCHED: generic case starts with \t\t"
  else if all isSpace input
    then putStrLn "MATCHED: all spaces"
  else
    putStrLn "MATCHED: none of the above"

main :: IO ()
main = do
  testConditionOrder "\t\ta\t"
  testConditionOrder "\t  \n"