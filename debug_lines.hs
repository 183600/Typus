import Data.List (lines)

-- 测试 lines 函数的行为
testLines :: IO ()
testLines = do
  let input1 = "\n"
  let input2 = "\n\n"
  let lines1 = lines input1
  let lines2 = lines input2
  putStrLn $ "input1: " ++ show input1
  putStrLn $ "lines1: " ++ show lines1
  putStrLn $ "length lines1: " ++ show (length lines1)
  putStrLn $ "input2: " ++ show input2
  putStrLn $ "lines2: " ++ show lines2
  putStrLn $ "length lines2: " ++ show (length lines2)
  
  -- 测试条件
  let ifTwoEmptyLines1 = case lines1 of
                           ["", ""] -> True
                           _ -> False
  let ifTwoEmptyLines2 = case lines2 of
                           ["", ""] -> True
                           _ -> False
  putStrLn $ "ifTwoEmptyLines1: " ++ show ifTwoEmptyLines1
  putStrLn $ "ifTwoEmptyLines2: " ++ show ifTwoEmptyLines2

main :: IO ()
main = testLines