import qualified Utils as U
import Data.List (lines)

-- 测试 removeLineComments 的行为
testRemoveLineComments :: IO ()
testRemoveLineComments = do
  let lines' = ["\n"]
  let code = unlines lines'
  let processed = U.removeLineComments code
  let procLines = lines processed
  putStrLn $ "Input lines': " ++ show lines'
  putStrLn $ "code: " ++ show code
  putStrLn $ "processed: " ++ show processed
  putStrLn $ "procLines: " ++ show procLines
  putStrLn $ "Expected length: 1"
  putStrLn $ "Actual length: " ++ show (length procLines)
  
  -- 测试 preserveLineCount 的行为
  putStrLn "\n=== Testing preserveLineCount directly ==="
  let inputLines = lines code
  putStrLn $ "inputLines: " ++ show inputLines
  let ifTwoEmptyLines = case inputLines of
                          ["", ""] -> True
                          _ -> False
  putStrLn $ "ifTwoEmptyLines: " ++ show ifTwoEmptyLines
  
  -- 测试条件顺序
  putStrLn "\n=== Testing condition order ==="
  putStrLn $ "code == \"\\n\": " ++ show (code == "\n")
  putStrLn $ "ifTwoEmptyLines: " ++ show ifTwoEmptyLines
  let ifSingleNewline = case inputLines of
                          [] -> False
                          [""] -> code == "\n"
                          _ -> False
  putStrLn $ "ifSingleNewline: " ++ show ifSingleNewline
  putStrLn $ "Should return \"\\n\": " ++ show (ifTwoEmptyLines)

main :: IO ()
main = testRemoveLineComments