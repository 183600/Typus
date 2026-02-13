import qualified Utils as U

main :: IO ()
main = do
  let s = "aa"
  putStrLn $ "Input s: " ++ show s
  
  let withEscape = "\"" ++ s ++ "\\\""
  putStrLn $ "withEscape: " ++ show withEscape
  putStrLn $ "isProblematicUnclosedString withEscape: " ++ show (U.isProblematicUnclosedString withEscape)
  
  -- 检查isCompleteStringLiteral
  putStrLn $ "\nChecking isCompleteStringLiteral:"
  putStrLn $ "  isCompleteStringLiteral withEscape: " ++ show (U.isCompleteStringLiteral withEscape)
  
  -- 测试期望
  putStrLn $ "\nTest expects isProblematicUnclosedString withEscape to be True"