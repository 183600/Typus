import qualified Utils as U (isProblematicUnclosedString)

main :: IO ()
main = do
  putStrLn "Testing isProblematicUnclosedString with escape quote:"
  putStrLn $ "isProblematicUnclosedString \"\\\"\": " ++ show (U.isProblematicUnclosedString "\"")
  putStrLn $ "isProblematicUnclosedString \"\\\\\": " ++ show (U.isProblematicUnclosedString "\\")
  putStrLn $ "isProblematicUnclosedString \"\\\"\\\\\\\"\": " ++ show (U.isProblematicUnclosedString "\"\\\"")
  
  -- 测试空字符串情况
  let s = ""
  let withEscape = "\"" ++ s ++ "\\\""
  putStrLn $ "s = " ++ show s
  putStrLn $ "withEscape = " ++ show withEscape
  putStrLn $ "isProblematicUnclosedString withEscape = " ++ show (U.isProblematicUnclosedString withEscape)
  
  -- 额外测试一些边界情况
  putStrLn $ "isProblematicUnclosedString \"\": " ++ show (U.isProblematicUnclosedString "")
  putStrLn $ "isProblematicUnclosedString \"\\\"\\\\\": " ++ show (U.isProblematicUnclosedString "\"\\")