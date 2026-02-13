import qualified Utils as U

main :: IO ()
main = do
  let s = "a\""
  putStrLn $ "Input s: " ++ show s
  
  let str = "\"" ++ s ++ "\""
  putStrLn $ "str: " ++ show str
  putStrLn $ "isCompleteStringLiteral str: " ++ show (U.isCompleteStringLiteral str)
  
  -- 测试期望
  putStrLn $ "\nTest expects isCompleteStringLiteral str to be True"