import Utils
import Data.List (isPrefixOf, isSuffixOf)

main :: IO ()
main = do
  let s = "a"  -- 测试输入
  let stringWithComment = "code /* comment */ more code"
  let result = removeComments stringWithComment
  
  putStrLn $ "Test input s: " ++ show s
  putStrLn $ "String with comment: " ++ show stringWithComment
  putStrLn $ "Result: " ++ show result
  putStrLn $ "not (null s): " ++ show (not (null s))
  putStrLn $ "\"code\" `isPrefixOf` result: " ++ show ("code" `isPrefixOf` result)
  putStrLn $ "\"more code \" `isSuffixOf` result: " ++ show ("more code " `isSuffixOf` result)
  putStrLn $ "Test passes: " ++ show (not (null s) && "code" `isPrefixOf` result && "more code " `isSuffixOf` result)