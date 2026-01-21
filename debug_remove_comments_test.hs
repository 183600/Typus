import Utils
import Data.List (isPrefixOf, isSuffixOf)

main :: IO ()
main = do
  let stringWithComment = "code /* comment */ more code"
  let result = removeComments stringWithComment
  putStrLn $ "Input: " ++ show stringWithComment
  putStrLn $ "Result: " ++ show result
  putStrLn $ "Starts with 'code': " ++ show ("code" `isPrefixOf` result)
  putStrLn $ "Ends with 'more code ': " ++ show ("more code " `isSuffixOf` result)