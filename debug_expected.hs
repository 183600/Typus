import Data.List (isPrefixOf, isSuffixOf)

main :: IO ()
main = do
  let input = "code /* comment */ more code"
  let expected = "code more code "  -- 假设的期望结果
  putStrLn $ "Input: " ++ show input
  putStrLn $ "Expected: " ++ show expected
  putStrLn $ "Starts with 'code': " ++ show ("code" `isPrefixOf` expected)
  putStrLn $ "Ends with 'more code ': " ++ show ("more code " `isSuffixOf` expected)