import Data.List (isInfixOf)

main :: IO ()
main = do
  let input = "    if condition {\n        // do something\n        return \n    }\n"
  let isCodeBlock = any (`isInfixOf` input) ["if condition", "func outer", "func inner", "return", "{", "}", "//"]
  putStrLn $ "input: " ++ show input
  putStrLn $ "isCodeBlock: " ++ show isCodeBlock
  putStrLn $ "contains 'if condition': " ++ show ("if condition" `isInfixOf` input)
  putStrLn $ "contains 'return': " ++ show ("return" `isInfixOf` input)
  putStrLn $ "contains '{': " ++ show ("{" `isInfixOf` input)
  putStrLn $ "contains '}': " ++ show ("}" `isInfixOf` input)
  putStrLn $ "contains '//': " ++ show ("//" `isInfixOf` input)