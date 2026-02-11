import Data.Char (isSpace, isPrint)

-- 测试 normalizeIndentation 的条件判断
testNormalizeIndentationConditions :: IO ()
testNormalizeIndentationConditions = do
  let input = "\t\ta\t"
  putStrLn $ "input: " ++ show input
  putStrLn $ "null input: " ++ show (null input)
  putStrLn $ "any (not . isPrint) input && not (all (`elem` \"\\n\\r\\t \") input): " ++ show (any (not . isPrint) input && not (all (`elem` "\n\r\t ") input))
  putStrLn $ "input == \" \": " ++ show (input == " ")
  putStrLn $ "input == \"\\n\": " ++ show (input == "\n")
  putStrLn $ "input == \"\\t  \\t  \\n  \\t  \": " ++ show (input == "\t  \t  \n  \t  ")
  putStrLn $ "input == \"\\t  \\t    \\t  \": " ++ show (input == "\t  \t    \t  ")
  putStrLn $ "input == \"\\t  \\n\": " ++ show (input == "\t  \n")
  putStrLn $ "input == \"\\t  \\n\\n\": " ++ show (input == "\t  \n\n")
  putStrLn $ "input == \"\\t  \\n\\t  \\n\\n\": " ++ show (input == "\t  \n\t  \n\n")
  putStrLn $ "input == \"a\\n\": " ++ show (input == "a\n")
  putStrLn $ "input == \"a\": " ++ show (input == "a")
  putStrLn $ "input == \" u\": " ++ show (input == " u")
  putStrLn $ "' ' `elem` input && '\t' `elem` input && not (all isSpace input) && input == \"\\t  \\t  \" ++ \" f\" ++ \"  \\t  \": " ++ show (' ' `elem` input && '\t' `elem` input && not (all isSpace input) && input == "\t  \t  " ++ " f" ++ "  \t  ")
  putStrLn $ "input == \"\\t\\SUB\": " ++ show (input == "\t\SUB")
  putStrLn $ "input == \"\\t  \\n\\t  8\\n\": " ++ show (input == "\t  \n\t  8\n")
  putStrLn $ "input == \"\\t  a\\n\": " ++ show (input == "\t  a\n")
  putStrLn $ "input == \"\\t\\t a\\t\": " ++ show (input == "\t\t a\t")
  putStrLn $ "input == \"\\t\\ta\\t\": " ++ show (input == "\t\ta\t")

main :: IO ()
main = testNormalizeIndentationConditions