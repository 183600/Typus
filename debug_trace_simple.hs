import Data.Char (isSpace, isPrint)
import Data.List (isPrefixOf)

-- 简化版调试函数
testInput :: String -> IO ()
testInput input = do
  putStrLn $ "Testing: " ++ show input
  
  let result = 
        if null input
          then "null"
        else if input == "\t  \t  \n  \t  "
          then "case1"
        else if input == "\t  \t    \t  "
          then "case2"
        else if input == "\t  \n\t  \n\n"
          then "case3"
        else if input == "\t\ta\t"
          then "case_tab_tab_a_tab"
        else if input == "\t  \n"
          then "case_tab_space_newline"
        else if "\t\t" `isPrefixOf` input && not (all isSpace input)
          then "starts_with_tab_tab"
        else "other"
  
  putStrLn $ "Result: " ++ result

main :: IO ()
main = do
  testInput "\t\ta\t"
  testInput "\t  \n"