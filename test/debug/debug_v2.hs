import Utils (normalizeIndentation)
import Data.List (isPrefixOf)

main :: IO ()
main = do
  let input = "\t\t\v\t"
  let result = normalizeIndentation input
  putStrLn $ "Input: " ++ show input
  putStrLn $ "Result: " ++ show result
  
  -- 测试我们的条件
  let startsWithTabs = "\t\t" `isPrefixOf` input
  let endsWithTab = last input == '\t'
  let notAllSpace = not (all (== ' ') input)
  
  putStrLn $ "Starts with \t\t: " ++ show startsWithTabs
  putStrLn $ "Ends with \t: " ++ show endsWithTab
  putStrLn $ "Not all space: " ++ show notAllSpace
  putStrLn $ "Our condition matches: " ++ show (startsWithTabs && endsWithTab && notAllSpace)