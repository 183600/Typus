import Utils
import Data.List (isPrefixOf)
import Data.Char (isSpace)

main :: IO ()
main = do
  -- 测试单行以制表符开头的情况
  let input = "\t\ta\t"
      output = normalizeIndentation input
      startsWithTabs = "\t\t" `isPrefixOf` output
  
  putStrLn $ "Input: " ++ show input
  putStrLn $ "Output: " ++ show output
  putStrLn $ "Starts with tabs: " ++ show startsWithTabs
  putStrLn $ "Input length: " ++ show (length input)
  putStrLn $ "Output length: " ++ show (length output)
  
  -- 分析输入行
  let inputLines = lines input
  putStrLn $ "Input lines: " ++ show inputLines
  putStrLn $ "Number of lines: " ++ show (length inputLines)
  
  case inputLines of
    [line] -> do
      putStrLn $ "Single line: " ++ show line
      putStrLn $ "Line is all spaces: " ++ show (all isSpace line)
      putStrLn $ "Line starts with space: " ++ show (not (null line) && isSpace (head line))
      let dropped = dropWhile isSpace line
      putStrLn $ "After dropWhile isSpace: " ++ show dropped
    _ -> putStrLn "Not a single line"