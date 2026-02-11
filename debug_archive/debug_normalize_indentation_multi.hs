import Utils
import Data.List

main :: IO ()
main = do
  -- 测试多行输入，这是prop_normalizeIndentation_deep测试的情况
  let s = "a"
      depth = 1
      deepIndent = unlines $ map (replicate depth ' ' ++) (lines s)
      normalized = Utils.normalizeIndentation deepIndent
      normLines = lines normalized
  
  putStrLn $ "Input: " ++ show deepIndent
  putStrLn $ "Normalized: " ++ show normalized
  putStrLn $ "Has leading spaces: " ++ show (any (isPrefixOf (replicate depth ' ')) normLines)