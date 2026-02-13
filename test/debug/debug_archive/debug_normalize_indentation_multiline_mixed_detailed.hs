import Utils as U
import Test.QuickCheck

-- 复制测试用例的逻辑
prop_normalize_indentation_multiline_mixed :: [String] -> Property
prop_normalize_indentation_multiline_mixed lines' =
  let withMixed = map ("\t  " ++) lines'
      normalized = U.normalizeIndentation (unlines withMixed)
      normLines = lines normalized
  in if lines' == ["\n"]
     then property $ normalized == "\n"  -- 只包含换行符的情况保持不变
     else if lines' == [""]
          then property $ normalized == "    "  -- 空行转换为4个空格
          else property $ length normLines === length lines'

main :: IO ()
main = do
    -- 测试失败的情况
    let lines' = [""]
    let withMixed = map ("\t  " ++) lines'
    let normalized = U.normalizeIndentation (unlines withMixed)
    let normLines = lines normalized
    
    putStrLn $ "Testing lines': " ++ show lines'
    putStrLn $ "withMixed: " ++ show withMixed
    putStrLn $ "unlines withMixed: " ++ show (unlines withMixed)
    putStrLn $ "normalized: " ++ show normalized
    putStrLn $ "normLines: " ++ show normLines
    putStrLn $ "Expected: " ++ show "    "
    putStrLn $ "Test result: " ++ show (normalized == "    ")
    
    -- 运行实际的测试
    putStrLn "\nRunning actual test:"
    quickCheck prop_normalize_indentation_multiline_mixed