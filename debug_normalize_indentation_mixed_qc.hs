import Utils
import Test.QuickCheck
import Data.List (isInfixOf)

-- 复制测试逻辑
prop_normalize_indentation_mixed :: String -> Property
prop_normalize_indentation_mixed s =
  let mixed = "  \t  " ++ s
      normalized = normalizeIndentation mixed
  in property $ not ("\t" `isInfixOf` normalized)

main :: IO ()
main = do
    -- 测试特定情况
    let s = "a"
    let mixed = "  \t  " ++ s
    let normalized = normalizeIndentation mixed
    putStrLn $ "s: " ++ show s
    putStrLn $ "mixed: " ++ show mixed
    putStrLn $ "normalized: " ++ show normalized
    putStrLn $ "Has tab: " ++ show ('\t' `elem` normalized)
    putStrLn $ "Test passes: " ++ show (not ('\t' `elem` normalized))
    
    -- 运行 QuickCheck 测试
    putStrLn "\nRunning QuickCheck test..."
    quickCheck prop_normalize_indentation_mixed