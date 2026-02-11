import Utils
import Test.QuickCheck

prop_is_complete_string_literal_escape_backslash :: String -> Property
prop_is_complete_string_literal_escape_backslash s =
  let withBackslash = "\"" ++ s ++ "\\\\"
  in property $ Utils.isCompleteStringLiteral withBackslash

main :: IO ()
main = do
    putStrLn "=== Testing prop_is_complete_string_literal_escape_backslash ==="
    
    -- 测试具体失败案例
    let s = "\""
    let withBackslash = "\"" ++ s ++ "\\\\"
    putStrLn $ "Input s: " ++ show s
    putStrLn $ "Constructed string: " ++ show withBackslash
    putStrLn $ "isCompleteStringLiteral result: " ++ show (Utils.isCompleteStringLiteral withBackslash)
    putStrLn $ "Expected: True"
    putStrLn $ "Test passes: " ++ show (Utils.isCompleteStringLiteral withBackslash == True)
    
    -- 运行QuickCheck测试
    putStrLn "\n=== Running QuickCheck ==="
    quickCheck prop_is_complete_string_literal_escape_backslash