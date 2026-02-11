-- 调试脚本
import Data.List (intercalate, lines)
import qualified Utils as U

main :: IO ()
main = do
  -- Test prop_string_lines
  let s1 = "a\n"
  let ls1 = lines s1
  let result1 = intercalate "\n" ls1
  putStrLn $ "Test prop_string_lines:"
  putStrLn $ "  Input: " ++ show s1
  putStrLn $ "  lines: " ++ show ls1
  putStrLn $ "  intercalate: " ++ show result1
  putStrLn $ "  Expected: " ++ show s1
  putStrLn ""
  
  -- Test prop_is_complete_string_literal_escape_backslash
  let s2 = "\""
  let withBackslash = "\"" ++ s2 ++ "\\\\"
  putStrLn $ "Test prop_is_complete_string_literal_escape_backslash:"
  putStrLn $ "  Input: " ++ show s2
  putStrLn $ "  withBackslash: " ++ show withBackslash
  putStrLn $ "  isCompleteStringLiteral: " ++ show (U.isCompleteStringLiteral withBackslash)
  putStrLn ""