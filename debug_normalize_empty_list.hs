import Utils (normalizeIndentation)
import Test.QuickCheck
import Data.List (isInfixOf)

prop_normalize_indentation_empty_lines :: String -> Property
prop_normalize_indentation_empty_lines s =
  let withEmpty = s ++ "\n\n"
      normalized = normalizeIndentation withEmpty
  in if null s
     then property $ normalized == "    "  -- 空字符串加两个换行符转换为4个空格
     else property $ "\n\n" `isInfixOf` normalized  -- 非空字符串加两个换行符应该保留换行符

main :: IO ()
main = do
  quickCheck prop_normalize_indentation_empty_lines