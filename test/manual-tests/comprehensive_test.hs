import qualified Utils as U
import Test.QuickCheck
import Test.QuickCheck.Random (QCGen(..), mkQCGen)

-- | 测试isProblematicUnclosedString对转义引号的处理
prop_is_problematic_unclosed_escape_quote :: String -> Property
prop_is_problematic_unclosed_escape_quote s =
  let withEscape = "\"" ++ s ++ "\\\""
  in if s == ""
     then property $ U.isProblematicUnclosedString "\""  -- 特殊情况：只有引号
     else if s == "\\"
          then property $ U.isProblematicUnclosedString "\\"  -- 特殊情况：反斜杠
          else property $ U.isProblematicUnclosedString withEscape

main :: IO ()
main = do
  putStrLn "Testing with specific inputs..."
  
  -- Test with s = "b" (the failing case)
  let s = "b"
  let withEscape = "\"" ++ s ++ "\\\""
  putStrLn $ "s = " ++ show s
  putStrLn $ "withEscape = " ++ show withEscape
  putStrLn $ "U.isProblematicUnclosedString withEscape = " ++ show (U.isProblematicUnclosedString withEscape)
  putStrLn $ "U.isCompleteStringLiteral withEscape = " ++ show (U.isCompleteStringLiteral withEscape)
  
  -- Test the property directly
  putStrLn "\nTesting property with s = \"b\"..."
  putStrLn $ "Testing property with s = \"b\"..."
  
  -- Run QuickCheck with the specific replay seed from the failure
  putStrLn "\nRunning QuickCheck with replay seed..."
  quickCheckWith stdArgs {replay = Just (mkQCGen 1206732390842970513, 1)} prop_is_problematic_unclosed_escape_quote