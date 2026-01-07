module Test.Unit.NewUtilsValidationSpec where


import Test.Tasty 
import Test.Tasty.HUnit (testCase, assertFailure, assertBool, (@?=)), assertBool
import Test.Tasty.QuickCheck (testProperty, Property, (==>), Positive)
prop_trim_idempotent                               s = trim (trim s) == trim s

prop_trim_no_leading_trailing_whitespace :: String -> Bool
prop_trim_no_leading_trailing_whitespace                               s = 
  let t = trim s
in null t || (not (isSpace (L.head t) && not (isSpace (last t))

prop_splitBy_length :: String -> Char -> Bool
prop_splitBy_length s                               c = 
  let parts = splitBy c s
                              commas = L.length (L.filter (== c) s)
  in L.length                               parts == commas + 1

prop_splitByCollapsed_no_empty :: String -> Char -> Bool  
prop_splitByCollapsed_no_empty s                               c = 
  L.all (not . null) (splitByCollapsed c s)

prop_breakOn_consistency :: String -> String -> Bool
prop_breakOn_consistency s                               needle = 
case breakOn needle s of
    (before, after) -> before ++ needle ++                               after == s

-- QuickCheck test suite
utilsQuickCheckProperties :: TestTree
utilsQuickCheckProperties = testGroup "Utils QuickCheck Properties"
  [             testProperty "trim is idempotent" prop_trim_idempotent
  ,             testProperty "trim removes leading/trailing whitespace" prop_trim_no_leading_trailing_whitespace
  ,             testProperty "splitBy creates correct number of parts" prop_splitBy_length
  ,             testProperty "splitByCollapsed removes empty segments" prop_splitByCollapsed_no_empty
  ,             testProperty "breakOn is consistent" prop_breakOn_consistency
  ]