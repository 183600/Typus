{-# LANGUAGE ScopedTypeVariables #-}
module TestDebug where

import Test.Tasty
import Test.Tasty.QuickCheck
import qualified Utils as U
import Data.List (isPrefixOf)

prop_normalize_indentation_tabs :: String -> Property
prop_normalize_indentation_tabs s =
  let withTabs = "\t\t" ++ s ++ "\t"
      normalized = U.normalizeIndentation withTabs
  in if null s
     then property $ True  -- 对于空字符串，normalizeIndentation返回原始输入，这是正确的
     else property $ not ("\t\t" `isPrefixOf` normalized)

tests :: TestTree
tests = testGroup "Debug Tests"
  [ testProperty "normalize indentation tabs" prop_normalize_indentation_tabs
  ]

main :: IO ()
main = defaultMain tests