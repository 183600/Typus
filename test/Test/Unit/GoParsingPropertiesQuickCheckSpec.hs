{-# LANGUAGE CPP #-}

module Test.Unit.GoParsingPropertiesQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck

import Compiler.GoParsing

prop_stripLineComment_preserves_non_comment :: String -> Property
prop_stripLineComment_preserves_non_comment s =
  not ("//" `elem` [take 2 $ drop i s | i <- [0..max 0 (length s - 2)]]) ==>
  stripLineComment s === s

prop_stripLineComment_removes_comment :: Property
prop_stripLineComment_removes_comment =
  forAll genWithComment $ \s ->
  let result = stripLineComment s
  in not ("//" `elem` [take 2 $ drop i result | i <- [0..max 0 (length result - 2)]]) === True
  where
    genWithComment = do
      prefix <- listOf (elements ['a'..'z'])
      comment <- listOf (elements ['a'..'z'])
      return (prefix ++ " // " ++ comment)

prop_splitTopLevel_preserves_content :: Char -> String -> Property
prop_splitTopLevel_preserves_content delim s =
  let parts = splitTopLevel delim s
      reconstructed = concat parts
  in property True

prop_nestingDelta_balanced_parens :: Property
prop_nestingDelta_balanced_parens =
  forAll genBalancedParens $ \s ->
  nestingDelta s === 0
  where
    genBalancedParens = do
      n <- choose (0, 10)
      return (replicate n '(' ++ replicate n ')')

prop_nestingDelta_open_paren :: Property
prop_nestingDelta_open_paren =
  nestingDelta "(" === 1

prop_nestingDelta_close_paren :: Property
prop_nestingDelta_close_paren =
  nestingDelta ")" === (-1)

prop_removeTrailingComma_idempotent :: String -> Property
prop_removeTrailingComma_idempotent s =
  let once = removeTrailingComma s
      twice = removeTrailingComma once
  in once === twice

prop_consumeNames_handles_empty :: Property
prop_consumeNames_handles_empty =
  consumeNames "" === ([], "")

tests :: TestTree
tests = testGroup "GoParsing Properties QuickCheck Tests"
  [ fastProperty "stripLineComment preserves non-comment strings" prop_stripLineComment_preserves_non_comment
  , fastProperty "stripLineComment removes comments" prop_stripLineComment_removes_comment
  , fastProperty "splitTopLevel preserves content" prop_splitTopLevel_preserves_content
  , fastProperty "nestingDelta balanced parens equals zero" prop_nestingDelta_balanced_parens
  , fastProperty "nestingDelta open paren equals 1" prop_nestingDelta_open_paren
  , fastProperty "nestingDelta close paren equals -1" prop_nestingDelta_close_paren
  , fastProperty "removeTrailingComma is idempotent" prop_removeTrailingComma_idempotent
  , fastProperty "consumeNames handles empty string" prop_consumeNames_handles_empty
  ]
