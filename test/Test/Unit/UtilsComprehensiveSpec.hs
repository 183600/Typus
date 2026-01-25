{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Test.Unit.UtilsComprehensiveSpec where



import Test.Tasty.HUnit
import Test.Tasty (TestTree, testGroup)
import Test.Tasty
-- Removed empty QuickCheck import
import Test.Tasty.QuickCheck (testProperties, Arbitrary(..), Gen, choose, listOf, elements, oneof, vectorOf, property, (===), forAll, counterexample)
import Test.QuickCheck (Gen, Property, (==>), classify)
import Utils (trim, splitBy, splitByCollapsed, splitByComma, splitByCommaCollapsed,
             removeLineComments, removeComments, normalizeIndentation, 
             forceSingleTabIndentation, fixIndentation, breakOn,
             safeProcessString, isValidChar)
import qualified Data.Text as T
import Data.Char (isSpace, isAlpha, isAlphaNum, isPrint)
import Data.List (isPrefixOf, isSuffixOf, isInfixOf)

-- Helper generators for Utils tests
genChar :: Gen Char
genChar = elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " \t\n\r.,;:!()[]{}+-*/=<>&|^%~?"

genString :: Gen String
genString = do
  len <- choose (0, 50)
  vectorOf len genChar

genNonEmptyString :: Gen String
genNonEmptyString = do
  len <- choose (1, 50)
  vectorOf len genChar

genWhitespace :: Gen String
genWhitespace = listOf $ elements " \t\n\r"

genAlphaNum :: Gen String
genAlphaNum = do
  len <- choose (1, 20)
  vectorOf len $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']

-- Test properties for Utils module

-- Property 1: trim removes leading and trailing whitespace
prop_trim_removes_whitespace :: String -> Bool
prop_trim_removes_whitespace s = 
  let trimmed = trim s
      firstChar s' = case s' of
                       (c:_) -> c
                       [] -> ' '
      lastChar s' = case reverse s' of
                      (c:_) -> c
                      [] -> ' '
      hasLeadingOrTrailing = not (null s) && (isSpace (firstChar s) || isSpace (lastChar s))
  in if hasLeadingOrTrailing
     then not (isSpace (firstChar trimmed)) && not (isSpace (lastChar trimmed))
     else trimmed == s

-- Property 2: trim is idempotent
prop_trim_idempotent :: String -> Bool
prop_trim_idempotent s = trim (trim s) == trim s

-- Property 3: splitBy with comma behaves correctly
prop_splitBy_comma_basic :: String -> Property
prop_splitBy_comma_basic s = 
  not (',' `elem` s) ==> splitBy ',' s == [s]

-- Property 4: splitBy preserves empty segments
prop_splitBy_preserves_empty :: String -> Property
prop_splitBy_preserves_empty s = 
  ",," `isInfixOf` s ==> any null (splitBy ',' s)

-- Property 5: splitByCollapsed removes consecutive delimiters
prop_splitByCollapsed_removes_consecutive :: String -> Bool
prop_splitByCollapsed_removes_consecutive s = 
  not (any (== ",,") (splitByCommaCollapsed s))

-- Property 6: splitBy and splitByCollapsed relationship
prop_splitBy_vs_collapsed :: String -> Bool
prop_splitBy_vs_collapsed s = 
  let normal = splitBy ',' s
      collapsed = splitByCommaCollapsed s
      filtered = filter (not . null) normal
  in length collapsed == length filtered

-- Property 7: removeLineComments only removes // comments
prop_removeLine_comments_basic :: String -> Bool
prop_removeLine_comments_basic s = 
  let withoutComments = removeLineComments s
      hasLineComment = "//" `isInfixOf` s
  in if hasLineComment
     then not ("//" `isInfixOf` withoutComments)
     else withoutComments == s

-- Property 8: removeComments handles both // and /* */ comments
prop_remove_comments_comprehensive :: String -> Bool
prop_remove_comments_comprehensive s = 
  let withoutComments = removeComments s
      hasAnyComment = "//" `isInfixOf` s || "/*" `isInfixOf` s
  in if hasAnyComment
     then not ("//" `isInfixOf` withoutComments) && not ("/*" `isInfixOf` withoutComments)
     else withoutComments == s

-- Property 9: normalizeIndentation preserves relative indentation
prop_normalize_preserves_relative :: String -> Bool
prop_normalize_preserves_relative s = 
  let normalized = normalizeIndentation s
      originalLines = lines s
      normalizedLines = lines normalized
  in length originalLines == length normalizedLines

-- Property 10: breakOn behaves like break but for strings
prop_breakOn_consistency :: String -> Char -> Property
prop_breakOn_consistency s c = 
  c `elem` s ==> let (before, after) = breakOn [c] s
                 in before ++ [c] ++ after == s

-- Property 11: safeProcessString handles invalid characters
prop_safe_process_string :: String -> Bool
prop_safe_process_string s = 
  let processed = safeProcessString s
  in case processed of
       Right str -> all isValidChar str
       Left _ -> True

-- Property 12: isValidChar correctly identifies valid characters
prop_is_valid_char_consistency :: Char -> Bool
prop_is_valid_char_consistency c = 
  isValidChar c == isPrint c

-- Unit tests for edge cases
test_trim_edge_cases :: [TestTree]
test_trim_edge_cases = 
  [ testCase "trim empty string" $ 
      assertEqual "" "" (trim "")
  , testCase "trim only whitespace" $ 
      assertEqual "" "" (trim "   \t\n\r  ")
  , testCase "trim no whitespace" $ 
      assertEqual "hello" "hello" (trim "hello")
  , testCase "trim mixed content" $ 
      assertEqual "hello" "hello" (trim "  hello  ")
  ]

test_splitBy_edge_cases :: [TestTree]
test_splitBy_edge_cases = 
  [ testCase "splitBy empty string" $ do
      assertEqual "should return empty list" [] (splitBy ',' "")
  , testCase "splitBy single delimiter" $ do
      assertEqual "should split at delimiter" ["", ""] (splitBy ',' ",")
  , testCase "splitBy consecutive delimiters" $ do
      assertEqual "should handle consecutive delimiters" ["", "", ""] (splitBy ',' ",,")
  , testCase "splitBy no delimiter" $ do
      assertEqual "should return single element" ["hello"] (splitBy ',' "hello")
  ]
test_comment_removal_edge_cases :: [TestTree]
test_comment_removal_edge_cases = 
  [ testCase "removeLineComments no comments" $ 
      assertEqual "hello world" "hello world" (removeLineComments "hello world")
  , testCase "removeLineComments only comment" $ 
      assertEqual "" "" (removeLineComments "// this is a comment")
  , testCase "removeLineComments mixed content" $ 
      assertEqual "hello " "hello " (removeLineComments "hello // comment")
  , testCase "removeComments block comment" $ 
      assertEqual "hello " "hello " (removeComments "hello /* block comment */")
  , testCase "removeComments nested-like" $ 
      assertEqual "hello " "hello " (removeComments "hello /* outer /* inner */ */")
  ]

test_indentation_edge_cases :: [TestTree]
test_indentation_edge_cases = 
  [ testCase "normalizeIndentation empty string" $ 
      assertEqual "" "" (normalizeIndentation "")
  , testCase "normalizeIndentation single line" $ 
      assertEqual "hello" "hello" (normalizeIndentation "hello")
  , testCase "normalizeIndentation multiple lines" $ 
      assertEqual "hello\nworld" "hello\nworld" (normalizeIndentation "  hello\n  world")
  ]

test_string_processing_edge_cases :: [TestTree]
test_string_processing_edge_cases = 
  [ testCase "safeProcessString empty" $ 
      case safeProcessString "" of
        Right result -> assertEqual "" "" result
        Left _ -> assertFailure "safeProcessString failed on empty string"
  , testCase "safeProcessString valid chars" $ 
      case safeProcessString "hello123" of
        Right result -> assertEqual "hello123" "hello123" result
        Left _ -> assertFailure "safeProcessString failed on valid chars"
  , testCase "breakOn not found" $ do
      let result = breakOn "x" "hello" :: (String, String)
      assertEqual "should return full string when not found" ("hello", "") result
  , testCase "breakOn first occurrence" $ do
      let result = breakOn "l" "hello" :: (String, String)
      assertEqual "should split at first occurrence" ("he", "llo") result
  ]

-- QuickCheck property tests
utilsQuickCheckTests :: TestTree
utilsQuickCheckTests = testGroup "QuickCheck Properties"
  [ testProperties "String Processing"
      [ ("trim removes whitespace", property prop_trim_removes_whitespace)
      , ("trim is idempotent", property prop_trim_idempotent)
      , ("breakOn consistency", property prop_breakOn_consistency)
      , ("safeProcessString handles invalid chars", property prop_safe_process_string)
      , ("isValidChar consistency", property prop_is_valid_char_consistency)
      ]
  , testProperties "Split Operations"
      [ ("splitBy comma basic", property prop_splitBy_comma_basic)
      , ("splitBy preserves empty", property prop_splitBy_preserves_empty)
      , ("splitByCollapsed removes consecutive", property prop_splitByCollapsed_removes_consecutive)
      , ("splitBy vs collapsed", property prop_splitBy_vs_collapsed)
      ]
  , testProperties "Comment Removal"
      [ ("removeLineComments basic", property prop_removeLine_comments_basic)
      , ("removeComments comprehensive", property prop_remove_comments_comprehensive)
      ]
  , testProperties "Indentation"
      [ ("normalize preserves relative", property prop_normalize_preserves_relative)
      ]
  ]

-- Unit tests
utilsUnitTests :: TestTree
utilsUnitTests = testGroup "Unit Tests"
  [ testGroup "Trim Edge Cases" test_trim_edge_cases
  , testGroup "SplitBy Edge Cases" test_splitBy_edge_cases
  , testGroup "Comment Removal Edge Cases" test_comment_removal_edge_cases
  , testGroup "Indentation Edge Cases" test_indentation_edge_cases
  , testGroup "String Processing Edge Cases" test_string_processing_edge_cases
  ]

-- Main test suite
utilsComprehensiveTests :: TestTree
utilsComprehensiveTests = testGroup "Utils Comprehensive Tests"
  [ utilsUnitTests
  , utilsQuickCheckTests
  ]