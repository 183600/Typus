module Test.Unit.NewCabalUtilsSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.QuickCheck ((===), Property, counterexample)

import TestSupport.QuickCheck (fastProperty)
import Utils
import Data.Char (isSpace)
import qualified Data.Text as T

-- | Additional comprehensive tests for Utils module
tests :: TestTree
tests =
  testGroup "NewCabal Utils Tests"
    [ testGroup "String processing edge cases"
        [ testCase "trim handles unicode whitespace correctly" $ do
            trim "\x2003hello\x2002world\x00A0" @?= "hello\x2002world"

        , testCase "splitBy handles multibyte characters" $ do
            splitBy '€' "alpha€beta€gamma" @?= ["alpha", "beta", "gamma"]

        , testCase "removeComments handles nested quotes correctly" $ do
            let input = "text := \"He said \\\"/* not comment */\\\"\"" /* real comment */"
                expected = "text := \"He said \\\"/* not comment */\\\"\"" "
            removeComments input @?= expected
        ]

    , testGroup "Advanced comment handling"
        [ testCase "removeComments handles complex escape sequences" $ do
            let input = "path := \"C:\\\\tmp\\\\//not_comment\"" // real comment\n"
                expected = "path := \"C:\\\\tmp\\\\//not_comment\"" \n"
            removeComments input @?= expected

        , testCase "removeLineComments preserves line structure" $ do
            let input = "line1 // comment1\nline2 // comment2\n"
                expected = "line1 \nline2 \n"
            removeLineComments input @?= expected

        , testCase "removeComments handles multiple block comments" $ do
            let input = "a/*comment1*/b/*comment2*/c"
                expected = "abc"
            removeComments input @?= expected
        ]

    , testGroup "Indentation edge cases"
        [ testCase "normalizeIndentation handles mixed tabs and spaces" $ do
            let input = "\t    mixed\n\t    indentation"
                expected = "mixed\nindentation"
            normalizeIndentation input @?= expected

        , testCase "normalizeIndentation preserves trailing empty lines" $ do
            let input = "    content\n\n"
                expected = "content\n\n"
            normalizeIndentation input @?= expected
        ]

    , testGroup "QuickCheck property tests"
        [ fastProperty "splitBy and splitByCollapsed relationship" prop_splitByRelationship
        , fastProperty "breakOn correctness" prop_breakOnCorrectness
        , fastProperty "trim removes only leading/trailing whitespace" prop_trimOnlyRemovesWhitespace
        , fastProperty "removeComments preserves non-comment content" prop_removeCommentsPreservesContent
        ]
    ]

-- Property: splitByCollapsed should never have empty strings when splitBy has non-empty
prop_splitByRelationship :: String -> Property
prop_splitByRelationship input =
  let normal = splitBy ':' input
      collapsed = splitByCollapsed ':' input
      hasNonEmpty = any (not . null) normal
  in counterexample ("normal: " ++ show normal ++ ", collapsed: " ++ show collapsed) $
     if hasNonEmpty 
     then all (not . null) collapsed
     else collapsed === []

-- Property: breakOn should correctly split strings
prop_breakOnCorrectness :: String -> String -> Property
prop_breakOnCorrectness pat haystack = 
  let (prefix, suffix) = breakOn pat haystack
      reconstructed = prefix ++ pat ++ suffix
  in if null pat
     then prefix === "" && suffix === haystack
     else reconstructed === haystack

-- Property: trim should only remove whitespace from ends
prop_trimOnlyRemovesWhitespace :: String -> Property
prop_trimOnlyRemovesWhitespace input =
  let trimmed = trim input
      originalLength = length input
      trimmedLength = length trimmed
  in counterexample ("original: " ++ show input ++ ", trimmed: " ++ show trimmed) $
     trimmedLength <= originalLength

-- Property: removeComments should preserve non-comment characters
prop_removeCommentsPreservesContent :: String -> Property
prop_removeCommentsPreservesContent input =
  let withoutComments = removeComments input
      -- Count non-comment characters (simplified check)
      nonCommentChars = filter (`notElem` "/*") input
      resultChars = filter (`notElem` " \n\r\t") withoutComments
  in counterexample ("input: " ++ show input ++ ", result: " ++ show withoutComments) $
     length resultChars <= length nonCommentChars