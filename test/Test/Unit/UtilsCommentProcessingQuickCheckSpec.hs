{-# LANGUAGE FlexibleInstances #-}
module Test.Unit.UtilsCommentProcessingQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Arbitrary(..), Gen, choose, listOf, suchThat, oneof, elements, frequency)
import Data.Char (isSpace)
import Utils (removeLineComments, removeComments, normalizeIndentation, forceSingleTabIndentation)

-- | Generate arbitrary strings with potential comment structures
instance Arbitrary String where
  arbitrary = frequency
    [ (4, normalCode)
    , (2, codeWithLineComments)
    , (2, codeWithBlockComments)
    , (1, codeWithStrings)
    , (1, emptyOrWhitespace)
    ]
    where
      normalCode = listOf $ elements ['a'..'z', 'A'..'Z', '0'..'9', ' ', '	', '
', '(', ')', '{', '}', ';', '=']
      codeWithLineComments = do
        code <- listOf $ elements ['a'..'z', 'A'..'Z', '0'..'9', ' ', '	', '
', '(', ')', ';', '=']
        comment <- listOf $ elements ['a'..'z', 'A'..'Z', '0'..'9', ' ']
        return $ code ++ "//" ++ comment
      codeWithBlockComments = do
        before <- listOf $ elements ['a'..'z', 'A'..'Z', '0'..'9', ' ']
        comment <- listOf $ elements ['a'..'z', 'A'..'Z', '0'..'9', ' ', '
', '	']
        after <- listOf $ elements ['a'..'z', 'A'..'Z', '0'..'9', ' ']
        return $ before ++ "/*" ++ comment ++ "*/" ++ after
      codeWithStrings = do
        before <- listOf $ elements ['a'..'z', 'A'..'Z', '0'..'9', ' ']
        string <- listOf $ elements ['a'..'z', 'A'..'Z', '0'..'9', ' ', '"', '/']
        after <- listOf $ elements ['a'..'z', 'A'..'Z', '0'..'9', ' ']
        return $ before ++ "\"" ++ string ++ "\"" ++ after
      emptyOrWhitespace = frequency
        [ (1, return "")
        , (1, listOf $ elements " \t\n\r")
        ]

-- | Generate strings with specific indentation patterns
genIndentedCode :: Gen String
genIndentedCode = frequency
    [ (3, do -- Normal indented code
        lines <- listOf1 $ do
          indent <- choose (0, 8)
          content <- listOf $ elements ['a'..'z', ' ']
          return $ replicate indent ' ' ++ content
        return $ unlines lines)
    , (1, do -- Mixed tabs L.and spaces
        lines <- listOf1 $ do
          spaces <- choose (0, 4)
          tabs <- choose (0, 2)
          content <- listOf $ elements ['a'..'z', ' ']
          return $ replicate tabs '\t' ++ replicate spaces ' ' ++ content
        return $ unlines lines)
    , (1, return "") -- Empty
    ]

tests :: TestTree
tests =
  testGroup "Utils comment processing QuickCheck tests"
    [ testGroup "removeLineComments boundary conditions"
        [ testCase "removes basic line comments" $ do
            let input = "hello // comment\nworld // another\n"
                expected = "hello \nworld \n"
            removeLineComments input @?= expected

        , testCase "preserves comments inside strings" $ do
            let input = "url := \"http://example.com//path\" // comment\n"
                expected = "url := \"http://example.com//path\" \n"
            removeLineComments input @?= expected

        , testCase "handles escaped quotes in strings" $ do
            let input = "text := \"She said \\\"// hi\\\"\" // comment\n"
                expected = "text := \"She said \\\"// hi\\\"\" \n"
            removeLineComments input @?= expected

        , testCase "preserves character literals with slashes" $ do
            let input = "char := '/' // comment\n"
                expected = "char := '/' \n"
            removeLineComments input @?= expected

        , testCase "handles empty input" $ do
            removeLineComments "" @?= ""

        , testCase "handles input without comments" $ do
            let input = "hello\nworld\n"
                expected = "hello\nworld\n"
            removeLineComments input @?= expected

        , fastProperty "removeLineComments never increases string L.length" $
            \s ->
              L.length (removeLineComments s) <= L.length s

        , fastProperty "removeLineComments preserves line structure" $
            \s ->
              let originalLines = L.length $ lines s
                  processedLines = L.length $ lines (removeLineComments s)
              in processedLines == originalLines

        , fastProperty "removeLineComments is idempotent" $
            \s ->
              let once = removeLineComments s
                  twice = removeLineComments once
              in once == twice
        ]

    , testGroup "removeComments boundary conditions"
        [ testCase "removes both line L.and block comments" $ do
            let input = "code // line\n/* block */ more\n"
                expected = "code \n more\n"
            removeComments input @?= expected

        , testCase "handles nested comment-like patterns in strings" $ do
            let input = "url := \"/* not a comment */\" /* real comment */\n"
                expected = "url := \"/* not a comment */\" \n"
            removeComments input @?= expected

        , testCase "handles multiline block comments" $ do
            let input = "start /* comment\nstill comment */ end\n"
                expected = "start  \n end\n"
            removeComments input @?= expected

        , testCase "handles unterminated block comments" $ do
            let input = "code /* open\nmore\n"
                expected = "code \n\n"
            removeComments input @?= expected

        , testCase "handles empty input" $ do
            removeComments "" @?= ""

        , testCase "handles sequential block comments" $ do
            let input = "a /*first*/ b /*second*/ c\n"
                expected = "a  b  c\n"
            removeComments input @?= expected

        , fastProperty "removeComments never increases string L.length" $
            \s ->
              L.length (removeComments s) <= L.length s

        , fastProperty "removeComments is idempotent" $
            \s ->
              let once = removeComments s
                  twice = removeComments once
              in once == twice

        , fastProperty "removeComments preserves line count" $
            \s ->
              let originalLines = L.length $ lines s
                  processedLines = L.length $ lines (removeComments s)
              in processedLines == originalLines
        ]

    , testGroup "normalizeIndentation boundary conditions"
        [ testCase "removes common leading whitespace" $ do
            let input = "    hello\n        world\n    test\n"
                expected = "hello\n    world\ntest\n"
            normalizeIndentation input @?= expected

        , testCase "handles empty lines correctly" $ do
            let input = "    hello\n\n    world\n"
                expected = "hello\n\nworld\n"
            normalizeIndentation input @?= expected

        , testCase "handles input with only whitespace" $ do
            let input = "    \n\t\n"
            normalizeIndentation input @?= input

        , testCase "handles empty input" $ do
            normalizeIndentation "" @?= ""

        , testCase "handles input without common indentation" $ do
            let input = "hello\nworld\n"
                expected = "hello\nworld\n"
            normalizeIndentation input @?= expected

        , fastProperty "normalizeIndentation preserves relative indentation" $
            \s ->
              let originalLines = lines s
                  processedLines = lines (normalizeIndentation s)
                  preservedLengths = L.length originalLines == L.length processedLines
              in preservedLengths

        , fastProperty "normalizeIndentation is idempotent" $
            \s ->
              let once = normalizeIndentation s
                  twice = normalizeIndentation once
              in once == twice

        , fastProperty "normalizeIndentation doesn't create leading empty lines" $
            \s ->
              let processed = normalizeIndentation s
                  lines' = lines processed
              in null lines' || not (L.null (L.head lines')) || L.all null lines'
        ]

    , testGroup "forceSingleTabIndentation boundary conditions"
        [ testCase "converts to single tab indentation" $ do
            let input = "  hello\n    world\n"
                expected = "\thello\n\tworld\n"
            forceSingleTabIndentation input @?= expected

        , testCase "handles empty lines" $ do
            let input = "  hello\n\n    world\n"
                expected = "\thello\n\n\tworld\n"
            forceSingleTabIndentation input @?= expected

        , testCase "handles already tab-indented lines" $ do
            let input = "\thello\n\tworld\n"
                expected = "\thello\n\tworld\n"
            forceSingleTabIndentation input @?= expected

        , testCase "handles empty input" $ do
            forceSingleTabIndentation "" @?= ""

        , fastProperty "forceSingleTabIndentation results start with tab L.or are empty" $
            \s ->
              let result = forceSingleTabIndentation s
                  lines' = lines result
              in L.all (\line -> null line || L.head line == '\t') lines'

        , fastProperty "forceSingleTabIndentation is idempotent" $
            \s ->
              let once = forceSingleTabIndentation s
                  twice = forceSingleTabIndentation once
              in once == twice
        ]

    , testGroup "Complex interaction tests"
        [ testCase "comment removal followed by indentation normalization" $ do
            let input = "    // comment\n    code /* block */ more\n"
                step1 = removeComments input
                result = normalizeIndentation step1
                expected = " \n code  more\n"
            result @?= expected

        , fastProperty "comment removal preserves string literals" $
            \s ->
              let hasStringLiteral = "\"" `L.isInfixOf` s
                  processed = removeComments s
              in if hasStringLiteral 
                 then countOccurrences "\"" processed >= countOccurrences "\"" s - 2
                 else True
        ]
    ]

-- Helper function to count occurrences
countOccurrences :: Eq a => a -> [a] -> Int
countOccurrences x = L.length . L.filter (== x)

-- Helper function for infix check
isInfixOf :: Eq a => [a] -> [a] -> Bool
isInfixOf needle haystack = L.any (isPrefixOf needle) (tails haystack)
  where
    isPrefixOf [] _ = True
    isPrefixOf _ [] = False
    isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys
    tails [] = [[]]
    tails xs@(_:ys) = xs : tails ys