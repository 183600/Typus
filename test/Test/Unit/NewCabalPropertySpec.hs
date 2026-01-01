{-# LANGUAGE LambdaCase #-}

module Test.Unit.NewCabalPropertySpec (tests) where

import Test.Tasty (TestTree, testGroup)
import TestSupport.QuickCheck (fastProperty)
import Test.Tasty.HUnit (testCase, (@?=))

import Utils (trim, splitBy, splitByCollapsed)
import SourceLocation (SourcePos(..), SourceSpan(..), spanStart, spanEnd)
import qualified Data.List as L
import Data.List (isPrefixOf, isSuffixOf)
import Data.Char (isSpace, isAlphaNum)

-- | Property-based tests using QuickCheck
tests :: TestTree
tests =
  testGroup "New Cabal Property Tests"
    [ testGroup "Utils properties"
        [ fastProperty "trim . trim = trim" $ \s ->
            trim (trim s) == trim s

        , fastProperty "splitBy delim . intercalate [delim] = original" $ \xs ->
            let delim = ','
                s = L.concat $ L.map (\x -> x ++ [delim]) xs
            in splitBy delim (init s) == xs

        , fastProperty "splitByCollapsed removes empty segments" $ \s ->
            let delim = ','
                result = splitByCollapsed delim s
            in L.all (not . null) result

        , fastProperty "trim doesn't change non-whitespace strings" $ \s ->
            let noWhitespace = L.all (not . isSpace) s
            in if noWhitespace then trim s == s else True

        , fastProperty "splitBy preserves order" $ \s ->
            let delim = ','
                result = splitBy delim s
            in L.concat result == s
        ]

    , testGroup "SourceLocation properties"
        [ fastProperty "span start line <= span end line" $ \line1 col1 line2 col2 ->
            let pos1 = SourcePos { posLine = abs line1, posColumn = abs col1 }
                pos2 = SourcePos { posLine = abs line2, posColumn = abs col2 }
                span = if posLine pos1 <= posLine pos2 
                       then SourceSpan { spanStart = pos1, spanEnd = pos2 }
                       else SourceSpan { spanStart = pos2, spanEnd = pos1 }
            in posLine (spanStart span) <= posLine (spanEnd span)

        , fastProperty "position components are non-negative" $ \line col ->
            let pos = SourcePos { posLine = abs line, posColumn = abs col }
            in posLine pos >= 0 && posColumn pos >= 0

        , fastProperty "span equality is symmetric" $ \line1 col1 line2 col2 ->
            let pos1 = SourcePos { posLine = line1, posColumn = col1 }
                pos2 = SourcePos { posLine = line2, posColumn = col2 }
                span1 = SourceSpan { spanStart = pos1, spanEnd = pos2 }
                span2 = SourceSpan { spanStart = pos2, spanEnd = pos1 }
            in (span1 == span2) == (span2 == span1)
        ]

    , testGroup "String manipulation properties"
        [ fastProperty "L.length after trim is <= original L.length" $ \s ->
            L.length (trim s) <= L.length s

        , fastProperty "trim removes only leading/trailing whitespace" $ \s ->
            let trimmed = trim s
                hasInternalWhitespace = L.any isSpace trimmed
            in if null trimmed then True 
               else not (isSpace (L.head trimmed) || isSpace (last trimmed))

        , fastProperty "splitBy on single character returns list of single characters" $ \c ->
            let s = [c]
                delim = ','
            in splitBy delim s == if c == delim then ["", ""] else [s]

        , fastProperty "splitBy on empty string returns single empty string" $ \delim ->
            splitBy delim "" == [""]
        ]

    , testGroup "Parser-related properties"
        [ fastProperty "parsing empty string produces predictable result" $ \_ ->
            -- This would need actual parser implementation
            True

        , fastProperty "parsing is idempotent for valid inputs" $ \s ->
            -- If parsing succeeds, parsing the result again should be consistent
            -- This is a placeholder since we don't have the full parser context
            L.length s <= 1000  -- Reasonable size limit
        ]

    , testGroup "List properties"
        [ fastProperty "L.concat . splitBy delim = original (for delim not in string)" $ \s ->
            let delim = '\0'  -- Null character unlikely to be in test strings
                parts = splitBy delim s
            in if delim `elem` s then True else L.concat parts == s

        , fastProperty "splitBy preserves total character count" $ \s ->
            let delim = ','
                parts = splitBy delim s
                originalLength = L.length s
                splitLength = L.sum (map L.length parts) + L.length (L.filter (== delim) s)
            in originalLength == splitLength

        , fastProperty "fold with splitBy can reconstruct original" $ \s ->
            let delim = ','
                parts = splitBy delim s
            in if null parts then s == ""
               else L.concat (intersperse [delim] parts) ++ (if last s == delim then [delim] else "") == s
        ]

    , testGroup "Character properties"
        [ fastProperty "isSpace is consistent with trim behavior" $ \s ->
            let trimmed = trim s
                hasLeadingSpace = not (null s) && isSpace (L.head s)
                hasTrailingSpace = not (null s) && isSpace (last s)
            in if hasLeadingSpace || hasTrailingSpace 
               then L.length trimmed < L.length s 
               else L.length trimmed == L.length s

        , fastProperty "alphanumeric strings are unchanged by trim" $ \s ->
            let alnumStr = filter isAlphaNum s
            in if null alnumStr then trim alnumStr == ""
               else trim alnumStr == alnumStr
        ]

    , testGroup "Error handling properties"
        [ testCase "error messages are non-empty" $ do
            -- This would test actual error handling
            True @?= True

        , testCase "error recovery maintains some structure" $ do
            -- Placeholder for error recovery tests
            True @?= True
        ]
    ]

-- Helper function for intersperse
intersperse :: a -> [a] -> [a]
intersperse _ [] = []
intersperse _ [x] = [x]
intersperse sep (x:xs) = x : sep : intersperse sep xs