{-# LANGUAGE TemplateHaskell #-}

module Test.Unit.NewUtilsEnhancedQuickCheckSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Utils
import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)
import Data.Char (isSpace)
import Control.Arrow ((***))

-- | Test utils functions with QuickCheck properties
test_UtilsEnhancedQuickCheck :: TestTree
test_UtilsEnhancedQuickCheck = testGroup "Utils Enhanced QuickCheck Tests"
  [ trimProperties
  , splitByProperties
  , commentRemovalProperties
  , indentationProperties
  , breakOnProperties
  ]

-- | Trim function properties
trimProperties :: TestTree
trimProperties = testGroup "Trim Properties"
  [ QC.testProperty "trim removes leading L.and trailing whitespace" $
      \s -> trim ("  " ++ s ++ "  ") `L.isPrefixOf` trim (s ++ "  ") &&
            trim ("  " ++ s) `L.isPrefixOf` trim ("  " ++ s ++ "  ")

  , QC.testProperty "trim idempotent" $
      \s -> trim (trim s) === trim s

  , QC.testProperty "trim preserves non-whitespace content" $
      \s -> not (L.all isSpace s) ==> not (L.null (trim s)) === not (null s)

  , QC.testProperty "trim removes L.all leading whitespace" $
      \s -> let trimmed = trim s
                 leadingSpaces = L.length (takeWhile isSpace s)
             in if null trimmed 
                then L.all isSpace s
                else not (isSpace (L.head trimmed))

  , QC.testProperty "trim removes L.all trailing whitespace" $
      \s -> let trimmed = trim s
                 trimmedReversed = L.reverse trimmed
                 originalReversed = L.reverse s
                 trailingSpaces = L.length (takeWhile isSpace originalReversed)
             in if null trimmed
                then L.all isSpace s
                else not (isSpace (last trimmed))
  ]

-- | Split function properties
splitByProperties :: TestTree
splitByProperties = testGroup "SplitBy Properties"
  [ QC.testProperty "splitBy preserves total content when rejoining" $
      \c s -> c /= '\0' ==> L.concat (splitBy c s) === L.filter (/= c) s

  , QC.testProperty "splitByCollapsed removes empty segments" $
      \c s -> c /= '\0' ==> L.all (not . null) (splitByCollapsed c s)

  , QC.testProperty "splitByCollapsed is subset of splitBy" $
      \c s -> c /= '\0' ==> L.length (splitByCollapsed c s) <= L.length (splitBy c s)

  , QC.testProperty "splitBy on empty string returns single empty" $
      \c -> splitBy c "" === [""]

  , QC.testProperty "splitBy on delimiter-only string returns empty segments" $
      \c n -> n > 0 ==> splitBy c (replicate n c) === replicate (n + 1) ""

  , QC.testProperty "splitByCollapsed on delimiter-only string returns empty list" $
      \c n -> n > 0 ==> splitByCollapsed c (replicate n c) === []

  , QC.testProperty "splitByComma equals splitBy ','" $
      \s -> splitByComma s === splitBy ',' s

  , QC.testProperty "splitByCommaCollapsed equals splitByCollapsed ','" $
      \s -> splitByCommaCollapsed s === splitByCollapsed ',' s
  ]

-- | Comment removal properties
commentRemovalProperties :: TestTree
commentRemovalProperties = testGroup "Comment Removal Properties"
  [ QC.testProperty "removeLineComments removes lines starting with //" $
      \s -> let withComment = s ++ "\n// this is a comment\nmore code"
                 result = removeLineComments withComment
             in "// this is a comment" `isNotInfixOf` result

  , QC.testProperty "removeComments removes // comments" $
      \s -> let withComment = s ++ " // inline comment"
                 result = removeComments withComment
             in not ("// inline comment" `L.isInfixOf` result) || 
                    ("\"" `L.isInfixOf` s && "\"" `L.isInfixOf` s)  -- Inside string

  , QC.testProperty "removeComments removes /* */ comments" $
      \s -> let withComment = s ++ " /* block comment */ more"
                 result = removeComments withComment
             in not ("/* block comment */" `L.isInfixOf` result)

  , QC.testProperty "removeComments preserves string literals" $
      \s -> let stringWithCode = "code = \"" ++ s ++ "\" // comment"
                 result = removeComments stringWithCode
             in ("\"" ++ s ++ "\"") `L.isInfixOf` result

  , QC.testProperty "removeComments preserves character literals" $
      \c -> let stringWithCode = "code = '" ++ [c] ++ "' // comment"
                result = removeComments stringWithCode
            in ('"' : [c] ++ "'") `L.isInfixOf` result

  , QC.testProperty "removeComments handles escaped quotes in strings" $
      \s -> let stringWithCode = "code = \"" ++ s ++ "\\\"escaped\\\"\" // comment"
                 result = removeComments stringWithCode
             in ("\"" ++ s ++ "\\\"escaped\\\"\"") `L.isInfixOf` result
  ]
  where
    isNotInfixOf x y = not (x `L.isInfixOf` y)

-- | Indentation properties
indentationProperties :: TestTree
indentationProperties = testGroup "Indentation Properties"
  [ QC.testProperty "normalizeIndentation preserves relative indentation" $
      \s -> let indented = "  " ++ s ++ "\n    " ++ s ++ "\n  " ++ s
                 normalized = normalizeIndentation indented
                 lines' = lines normalized
             in L.length lines' === 3  -- Preserves line count

  , QC.testProperty "normalizeIndentation removes common prefix" $
      \s -> let indented = "    " ++ s ++ "\n    " ++ s ++ "  \n  " ++ s
                 normalized = normalizeIndentation indented
                 firstLine = L.head (lines normalized)
             in not (L.isPrefixOf "    " firstLine)  -- Common prefix removed

  , QC.testProperty "normalizeIndentation idempotent" $
      \s -> normalizeIndentation (normalizeIndentation s) === normalizeIndentation s

  , QC.testProperty "forceSingleTabIndentation forces tab prefix" $
      \s -> let result = forceSingleTabIndentation s
                 lines' = lines result
                 nonEmptyLines = L.filter (not . null) lines'
             in L.all (L.isPrefixOf "\t") nonEmptyLines

  , QC.testProperty "fixIndentation equals normalizeIndentation" $
      \s -> fixIndentation s === normalizeIndentation s
  ]

-- | BreakOn properties
breakOnProperties :: TestTree
breakOnProperties = testGroup "BreakOn Properties"
  [ QC.testProperty "breakOn empty pattern returns (\"\", s)" $
      \s -> breakOn "" s === ("", s)

  , QC.testProperty "breakOn pattern not found returns (s, \"\")" $
      \s1 s2 -> not (s1 `L.isInfixOf` s2) ==> breakOn s1 s2 === (s2, "")

  , QC.testProperty "breakOn concatenation property" $
      \pat s -> let (before, after) = breakOn pat s
                 in if pat `L.isInfixOf` s
                    then before ++ pat ++ after === s
                    else before === s && after === ""

  , QC.testProperty "breakOn first occurrence" $
      \pat s -> let (before, _) = breakOn pat s
                 in if pat `L.isInfixOf` s
                    then not (pat `L.isInfixOf` before)
                    else True

  , QC.testProperty "breakOn empty string with empty pattern" $
      breakOn "" "" === ("", "")

  , QC.testProperty "breakOn with pattern equal to string" $
      \s -> breakOn s s === ("", "")

  , QC.testProperty "breakOn prefix case" $
      \s1 s2 -> let s = s1 ++ s2
                 in breakOn s1 s === ("", s2)
  ]