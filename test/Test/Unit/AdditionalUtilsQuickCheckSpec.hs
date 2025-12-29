{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.AdditionalUtilsQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))

import Utils
  ( trim
  , splitBy
  , splitByCollapsed
  , splitByComma
  , splitByCommaCollapsed
  , removeLineComments
  , removeComments
  , normalizeIndentation
  , breakOn
  )

import Data.Char (isSpace, isAlphaNum, isLetter)
import qualified Data.List as Data.List
import Data.List (isPrefixOf, tails, isInfixOf, sort, nub)
import Data.String (IsString)

-- Property: trim is idempotent (trimming twice gives same result as trimming once)
prop_trim_idempotent :: String -> Property
prop_trim_idempotent str =
  let trimmedOnce = trim str
      trimmedTwice = trim trimmedOnce
  in property $ trimmedOnce === trimmedTwice

-- Property: trim never adds characters
prop_trim_never_adds :: String -> Property
prop_trim_never_adds str =
  let trimmed = trim str
  in property $ length trimmed <= length str

-- Property: splitBy and splitByCollapsed relationship
prop_splitBy_splitByCollapsed :: String -> Char -> Property
prop_splitBy_splitByCollapsed str delim =
  let normal = splitBy delim str
      collapsed = splitByCollapsed delim str
      collapsedFromNormal = filter (not . null) normal
  in property $ collapsed === collapsedFromNormal

-- Property: splitByComma is equivalent to splitBy ','
prop_splitByComma_equals_splitBy :: String -> Property
prop_splitByComma_equals_splitBy str =
  let byComma = splitByComma str
      byChar = splitBy ',' str
  in property $ byComma === byChar

-- Property: splitByCommaCollapsed is equivalent to splitByCollapsed ','
prop_splitByCommaCollapsed_equals_splitByCollapsed :: String -> Property
prop_splitByCommaCollapsed_equals_splitByCollapsed str =
  let byCommaCollapsed = splitByCommaCollapsed str
      byCharCollapsed = splitByCollapsed ',' str
  in property $ byCommaCollapsed === byCharCollapsed

-- Property: splitBy preserves total content when rejoined
prop_splitBy_preserves_content :: String -> Char -> Property
prop_splitBy_preserves_content str delim =
  let segments = splitBy delim str
      rejoined = Data.List.intercalate [delim] segments
  in property $ rejoined === str

-- Property: splitBy on empty string returns singleton empty
prop_splitBy_empty_string :: Char -> Property
prop_splitBy_empty_string delim =
  let result = splitBy delim ""
  in property $ result === [""]

-- Property: splitByCollapsed on string with only delimiters returns empty
prop_splitByCollapsed_only_delimiters :: Char -> Int -> Property
prop_splitByCollapsed_only_delimiters delim n =
  n > 0 ==>
  let str = replicate n delim
      result = splitByCollapsed delim str
  in property $ result === []

-- Property: removeLineComments doesn't affect strings without comment markers
prop_removeLineComments_no_comment_markers :: String -> Property
prop_removeLineComments_no_comment_markers str =
  not ("//" `isInfixOf` str) ==>
  let cleaned = removeLineComments str
  in property $ cleaned === str

-- Property: removeComments preserves content without block comments
prop_removeComments_no_block_comments :: String -> Property
prop_removeComments_no_block_comments str =
  not ("/*" `isInfixOf` str) ==>
  let cleaned = removeComments str
  in property $ cleaned === str

-- Property: removeLineComments is idempotent
prop_removeLineComments_idempotent :: String -> Property
prop_removeLineComments_idempotent str =
  let cleanedOnce = removeLineComments str
      cleanedTwice = removeLineComments cleanedOnce
  in property $ cleanedOnce === cleanedTwice

-- Property: removeComments is idempotent
prop_removeComments_idempotent :: String -> Property
prop_removeComments_idempotent str =
  let cleanedOnce = removeComments str
      cleanedTwice = removeComments cleanedOnce
  in property $ cleanedOnce === cleanedTwice

-- Property: normalizeIndentation preserves relative structure
prop_normalizeIndentation_preserves_lines :: String -> Property
prop_normalizeIndentation_preserves_lines str =
  let normalized = normalizeIndentation str
      originalLines = lines str
      normalizedLines = lines normalized
  in property $ length originalLines === length normalizedLines

-- Property: breakOn always returns a tuple where second part starts with delimiter or is empty
prop_breakOn_structure :: String -> Char -> Property
prop_breakOn_structure str delim =
  let (before, after) = breakOn delim str
  in property $ null after || head after == delim

-- Property: breakOn on empty string returns empty tuple
prop_breakOn_empty :: Char -> Property
prop_breakOn_empty delim =
  let result = breakOn delim ""
  in property $ result === ("", "")

-- Property: breakOn with delimiter not in string returns original string and empty suffix
prop_breakOn_delimiter_not_found :: String -> Char -> Property
prop_breakOn_delimiter_not_found str delim =
  not (delim `elem` str) ==>
  let (before, after) = breakOn delim str
  in property $ (before, after) === (str, "")

-- Property: splitBy respects delimiter count
prop_splitBy_delimiter_count :: String -> Char -> Property
prop_splitBy_delimiter_count str delim =
  let segments = splitBy delim str
      expectedCount = length (filter (== delim) str) + 1
  in property $ length segments === expectedCount

-- Property: trim preserves non-space characters
prop_trim_preserves_non_space :: String -> Property
prop_trim_preserves_non_space str =
  let trimmed = trim str
      originalNonSpaces = filter (not . isSpace) str
      trimmedNonSpaces = filter (not . isSpace) trimmed
  in property $ originalNonSpaces === trimmedNonSpaces

-- Property: removeLineComments preserves line structure
prop_removeLineComments_preserves_lines :: String -> Property
prop_removeLineComments_preserves_lines str =
  let cleaned = removeLineComments str
      originalLines = lines str
      cleanedLines = lines cleaned
  in property $ length originalLines === length cleanedLines

tests :: TestTree
tests =
  testGroup "Additional Utils QuickCheck tests"
    [ testGroup "Trim properties"
        [ fastProperty "trim is idempotent" prop_trim_idempotent
        , fastProperty "trim never adds characters" prop_trim_never_adds
        , fastProperty "trim preserves non-space characters" prop_trim_preserves_non_space
        ]

    , testGroup "Split properties"
        [ fastProperty "splitBy/splitByCollapsed relationship" prop_splitBy_splitByCollapsed
        , fastProperty "splitByComma equals splitBy ','" prop_splitByComma_equals_splitBy
        , fastProperty "splitByCommaCollapsed equals splitByCollapsed ','" prop_splitByCommaCollapsed_equals_splitByCollapsed
        , fastProperty "splitBy preserves content when rejoined" prop_splitBy_preserves_content
        , fastProperty "splitBy empty string returns singleton empty" prop_splitBy_empty_string
        , fastProperty "splitByCollapsed only delimiters returns empty" prop_splitByCollapsed_only_delimiters
        , fastProperty "splitBy respects delimiter count" prop_splitBy_delimiter_count
        ]

    , testGroup "Comment removal properties"
        [ fastProperty "removeLineComments no comment markers" prop_removeLineComments_no_comment_markers
        , fastProperty "removeComments no block comments" prop_removeComments_no_block_comments
        , fastProperty "removeLineComments is idempotent" prop_removeLineComments_idempotent
        , fastProperty "removeComments is idempotent" prop_removeComments_idempotent
        , fastProperty "removeLineComments preserves line structure" prop_removeLineComments_preserves_lines
        ]

    , testGroup "Indentation properties"
        [ fastProperty "normalizeIndentation preserves lines" prop_normalizeIndentation_preserves_lines
        ]

    , testGroup "Break on properties"
        [ fastProperty "breakOn structure" prop_breakOn_structure
        , fastProperty "breakOn empty string" prop_breakOn_empty
        , fastProperty "breakOn delimiter not found" prop_breakOn_delimiter_not_found
        ]
    ]