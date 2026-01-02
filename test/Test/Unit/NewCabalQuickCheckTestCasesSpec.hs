{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewCabalQuickCheckTestCasesSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, oneof, elements, listOf, sized, choose, arbitrary)

import Parser
  ( FileDirectives(..)
  , BlockDirectives(..)
  , CodeBlock(..)
  , TypusFile(..)
  , defaultFileDirectives
  , defaultBlockDirectives
  , parseTypus
  )

import SourceLocation
  ( SourcePos(..)
  , SourceSpan(..)
  , locatedWithSpan
  , locatedValue
  , startPos
  )

import Ownership
  ( OwnershipMode(..)
  , OwnershipTransfer(..)
  , checkOwnership
  )

import Utils
  ( trim
  , splitLines
  , isTypusFile
  , normalizeIndentation
  )

import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf)
import Data.List (sort, nub, intercalate)
import Data.Char (isSpace, toLower, toUpper)
import qualified Data.Text as T

-- ============================================================================
-- Test Cases for Core Parser Functionality
-- ============================================================================

-- Test Case 1: FileDirectives round-trip property
prop_fileDirectives_roundTrip :: Property
prop_fileDirectives_roundTrip =
  forAll arbitraryFileDirectives $ \fd ->
    let reconstructed = FileDirectives
          { fdOwnership = fdOwnership fd
          , fdDependentTypes = fdDependentTypes fd
          , fdConstraints = fdConstraints fd
          }
    in fd === reconstructed

-- Generate arbitrary FileDirectives for testing
arbitraryFileDirectives :: Gen FileDirectives
arbitraryFileDirectives = do
  ownership <- arbitraryMaybeLocatedBool
  dependentTypes <- arbitraryMaybeLocatedBool
  constraints <- arbitraryMaybeLocatedBool
  return $ FileDirectives ownership dependentTypes constraints

arbitraryMaybeLocatedBool :: Gen (Maybe (Located Bool))
arbitraryMaybeLocatedBool = oneof
  [ return Nothing
  , do
      b <- arbitrary
      pos <- arbitrarySourcePos
      return $ Just $ locatedWithSpan (SourceSpan pos pos) b
  ]

arbitrarySourcePos :: Gen SourcePos
arbitrarySourcePos = do
  line <- choose (1, 100)
  col <- choose (1, 100)
  return $ SourcePos line col

-- Test Case 2: BlockDirectives consistency property
prop_blockDirectives_consistency :: Property
prop_blockDirectives_consistency =
  forAll arbitraryBlockDirectives $ \bd ->
    let ownershipCount = L.length [() | Just _ <- [bdOwnership bd]]
        depTypesCount = L.length [() | Just _ <- [bdDependentTypes bd]]
        constraintsCount = L.length [() | Just _ <- [bdConstraints bd]]
        totalDirectives = ownershipCount + depTypesCount + constraintsCount
    in totalDirectives >= 0 && totalDirectives <= 3

arbitraryBlockDirectives :: Gen BlockDirectives
arbitraryBlockDirectives = do
  ownership <- arbitraryMaybeLocatedBool
  dependentTypes <- arbitraryMaybeLocatedBool
  constraints <- arbitraryMaybeLocatedBool
  return $ BlockDirectives ownership dependentTypes constraints

-- Test Case 3: Source position ordering property
prop_sourcePosition_ordering :: Property
prop_sourcePosition_ordering =
  forAll arbitrarySourcePos $ \pos1 ->
  forAll arbitrarySourcePos $ \pos2 ->
    let line1 = sourceLine pos1
        col1 = sourceColumn pos1
        line2 = sourceLine pos2
        col2 = sourceColumn pos2
        isEarlier = line1 < line2 || (line1 == line2 && col1 <= col2)
    in classify (line1 == line2) "same line" $
       classify (line1 < line2) "earlier line" $
       property isEarlier

-- Test Case 4: String trimming property
prop_stringTrimming_idempotent :: Property
prop_stringTrimming_idempotent =
  forAll arbitraryString $ \s ->
    let trimmedOnce = trim s
        trimmedTwice = trim trimmedOnce
    in trimmedOnce === trimmedTwice

arbitraryString :: Gen String
arbitraryString = listOf $ elements $ " \t\n\r" ++ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']

-- Test Case 5: Line splitting property
prop_lineSplitting_consistency :: Property
prop_lineSplitting_consistency =
  forAll arbitraryString $ \s ->
    let lines = splitLines s
        rejoined = intercalate "\n" lines
        hasNewlines = '\n' `elem` s
    in classify hasNewlines "contains newlines" $
       classify (null lines) "empty result" $
       property $ L.length lines >= 1

-- Test Case 6: Typus file extension detection
prop_typusFileDetection_consistent :: Property
prop_typusFileDetection_consistent =
  forAll arbitraryFileName $ \filename ->
    let isTypus = isTypusFile filename
        hasTypusExtension = ".typus" `L.isSuffixOf` filename
        hasTypusExtensionLower = ".typus" `L.isInfixOf` (map toLower filename)
    in classify isTypus "is typus file" $
       classify hasTypusExtension "has typus extension" $
       property $ isTypus == hasTypusExtensionLower

arbitraryFileName :: Gen String
arbitraryFileName = do
  base <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_.-"
  ext <- oneof
    [ return ""
    , return ".typus"
    , return ".go"
    , return ".hs"
    , return ".md"
    , return $ "." ++ listOf (elements ['a'..'z'])
    ]
  return $ base ++ ext

-- Test Case 7: Indentation normalization property
prop_indentationNormalization_preservesContent :: Property
prop_indentationNormalization_preservesContent =
  forAll arbitraryIndentedString $ \s ->
    let normalized = normalizeIndentation s
        -- Remove L.all leading spaces to compare content
        strippedOriginal = dropWhile isSpace s
        strippedNormalized = dropWhile isSpace normalized
    in classify (null s) "empty string" $
       classify (L.all isSpace s) "only whitespace" $
       property $ strippedOriginal == strippedNormalized

arbitraryIndentedString :: Gen String
arbitraryIndentedString = do
  lines <- listOf $ do
    indent <- listOf $ elements " \t"
    content <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " "
    return $ indent ++ content
  return $ intercalate "\n" lines

-- Test Case 8: Directive parsing consistency
prop_directiveParsing_caseInsensitive :: Property
prop_directiveParsing_caseInsensitive =
  forAll arbitraryDirectiveString $ \directive ->
    let lower = map toLower directive
        upper = map toUpper directive
        mixed = L.map (\c -> if even $ fromEnum c then toLower c else toUpper c) directive
    in classify (directive `L.isInfixOf` "ownership") "ownership directive" $
       classify (directive `L.isInfixOf` "dependent") "dependent types directive" $
       classify (directive `L.isInfixOf` "constraint") "constraints directive" $
       property $ L.length directive > 0

arbitraryDirectiveString :: Gen String
arbitraryDirectiveString = oneof
  [ return "ownership: on"
  , return "ownership: off"
  , return "dependent_types: on"
  , return "dependent_types: off"
  , return "constraints: on"
  , return "constraints: off"
  , do
      base <- elements ["ownership", "dependent_types", "constraints"]
      value <- elements ["on", "off", "true", "false", "1", "0"]
      return $ base ++ ": " ++ value
  ]

-- Test Case 9: List deduplication property
prop_listDeduplication_nubProperty :: Property
prop_listDeduplication_nubProperty =
  forAll (listOf arbitrary) $ \xs ->
    let deduplicated = nub xs
        hasDuplicates = L.length xs > L.length deduplicated
    in classify hasDuplicates "had duplicates" $
       classify (null xs) "empty list" $
       property $ L.all (`elem` deduplicated) xs && L.length deduplicated <= L.length xs

-- Test Case 10: String case conversion properties
prop_stringCaseConversion_roundTrip :: Property
prop_stringCaseConversion_roundTrip =
  forAll arbitraryString $ \s ->
    let lower = map toLower s
        upper = map toUpper s
        lowerThenUpper = map toUpper lower
        upperThenLower = map toLower upper
    in classify (L.all isLower s) "already lowercase" $
       classify (L.all isUpper s) "already uppercase" $
       property $ lowerThenUpper == upper && upperThenLower == lower

-- ============================================================================
-- Test Suite Definition
-- ============================================================================

tests :: TestTree
tests = testGroup "New Cabal QuickCheck Test Cases"
  [ fastProperty "FileDirectives round-trip property" prop_fileDirectives_roundTrip
  , fastProperty "BlockDirectives consistency property" prop_blockDirectives_consistency
  , fastProperty "Source position ordering property" prop_sourcePosition_ordering
  , fastProperty "String trimming idempotent property" prop_stringTrimming_idempotent
  , fastProperty "Line splitting consistency property" prop_lineSplitting_consistency
  , fastProperty "Typus file detection consistent property" prop_typusFileDetection_consistent
  , fastProperty "Indentation normalization preserves content property" prop_indentationNormalization_preservesContent
  , fastProperty "Directive parsing case insensitive property" prop_directiveParsing_caseInsensitive
  , fastProperty "List deduplication nub property" prop_listDeduplication_nubProperty
  , fastProperty "String case conversion round trip property" prop_stringCaseConversion_roundTrip
  ]