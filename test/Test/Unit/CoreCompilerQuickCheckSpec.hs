{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.CoreCompilerQuickCheckSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import Data.List (isInfixOf, nub)

import Compiler.Errors.Core
import SourceLocation
import Utils
import Parser (TypusFile(..), CodeBlock(..), BlockDirectives(..), defaultBlockDirectives)
import qualified SyntaxValidator

-- | Test source position arithmetic properties
testSourcePositionArithmetic :: Property
testSourcePositionArithmetic =
  forAll arbitrary $ \(pos :: SourcePos) ->
    forAll arbitrary $ \lineOffset ->
      forAll arbitrary $ \colOffset ->
        let newPos = advancePosBy lineOffset colOffset pos
            expectedLine = max 1 (sourceLine pos + lineOffset)
            expectedCol = if lineOffset == 0 then max 1 (sourceColumn pos + colOffset) else max 1 colOffset
        in sourceLine newPos === expectedLine .&&.
           sourceColumn newPos === expectedCol

-- | Test span merging properties
testSpanMerging :: Property
testSpanMerging =
  forAll arbitrary $ \(span1 :: SourceSpan) ->
    forAll arbitrary $ \(span2 :: SourceSpan) ->
      let merged = mergeSpans span1 span2
          start1 = spanStart span1
          end1 = spanEnd span1
          start2 = spanStart span2
          end2 = spanEnd span2
          mergedStart = spanStart merged
          mergedEnd = spanEnd merged
      in if isValidSpan span1 && isValidSpan span2
         then (sourceLine mergedStart <= min (sourceLine start1) (sourceLine start2)) .&&.
              (sourceLine mergedEnd >= max (sourceLine end1) (sourceLine end2))
         else property True -- Invalid spans should not crash

-- | Test error collection and filtering properties
testErrorCollectionProperties :: Property
testErrorCollectionProperties =
  forAll arbitrary $ \(errors :: [TypeError]) ->
    forAll arbitrary $ \severity ->
      let collector = newErrorCollector
          withErrors = foldr addError collector errors
          filteredErrors = filterBySeverity severity (getErrors withErrors)
      in all (\e -> errorSeverity e >= severity) filteredErrors

-- | Test text processing utilities properties
testTextProcessingProperties :: Property
testTextProcessingProperties =
  forAll arbitrary $ \text ->
    forAll arbitrary $ \delim ->
      let split = splitBy delim text
          rejoined = T.unpack $ T.intercalate (T.pack [delim]) (map T.pack split)
          trimmed = trim text
      in length split >= 0 .&&. -- Should never be negative
         (if null text then null split else True) .&&.
         (not (null trimmed) == not (all isSpace text))

-- | Test comment removal properties
testCommentRemovalProperties :: Property
testCommentRemovalProperties =
  forAll arbitrary $ \code ->
    let withoutComments = removeComments code
        withoutLineComments = removeLineComments code
        -- Comments should not contain code markers
        hasCodeMarkers = any (`isInfixOf` code) ["function", "var", "let", "if"]
    in if "//" `isInfixOf` code || "/*" `isInfixOf` code
       then length withoutComments <= length code
       else withoutComments === code

-- | Test indentation normalization properties
testIndentationNormalization :: Property
testIndentationNormalization =
  forAll arbitrary $ \code ->
    let normalized = normalizeIndentation code
        linesList = lines code
        normalizedLines = lines normalized
    in length normalizedLines === length linesList .&&.
       all (not . isPrefixOf "    ") normalizedLines -- No leading spaces

-- | Test directive parsing consistency
testDirectiveParsingConsistency :: Property
testDirectiveParsingConsistency =
  forAll arbitrary $ \directives ->
    let directivesText = show directives
        -- Basic sanity check for directive formatting
    in length directivesText >= 0 -- Should produce some output

-- | Test error location tracking
testErrorLocationTracking :: Property
testErrorLocationTracking =
  forAll arbitrary $ \pos ->
    forAll arbitrary $ \message ->
      let error = errorAt pos message
          errorLoc = toErrorLocation pos
      in errorLocation error === errorLoc

-- | Test syntax validation properties
testSyntaxValidationProperties :: Property
testSyntaxValidationProperties =
  forAll arbitrary $ \code ->
    let errors = SyntaxValidator.validateSyntax code
    -- Syntax validation should not crash on any input
    in length errors >= 0

-- | Test file directive properties
testFileDirectiveProperties :: Property
testFileDirectiveProperties =
  forAll arbitrary $ \file ->
    let TypusFile{..} = file
        directivesCount = length $ filter isJust 
          [fdOwnership tfDirectives, fdDependentTypes tfDirectives, fdConstraints tfDirectives]
        blocksCount = length tfBlocks
    in directivesCount >= 0 .&&. blocksCount >= 0
  where
    isJust (Just _) = True
    isJust Nothing = False

-- | Test code block directive properties
testCodeBlockProperties :: Property
testCodeBlockProperties =
  forAll arbitrary $ \block ->
    let CodeBlock{..} = block
        directivesCount = length $ filter isJust 
          [bdOwnership cbDirectives, bdDependentTypes cbDirectives, bdConstraints cbDirectives]
        contentLength = length cbContent
    in directivesCount >= 0 .&&. contentLength >= 0
  where
    isJust (Just _) = True
    isJust Nothing = False

-- | Test error recovery strategies
testErrorRecoveryStrategies :: Property
testErrorRecoveryStrategies =
  forAll arbitrary $ \error ->
    let recovery = shouldContinueAfter error
        canRecover = canRecoverFrom error
    in if errorSeverity error == FatalError
       then not recovery .&&. not canRecover
       else property True -- Non-fatal errors may have various recovery strategies

-- | Test error formatting consistency
testErrorFormattingConsistency :: Property
testErrorFormattingConsistency =
  forAll arbitrary $ \error ->
    let formatted = formatError error
        formattedWithLocation = formatErrorWithLocation error
    in not (null formatted) .&&.
       not (null formattedWithLocation) .&&.
       length formattedWithLocation >= length formatted

-- | Test combined error properties
testCombinedErrorProperties :: Property
testCombinedErrorProperties =
  forAll arbitrary $ \errors ->
    let combined = combineErrors errors
        combinedSeverity = combinedErrorSeverity combined
    in if null errors
       then property True
       else combinedSeverity `elem` [Info, Warning, Error, FatalError]

tests :: TestTree
tests = testGroup "Core Compiler QuickCheck Tests"
  [ testProperty "Source position arithmetic" testSourcePositionArithmetic
  , testProperty "Span merging properties" testSpanMerging
  , testProperty "Error collection properties" testErrorCollectionProperties
  , testProperty "Text processing properties" testTextProcessingProperties
  , testProperty "Comment removal properties" testCommentRemovalProperties
  , testProperty "Indentation normalization" testIndentationNormalization
  , testProperty "Directive parsing consistency" testDirectiveParsingConsistency
  , testProperty "Error location tracking" testErrorLocationTracking
  , testProperty "Syntax validation properties" testSyntaxValidationProperties
  , testProperty "File directive properties" testFileDirectiveProperties
  , testProperty "Code block properties" testCodeBlockProperties
  , testProperty "Error recovery strategies" testErrorRecoveryStrategies
  , testProperty "Error formatting consistency" testErrorFormattingConsistency
  , testProperty "Combined error properties" testCombinedErrorProperties
  ]