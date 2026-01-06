{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.NewEnhancedTestSpec where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertEqual, assertBool)
import Test.Tasty.QuickCheck (testProperty)
import Test.QuickCheck (Arbitrary(..), Gen, Property, (==>), forAll, choose, listOf1, elements, resize)
import TestSupport.QuickCheck (fastProperty)
import qualified Data.Text as T
import qualified Data.List as L
import Data.Char (isSpace, isAlphaNum, isDigit, isLetter)
import Data.Maybe (isJust, isNothing, fromMaybe)

import Utils (trim, splitBy, splitByCollapsed, splitByComma, removeComments, normalizeIndentation)
import SourceLocation (SourcePos(..), SourceSpan(..), Located(..), startPos, posAfter, emptySpan, spanFrom, mergeSpans, isValidSpan)
import Parser (FileDirectives(..), BlockDirectives(..), defaultFileDirectives, defaultBlockDirectives)
import Compiler.Errors.Core (ErrorSeverity(..), ErrorCategory(..), ErrorLocation(..), ErrorContext(..), emptyContext, formatError)

-- ============================================================================
-- Test Suite Definition
-- ============================================================================

tests :: TestTree
tests = testGroup "New Enhanced Test Suite"
  [ textProcessingProperties
  , sourceLocationMathProperties
  , parsingDirectiveProperties
  , errorHandlingProperties
  , utilsBoundaryProperties
  , parserConsistencyProperties
  , locationTrackingProperties
  , commentHandlingProperties
  ]

-- ============================================================================
-- Text Processing Properties
-- ============================================================================

textProcessingProperties :: TestTree
textProcessingProperties = testGroup "Text Processing Properties"
  [ testProperty "trim is idempotent" $
      \s -> trim (trim s) == trim s

  , testProperty "splitBy preserves concatenation with delimiter" $
      \c s -> splitBy c s `L.intercalate` [c] == s

  , testProperty "splitByCollapsed removes empty segments" $
      \c s -> L.all (not . null) (splitByCollapsed c s)

  , testProperty "normalizeIndentation preserves relative structure" $
      \lineList -> let normalized = normalizeIndentation lineList
                       indentLevels = L.map (L.length . takeWhile isSpace) normalized
                   in L.all (>= 0) indentLevels

  , testCase "trim handles empty strings" $
      assertEqual "trim empty" "" (trim "")

  , testCase "trim handles whitespace-only strings" $
      assertEqual "trim whitespace" "" (trim "   \t\n  ")
  ]

-- ============================================================================
-- Source Location Math Properties
-- ============================================================================

sourceLocationMathProperties :: TestTree
sourceLocationMathProperties = testGroup "Source Location Math Properties"
  [ testProperty "posAfter advances column by 1 for same line" $
      \pos -> posAfter pos (SourcePos 0 1) == SourcePos (sourceLine pos) (sourceColumn pos + 1)

  , testProperty "spanFrom creates valid spans" $
      \pos -> isValidSpan (spanFrom pos 5)

  , testProperty "mergeSpans is commutative for overlapping spans" $
      \span1 span2 -> let merged1 = mergeSpans span1 span2
                          merged2 = mergeSpans span2 span1
                      in spanStart merged1 == spanStart merged2 && spanEnd merged1 == spanEnd merged2

  , testProperty "emptySpan has zero L.length" $
      \pos -> let span = emptySpan pos
              in spanStart span == spanEnd span

  , testCase "startPos is at line 1, column 1" $
      assertEqual "startPos" (SourcePos 1 1) startPos
  ]

-- ============================================================================
-- Parsing Directive Properties
-- ============================================================================

parsingDirectiveProperties :: TestTree
parsingDirectiveProperties = testGroup "Parsing Directive Properties"
  [ testProperty "default file directives have no values" $
      let defaults = defaultFileDirectives
      in isNothing (fdOwnership defaults) &&
         isNothing (fdDependentTypes defaults) &&
         isNothing (fdConstraints defaults)

  , testProperty "default block directives have no values" $
      let defaults = defaultBlockDirectives
      in isNothing (bdOwnership defaults) &&
         isNothing (bdDependentTypes defaults) &&
         isNothing (bdConstraints defaults)

  , testCase "file directives roundtrip" $
      let directives = FileDirectives
            { fdOwnership = Just (Located (SourcePos 1 1) True)
            , fdDependentTypes = Just (Located (SourcePos 1 2) False)
            , fdConstraints = Nothing
            }
      in assertEqual "ownership" True (locatedValue $ fromMaybe (Located (SourcePos 0 0) False) (fdOwnership directives))
  ]

-- ============================================================================
-- Error Handling Properties
-- ============================================================================

errorHandlingProperties :: TestTree
errorHandlingProperties = testGroup "Error Handling Properties"
  [ testProperty "empty context has no information" $
      let context = emptyContext
      in null context

  , testProperty "error severity ordering" $
      \sev1 sev2 -> (sev1 == ErrorError && sev2 == ErrorWarning) ==> 
        let severityLevel ErrorCritical = 3
            severityLevel ErrorError = 2
            severityLevel ErrorWarning = 1
            severityLevel ErrorInfo = 0
        in severityLevel sev1 > severityLevel sev2

  , testProperty "error categories are distinct" $
      \cat1 cat2 -> (cat1 /= cat2) ==> 
        let categoryToString ErrorSyntax = "Syntax"
            categoryToString ErrorType = "Type"
            categoryToString ErrorOwnership = "Ownership"
            categoryToString ErrorDependentType = "DependentType"
            categoryToString ErrorParsing = "Parsing"
        in categoryToString cat1 /= categoryToString cat2

  , testCase "format error includes location" $
      let error = formatError "Test error" ErrorError (ErrorLocation (SourcePos 1 1) (SourcePos 1 5)) emptyContext
      in assertBool "error contains position" ("1:1" `L.isInfixOf` error)
  ]

-- ============================================================================
-- Utils Boundary Properties
-- ============================================================================

utilsBoundaryProperties :: TestTree
utilsBoundaryProperties = testGroup "Utils Boundary Properties"
  [ testProperty "splitBy handles single character strings" $
      \c -> splitBy c [c] == ["", ""]

  , testProperty "splitByCollapsed handles empty input" $
      \c -> L.null (splitByCollapsed c "")

  , testProperty "trim preserves non-whitespace content" $
      \s -> not (L.all isSpace s) ==> not (L.null (trim s))

  , testProperty "splitByComma handles consecutive commas" $
      \s -> splitByComma (",," ++ s ++ ",,") `L.L.isPrefixOf` ["", "", ""]

  , testCase "removeComments handles empty input" $
      assertEqual "removeComments empty" "" (removeComments "")
  ]

-- ============================================================================
-- Parser Consistency Properties
-- ============================================================================

parserConsistencyProperties :: TestTree
parserConsistencyProperties = testGroup "Parser Consistency Properties"
  [ testProperty "file directives maintain consistency" $
      \ownership dependent constraints ->
        let directives = FileDirectives
              { fdOwnership = if ownership then Just (Located (SourcePos 1 1) True) else Nothing
              , fdDependentTypes = if dependent then Just (Located (SourcePos 1 2) True) else Nothing
              , fdConstraints = if constraints then Just (Located (SourcePos 1 3) True) else Nothing
              }
        in (isJust (fdOwnership directives)) == ownership &&
           (isJust (fdDependentTypes directives)) == dependent &&
           (isJust (fdConstraints directives)) == constraints

  , testProperty "block directives maintain consistency" $
      \ownership dependent constraints ->
        let directives = BlockDirectives
              { bdOwnership = if ownership then Just (Located (SourcePos 2 1) False) else Nothing
              , bdDependentTypes = if dependent then Just (Located (SourcePos 2 2) False) else Nothing
              , bdConstraints = if constraints then Just (Located (SourcePos 2 3) False) else Nothing
              }
        in (isJust (bdOwnership directives)) == ownership &&
           (isJust (bdDependentTypes directives)) == dependent &&
           (isJust (bdConstraints directives)) == constraints
  ]

-- ============================================================================
-- Location Tracking Properties
-- ============================================================================

locationTrackingProperties :: TestTree
locationTrackingProperties = testGroup "Location Tracking Properties"
  [ testProperty "located values preserve their content" $
      \pos val -> locatedValue (Located pos val) == val

  , testProperty "located values track their position" $
      \pos val -> locatedPos (Located pos val) == pos

  , testProperty "span positions are ordered" $
      \pos1 pos2 len -> 
        let span = spanFrom pos1 len
        in sourceLine (spanStart span) <= sourceLine (spanEnd span)

  , testCase "empty span at specific position" $
      let pos = SourcePos 5 10
          span = emptySpan pos
      in assertEqual "empty span start" pos (spanStart span) `seq`
         assertEqual "empty span end" pos (spanEnd span)
  ]

-- ============================================================================
-- Comment Handling Properties
-- ============================================================================

commentHandlingProperties :: TestTree
commentHandlingProperties = testGroup "Comment Handling Properties"
  [ testProperty "removeComments preserves non-comment content" $
      \s -> not ('/' `elem` s) ==> removeComments s == s

  , testProperty "removeComments handles line comments" $
      \s -> removeComments ("// " ++ s) == ""

  , testProperty "removeComments handles block comments" $
      \s -> removeComments ("/* " ++ s ++ " */") == ""

  , testCase "removeComments handles nested line comments" $
      assertEqual "nested line comments" "" (removeComments "// comment // another")

  , testCase "removeComments handles mixed comments" $
      let input = "code /* block */ // line\nmore code"
          expected = "code  \nmore code"
      in assertEqual "mixed comments" expected (removeComments input)
  ]

-- ============================================================================
-- Helper Functions
-- ============================================================================

isInfixOf :: String -> String -> Bool
isInfixOf = L.L.isInfixOf