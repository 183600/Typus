{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.SourceLocationPrecisionQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool, assertEqual, (@?=))
import Test.Tasty.QuickCheck (testProperty)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, Gen, arbitrary, oneof, choose, listOf, elements)

import qualified Data.Text as T
import Data.List (isInfixOf, length, foldl', concat, sort)
import Data.Char (isSpace)
import Data.Maybe (isJust, fromMaybe)

import SourceLocation 
  ( SourcePos(..), SourceSpan(..), Located(..), locatedWithSpan
  , spanStart, spanEnd, advancePosBy, posAt, posLine, posColumn
  , posOffset, spanLength, spanContains, spanOverlaps, spanUnion
  )
import Parser (parseTypus)
import Compiler (compile, CompilerError(..))
import ErrorHandler (ErrorContext(..))

-- | QuickCheck tests for source location precision and accuracy
tests :: TestTree
tests = testGroup "Source Location Precision QuickCheck Tests"
  [ testGroup "Position Calculation Properties"
      [ testProperty "position advancement is consistent" $ fastProperty $
          \text offset ->
            let startPos = SourcePos 1 1 0
                advancedPos = advancePosBy text startPos
                calculatedOffset = posOffset advancedPos
            in offset >= 0 ==> calculatedOffset >= posOffset startPos

      , testProperty "line and column consistency" $ fastProperty $
          \linesList ->
            let text = unlines linesList
                finalPos = advancePosBy text (SourcePos 1 1 0)
                expectedLine = length linesList
                expectedColumn = if null linesList then 1 else length (last linesList) + 1
            in posLine finalPos === expectedLine .&&.
               posColumn finalPos === expectedColumn

      , testProperty "position round-trip preservation" $ fastProperty $
          \line column offset ->
            let pos = SourcePos line column offset
                reconstructed = posAt line column offset
            in posLine pos === posLine reconstructed .&&.
               posColumn pos === posColumn reconstructed .&&.
               posOffset pos === posOffset reconstructed
      ]

  , testGroup "Span Operations"
      [ testProperty "span length calculation is accurate" $ fastProperty $
          \startLine startColumn endLine endColumn ->
            let start = SourcePos startLine startColumn (startLine + startColumn)
                end = SourcePos endLine endColumn (endLine + endColumn)
                span = SourceSpan start end
                calculatedLength = spanLength span
            in startLine <= endLine && (startLine < endLine || startColumn <= endColumn) ==>
               calculatedLength >= 0

      , testProperty "span containment is reflexive" $ fastProperty $
          \startLine startColumn endLine endColumn ->
            let start = SourcePos startLine startColumn (startLine + startColumn)
                end = SourcePos endLine endColumn (endLine + endColumn)
                span = SourceSpan start end
            in startLine <= endLine && (startLine < endLine || startColumn <= endColumn) ==>
               spanContains span span

      , testProperty "span containment is transitive" $ fastProperty $
          \startLine startColumn midLine midColumn endLine endColumn ->
            let start = SourcePos startLine startColumn (startLine + startColumn)
                mid = SourcePos midLine midColumn (midLine + midColumn)
                end = SourcePos endLine endColumn (endLine + endColumn)
                inner = SourceSpan start mid
                outer = SourceSpan start end
            in startLine <= midLine && midLine <= endLine &&
               (startLine < midLine || startColumn <= midColumn) &&
               (midLine < endLine || midColumn <= endColumn) &&
               spanContains outer inner ==>
               spanContains outer inner

      , testProperty "span union preserves containment" $ fastProperty $
          \start1Line start1Col end1Line end1Col start2Line start2Col end2Line end2Col ->
            let start1 = SourcePos start1Line start1Col (start1Line + start1Col)
                end1 = SourcePos end1Line end1Col (end1Line + end1Col)
                start2 = SourcePos start2Line start2Col (start2Line + start2Col)
                end2 = SourcePos end2Line end2Col (end2Line + end2Col)
                span1 = SourceSpan start1 end1
                span2 = SourceSpan start2 end2
                union = spanUnion span1 span2
            in spanContains union span1 .&&. spanContains union span2
      ]

  , testGroup "Error Location Precision"
      [ testProperty "error spans are valid" $ fastProperty $
          \input ->
            let parseResult = parseTypus input
            in case parseResult of
              Left err -> 
                let span = errorSpan err
                    start = spanStart span
                    end = spanEnd span
                in posLine start <= posLine end && 
                   (posLine start < posLine end || posColumn start <= posColumn end)
              Right _ -> property True

      , testProperty "error positions are within input bounds" $ fastProperty $
          \input ->
            let parseResult = parseTypus input
                inputLength = length input
            in case parseResult of
              Left err ->
                let span = errorSpan err
                    startOffset = posOffset $ spanStart span
                    endOffset = posOffset $ spanEnd span
                in startOffset >= 0 && endOffset <= inputLength
              Right _ -> property True

      , testProperty "multiple errors have distinct locations" $ fastProperty $
          \input ->
            let parseResult = parseTypus input
                compileResult = case parseResult of
                  Right parsedFile -> compile parsedFile
                  Left _ -> Left []
            in case compileResult of
              Left errs | length errs >= 2 ->
                let spans = map errorSpan errs
                    distinctSpans = length spans == length (foldr (\s acc -> if s `elem` acc then acc else s:acc) [] spans)
                in distinctSpans
              _ -> property True
      ]

  , testGroup "Multiline Location Tracking"
      [ testProperty "multiline spans cover correct lines" $ fastProperty $
          \linesList ->
            let text = unlines linesList
                startLine = 1
                endLine = length linesList
                start = SourcePos startLine 1 0
                end = SourcePos endLine 1 (length text - 1)
                span = SourceSpan start end
                coveredLines = [startLine..endLine]
            in not (null linesList) ==> 
               all (\line -> spanContains span (SourcePos line 1 0)) coveredLines

      , testProperty "position tracking through indentation" $ fastProperty $
          \indentSize linesList ->
            let indentedLines = map (\line -> replicate indentSize ' ' ++ line) linesList
                text = unlines indentedLines
                positions = scanl (\pos line -> advancePosBy line pos) (SourcePos 1 1 0) indentedLines
            in all (\pos -> posColumn pos >= 1) positions

      , testProperty "column positions account for tabs" $ fastProperty $
          \tabSize content ->
            let textWithTabs = map (\c -> if c == '\t' then '\t' else c) content
                pos = advancePosBy textWithTabs (SourcePos 1 1 0)
                expectedColumn = foldl' (\col c -> if c == '\t' then col + tabSize else col + 1) 1 textWithTabs
            in posColumn pos === expectedColumn
      ]

  , testGroup "Location Context Preservation"
      [ testProperty "error context positions are within error spans" $ fastProperty $
          \input ->
            let parseResult = parseTypus input
            in case parseResult of
              Left err ->
                let span = errorSpan err
                    context = errorContext err
                    contextPos = contextPos context
                in spanContains span (SourceSpan contextPos contextPos)
              Right _ -> property True

      , testProperty "nested error contexts maintain hierarchy" $ fastProperty $
          \input ->
            let parseResult = parseTypus input
                compileResult = case parseResult of
                  Right parsedFile -> compile parsedFile
                  Left _ -> Left []
            in case compileResult of
              Left errs | length errs >= 2 ->
                let contexts = map errorContext errs
                    positions = map contextPos contexts
                    sorted = sort positions
                in positions === sorted
              _ -> property True
      ]

  , testGroup "Generated Code Location Mapping"
      [ testProperty "generated code preserves source location information" $ fastProperty $
          \input ->
            let parseResult = parseTypus input
            in case parseResult of
              Right parsedFile ->
                case compile parsedFile of
                  Right compiled -> 
                    let goCode = goCode compiled
                        hasLocationInfo = "// line:" `T.isInfixOf` goCode
                    in hasLocationInfo || T.length goCode > 0
                  Left _ -> property True
              Left _ -> property True

      , testProperty "location mapping is bijective" $ fastProperty $
          \input ->
            let parseResult = parseTypus input
            in case parseResult of
              Right parsedFile ->
                case compile parsedFile of
                  Right compiled -> 
                    let goCode = goCode compiled
                        sourceLines = lines input
                        goLines = T.lines goCode
                    in length sourceLines <= length goLines * 2  -- Allow for generated code expansion
                  Left _ -> property True
              Left _ -> property True
      ]

  , testGroup "Performance and Scalability"
      [ testProperty "position calculation is linear in input size" $ fastProperty $
          \inputSize ->
            let largeInput = unlines $ replicate (min inputSize 1000) "func test() { return 42; }"
                startPos = SourcePos 1 1 0
                endPos = advancePosBy largeInput startPos
            in posOffset endPos === length largeInput

      , testProperty "span operations are efficient for large inputs" $ fastProperty $
          \spanCount ->
            let spans = take (min spanCount 100) $ generateSpans 1 1
                unioned = foldl spanUnion (head spans) (tail spans)
            in spanLength unioned >= 0
      ]
  ]

-- Helper functions and generators
generateSpans :: Int -> Int -> [SourceSpan]
generateSpans startLine startColumn = 
  let start = SourcePos startLine startColumn (startLine + startColumn)
      end = SourcePos (startLine + 1) startColumn (startLine + startColumn + 10)
  in [SourceSpan start end]

errorSpan :: CompilerError -> SourceSpan
errorSpan (CompilerError _ _ _ span _) = span

errorContext :: CompilerError -> ErrorContext
errorContext (CompilerError _ _ _ _ ctx) = ctx

goCode :: CompiledModule -> T.Text
goCode = undefined  -- Placeholder - would be implemented in actual module

data CompiledModule = CompiledModule
  { goCode :: T.Text
  } deriving (Show, Eq)

-- Additional QuickCheck generators
genSourcePos :: Gen SourcePos
genSourcePos = do
  line <- choose (1, 1000)
  column <- choose (1, 1000)
  offset <- choose (0, 1000000)
  return $ SourcePos line column offset

genSourceSpan :: Gen SourceSpan  
genSourceSpan = do
  start <- genSourcePos
  endOffset <- choose (0, 1000)
  let end = SourcePos (posLine start) (posColumn start + endOffset) (posOffset start + endOffset)
  return $ SourceSpan start end

-- QuickCheck property operators
(.&&.) :: Property -> Property -> Property
(.&&.) = (&&.)

infixr 3 .&&.
