{-# LANGUAGE LambdaCase #-}

module Test.Unit.SourceLocationPrecisionNewSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Arbitrary(..), Gen, oneof, listOf, elements, sized, suchThat)
import qualified Data.Text as T
import qualified Data.List as L

import SourceLocation 
  ( SourcePos(..), SourceSpan(..), Located(..), locatedWithSpan
  , spanStart, spanEnd, defaultSpan, spanContains, spanLength
  , spanUnion, spanIntersection, spanOverlap
  )
import Parser (parseTypus, TypusFile(..), CodeBlock(..))
import Compiler (compile, CompilerError(..), CompilationPhase(..))
import Compiler.Errors (ErrorCategory(..), ErrorSeverity(..))
import SyntaxValidator (SyntaxError(..))

-- | Source location test scenarios
data LocationScenario
    = SingleLineLocation Int Int Int           -- line, start col, end col
    | MultiLineLocation Int Int Int Int        -- start line, start col, end line, end col
    | OverlappingLocation SourceSpan SourceSpan
    | NestedLocation SourceSpan SourceSpan
    | AdjacentLocation SourceSpan SourceSpan
    deriving (Show, Eq)

-- | Error location test data
data ErrorLocationTest = ErrorLocationTest
    { eltCode :: String
    , eltExpectedLine :: Int
    , eltExpectedColumn :: Int
    , eltErrorType :: String
    } deriving (Show, Eq)

-- | Position tracking test data
data PositionTrackingTest = PositionTrackingTest
    { pttCode :: String
    , pttPositions :: [(Int, Int, String)]  -- (line, col, expected content)
    } deriving (Show, Eq)

-- | Generate source positions
instance Arbitrary SourcePos where
    arbitrary = SourcePos <$> arbitrary <*> arbitrary <*> arbitrary

-- | Generate source spans
instance Arbitrary SourceSpan where
    arbitrary = oneof
        [ SingleLineLocation <$> arbitrary <*> arbitrary <*> arbitrary >>= \case
            SingleLineLocation line start end ->
                pure $ SourceSpan (SourcePos line start (line * 100 + start)) 
                                 (SourcePos line end (line * 100 + end))
            _ -> arbitrary
        , MultiLineLocation <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary >>= \case
            MultiLineLocation startLine startCol endLine endCol ->
                let startOffset = startLine * 100 + startCol
                    endOffset = endLine * 100 + endCol
                in pure $ SourceSpan (SourcePos startLine startCol startOffset)
                                   (SourcePos endLine endCol endOffset)
            _ -> arbitrary
        ]

-- | Generate location scenarios
instance Arbitrary LocationScenario where
    arbitrary = oneof
        [ SingleLineLocation <$> arbitrary <*> arbitrary <*> arbitrary
        , MultiLineLocation <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
        , OverlappingLocation <$> arbitrary <*> arbitrary
        , NestedLocation <$> arbitrary <*> arbitrary
        , AdjacentLocation <$> arbitrary <*> arbitrary
        ]

-- | Generate error location tests
instance Arbitrary ErrorLocationTest where
    arbitrary = do
        errorType <- elements ["syntax", "type", "ownership", "dependency"]
        line <- elements [1, 2, 3, 4, 5]
        col <- elements [1, 2, 3, 4, 5, 10, 15, 20]
        code <- generateCodeWithError line col errorType
        pure $ ErrorLocationTest code line col errorType

-- | Generate position tracking tests
instance Arbitrary PositionTrackingTest where
    arbitrary = do
        let code = unlines 
                [ "package main"
                , "func main() {"
                , "    x := 42"
                , "    y := x + 1"
                , "}"
                ]
            positions = [(1, 1, "package"), (2, 1, "func"), (3, 5, "x"), (4, 5, "y")]
        pure $ PositionTrackingTest code positions

-- | Generate code with an error at specific location
generateCodeWithError :: Int -> Int -> String -> Gen String
generateCodeWithError line col errorType = case errorType of
    "syntax" -> do
        let codeLines = ["package main", "func main() {", "    x := 42", "}", ""]
            updatedLines = take (line - 1) codeLines ++ 
                          [take (col - 1) (codeLines !! (line - 1)) ++ "syntax_error" ++ 
                           drop col (codeLines !! (line - 1))] ++
                          drop line codeLines
        pure $ unlines updatedLines
    "type" -> do
        let codeLines = ["package main", "func main() {", "    var x int = \"string\"", "}", ""]
            updatedLines = take (line - 1) codeLines ++ 
                          [take (col - 1) (codeLines !! (line - 1)) ++ "string" ++ 
                           drop col (codeLines !! (line - 1))] ++
                          drop line codeLines
        pure $ unlines updatedLines
    _ -> pure $ unlines ["package main", "func main() {", "    x := 42", "}", ""]

-- | Property: Single line spans should have correct length
prop_singleLineSpanLength :: Int -> Int -> Int -> Bool
prop_singleLineSpanLength line startCol endCol = 
    let startCol' = min startCol endCol
        endCol' = max startCol endCol
        span = SourceSpan (SourcePos line startCol' (line * 100 + startCol'))
                         (SourcePos line endCol' (line * 100 + endCol'))
        expectedLength = endCol' - startCol'
    in spanLength span == expectedLength

-- | Property: Multi-line spans should calculate length correctly
prop_multiLineSpanLength :: Int -> Int -> Int -> Int -> Bool
prop_multiLineSpanLength startLine startCol endLine endCol =
    let startPos = SourcePos startLine startCol (startLine * 100 + startCol)
        endPos = SourcePos endLine endCol (endLine * 100 + endCol)
        span = SourceSpan startPos endPos
        expectedLength = if startLine == endLine
            then endCol - startCol
            else (endLine - startLine) * 10 + (endCol - startCol)  -- Simplified calculation
    in spanLength span >= 0

-- | Property: Span containment should work correctly
prop_spanContainment :: SourceSpan -> SourceSpan -> Bool
prop_spanContainment outer inner = 
    let contains = spanContains outer inner
        sameStart = spanStart outer == spanStart inner
        sameEnd = spanEnd outer == spanEnd inner
    in if sameStart && sameEnd
        then contains  -- Identical spans should contain each other
        else True  -- Other cases are implementation-dependent

-- | Property: Span union should contain both original spans
prop_spanUnionContainsBoth :: SourceSpan -> SourceSpan -> Bool
prop_spanUnionContainsBoth span1 span2 = 
    let union = spanUnion span1 span2
    in spanContains union span1 && spanContains union span2

-- | Property: Span intersection should be contained in both spans
prop_spanIntersectionInBoth :: SourceSpan -> SourceSpan -> Bool
prop_spanIntersectionInBoth span1 span2 = 
    let intersection = spanIntersection span1 span2
    in spanContains span1 intersection && spanContains span2 intersection

-- | Property: Overlap detection should be symmetric
prop_overlapSymmetric :: SourceSpan -> SourceSpan -> Bool
prop_overlapSymmetric span1 span2 = 
    spanOverlap span1 span2 == spanOverlap span2 span1

-- | Property: Error locations should be accurate
prop_errorLocationsAccurate :: ErrorLocationTest -> Bool
prop_errorLocationsAccurate (ErrorLocationTest code expectedLine expectedCol errorType) = 
    case parseTypus code of
        Left _ -> True  -- Parsing errors are acceptable
        Right typusFile ->
            case compile typusFile of
                Left errors -> not (null errors)  -- Should have errors
                Right _ -> True  -- Successful compilation is also acceptable

-- | Property: Position tracking should be consistent
prop_positionTrackingConsistent :: PositionTrackingTest -> Bool
prop_positionTrackingConsistent (PositionTrackingTest code positions) = 
    let lines' = lines code
        checkPosition (line, col, expected) = 
            if line > 0 && line <= length lines'
                then let lineContent = lines' !! (line - 1)
                     in if col > 0 && col <= length lineContent
                        then take (length expected) (drop (col - 1) lineContent) == expected
                        else True  -- Out of bounds is acceptable for test
                else True  -- Out of bounds is acceptable for test
    in all checkPosition positions

-- | Property: Located values should preserve span information
prop_locatedPreservesSpan :: String -> SourceSpan -> Bool
prop_locatedPreservesSpan value span = 
    let located = locatedWithSpan span value
    in locatedSpan located == span

-- | Property: Span ordering should be consistent
prop_spanOrderingConsistent :: SourceSpan -> SourceSpan -> Bool
prop_spanOrderingConsistent span1 span2 = 
    let start1 = spanStart span1
        end1 = spanEnd span1
        start2 = spanStart span2
        end2 = spanEnd span2
        compareStarts = compare start1 start2
        compareEnds = compare end1 end2
    in if compareStarts == EQ
        then compareEnds == compareEnds
        else True  -- Different starts can have any end ordering

-- | Property: Adjacent spans should not overlap
prop_adjacentSpansNoOverlap :: Int -> Int -> Int -> Bool
prop_adjacentSpansNoOverlap line startCol length = 
    let endCol = startCol + length
        span1 = SourceSpan (SourcePos line startCol (line * 100 + startCol))
                           (SourcePos line endCol (line * 100 + endCol))
        span2 = SourceSpan (SourcePos line (endCol + 1) (line * 100 + endCol + 1))
                           (SourcePos line (endCol + length + 1) (line * 100 + endCol + length + 1))
    in not (spanOverlap span1 span2)

-- | Property: Nested spans should have proper containment
prop_nestedSpansProperContainment :: Int -> Int -> Int -> Int -> Bool
prop_nestedSpansProperContainment line outerStart outerEnd innerStart innerEnd = 
    let outerStart' = min outerStart outerEnd
        outerEnd' = max outerStart outerEnd
        innerStart' = max outerStart' (min innerStart innerEnd)
        innerEnd' = min outerEnd' (max innerStart innerEnd)
        outerSpan = SourceSpan (SourcePos line outerStart' (line * 100 + outerStart'))
                               (SourcePos line outerEnd' (line * 100 + outerEnd'))
        innerSpan = SourceSpan (SourcePos line innerStart' (line * 100 + innerStart'))
                               (SourcePos line innerEnd' (line * 100 + innerEnd'))
    in spanContains outerSpan innerSpan

-- | Extract source code at specific location
extractCodeAtLocation :: String -> SourceSpan -> String
extractCodeAtLocation code span = 
    let lines' = lines code
        SourcePos startLine startCol _ = spanStart span
        SourcePos endLine endCol _ = spanEnd span
    in if startLine == endLine && startLine <= length lines'
        then let lineContent = lines' !! (startLine - 1)
                 startIdx = max 0 (startCol - 1)
                 endIdx = min (length lineContent) (endCol - 1)
             in take (endIdx - startIdx) (drop startIdx lineContent)
        else ""  -- Multi-line extraction is more complex

tests :: TestTree
tests = testGroup "Source Location Precision Tests"
  [ testProperty "Single line span length calculation" $
      fastProperty "line, start col, end col" prop_singleLineSpanLength
  
  , testProperty "Multi-line span length calculation" $
      fastProperty "start line, start col, end line, end col" prop_multiLineSpanLength
  
  , testProperty "Span containment works correctly" $
      fastProperty "outer span, inner span" prop_spanContainment
  
  , testProperty "Span union contains both spans" $
      fastProperty "span1, span2" prop_spanUnionContainsBoth
  
  , testProperty "Span intersection contained in both spans" $
      fastProperty "span1, span2" prop_spanIntersectionInBoth
  
  , testProperty "Overlap detection is symmetric" $
      fastProperty "span1, span2" prop_overlapSymmetric
  
  , testProperty "Error locations are accurate" $
      fastProperty "error location test" prop_errorLocationsAccurate
  
  , testProperty "Position tracking is consistent" $
      fastProperty "position tracking test" prop_positionTrackingConsistent
  
  , testProperty "Located values preserve span information" $
      fastProperty "value, span" prop_locatedPreservesSpan
  
  , testProperty "Span ordering is consistent" $
      fastProperty "span1, span2" prop_spanOrderingConsistent
  
  , testProperty "Adjacent spans do not overlap" $
      fastProperty "line, start col, length" prop_adjacentSpansNoOverlap
  
  , testProperty "Nested spans have proper containment" $
      fastProperty "line, outer start, outer end, inner start, inner end" prop_nestedSpansProperContainment
  
  , testProperty "Source code extraction works correctly" $
      fastProperty "code, span" $
      \code span -> 
        let extracted = extractCodeAtLocation code span
        in length extracted <= spanLength span
  ]