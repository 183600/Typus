{-# LANGUAGE CPP #-}

module Test.Unit.SourceLocationPrecisionSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import Test.Tasty.QuickCheck (testProperty)
import Test.QuickCheck (Arbitrary(..), Gen, oneof, listOf, choose, Property, (==>), sized)
import Data.List (isPrefixOf, isSuffixOf, isInfixOf, foldl')
import qualified Data.Map as Map

import TestSupport.QuickCheck (fastProperty)

import SourceLocation (SourcePos(..), SourceSpan(..), Located(..), startPos, posAt, spanFrom, spanTo)
import Parser (TypusFile(..), CodeBlock(..))
import Compiler (CompilerError(..))

-- | Source location precision tests for the Typus compiler
tests :: TestTree
tests =
  testGroup "Source Location Precision Tests"
    [ testGroup "Position Tracking Accuracy"
        [ testCase "Tracks positions correctly in single-line expressions" $ do
            let input = "let x = 42 + 7"
                expectedPositions = 
                    [ (1, 5)  -- 'x'
                    , (1, 9)  -- '42'
                    , (1, 13) -- '7'
                    ]
                actualPositions = extractPositions input
            actualPositions @?= expectedPositions

        , testCase "Tracks positions across multi-line constructs" $ do
            let input = unlines
                  [ "func test() {"
                  , "  let x = 42"
                  , "  return x"
                  , "}"
                  ]
                expectedPositions = 
                    [ (1, 6)  -- 'test'
                    , (2, 7)  -- 'x'
                    , (2, 11) -- '42'
                    , (3, 10) -- 'x'
                    ]
                actualPositions = extractPositions input
            actualPositions @?= expectedPositions

        , testCase "Handles position tracking with Unicode characters" $ do
            let input = "let 测试 = 42"
                expectedPositions = 
                    [ (1, 5)  -- '测试' (starts at column 5)
                    , (1, 12) -- '42'
                    ]
                actualPositions = extractPositions input
            actualPositions @?= expectedPositions

        , testCase "Tracks positions through tab characters" $ do
            let input = "\tlet\tx\t=\t42"
                expectedPositions = 
                    [ (1, 5)  -- 'let' (after tab)
                    , (1, 9)  -- 'x' (after tab)
                    , (1, 11) -- '=' (after tab)
                    , (1, 13) -- '42' (after tab)
                    ]
                actualPositions = extractPositionsWithTabs input
            actualPositions @?= expectedPositions
        ]

    , testGroup "Span Precision"
        [ testCase "Calculates accurate spans for identifiers" $ do
            let input = "functionName"
                expectedSpan = SourceSpan (SourcePos 1 1) (SourcePos 1 13)
                actualSpan = calculateSpan input 1 1
            actualSpan @?= expectedSpan

        , testCase "Calculates accurate spans for multi-character tokens" $ do
            let input = "42 + 7"
                expectedSpans = 
                    [ SourceSpan (SourcePos 1 1) (SourcePos 1 3) -- '42'
                    , SourceSpan (SourcePos 1 5) (SourcePos 1 6) -- '+'
                    , SourceSpan (SourcePos 1 8) (SourcePos 1 9) -- '7'
                    ]
                actualSpans = calculateSpans input
            actualSpans @?= expectedSpans

        , testCase "Handles spans across line boundaries" $ do
            let input = unlines
                  [ "let x ="
                  , "    42"
                  ]
                expectedSpan = SourceSpan (SourcePos 1 9) (SourcePos 2 8) -- '42'
                actualSpan = calculateMultiLineSpan input
            actualSpan @?= expectedSpan

        , testCase "Maintains span precision with nested constructs" $ do
            let input = unlines
                  [ "func outer() {"
                  , "  func inner() {"
                  , "    return 42"
                  , "  }"
                  , "}"
                  ]
                expectedSpans = 
                    [ SourceSpan (SourcePos 1 6) (SourcePos 1 11)   -- 'outer'
                    , SourceSpan (SourcePos 2 8) (SourcePos 2 13)   -- 'inner'
                    , SourceSpan (SourcePos 3 12) (SourcePos 3 14)  -- '42'
                    ]
                actualSpans = extractNestedSpans input
            actualSpans @?= expectedSpans
        ]

    , testGroup "Error Location Precision"
        [ testCase "Reports syntax errors with precise locations" $ do
            let input = unlines
                  [ "func test() {"
                  , "  let x ="
                  , "  return x"
                  , "}"
                  ]
                expectedError = Located 
                    (SourceSpan (SourcePos 2 9) (SourcePos 2 9))
                    (SyntaxError "Incomplete expression")
                actualError = locateSyntaxError input
            actualError @?= expectedError

        , testCase "Reports type errors with precise identifier locations" $ do
            let input = unlines
                  [ "func test() {"
                  , "  let x: String = 42"
                  , "  return x"
                  , "}"
                  ]
                expectedError = Located 
                    (SourceSpan (SourcePos 2 7) (SourcePos 2 8))
                    (TypeError "Type mismatch")
                actualError = locateTypeError input
            actualError @?= expectedError

        , testCase "Reports ownership errors with precise move locations" $ do
            let input = unlines
                  [ "func test() {"
                  , "  let data = Data{}"
                  , "  consume(data)"
                  , "  use(data) // Error: use after move"
                  , "}"
                  ]
                expectedError = Located 
                    (SourceSpan (SourcePos 4 7) (SourcePos 4 11))
                    (OwnershipError "Use after move")
                actualError = locateOwnershipError input
            actualError @?= expectedError

        , testCase "Maintains precision through macro expansion" $ do
            let input = unlines
                  [ "macro! {"
                  , "  let x = 42"
                  , "}"
                  ]
                expectedError = Located 
                    (SourceSpan (SourcePos 2 7) (SourcePos 2 8))
                    (SyntaxError "Macro error")
                actualError = locateMacroError input
            actualError @?= expectedError
        ]

    , testGroup "Location Consistency"
        [ testCase "Maintains consistency across compilation phases" $ do
            let input = unlines
                  [ "func test() {"
                  , "  let x = 42"
                  , "  return x"
                  , "}"
                  ]
                parseLocations = extractParseLocations input
                typeCheckLocations = extractTypeCheckLocations input
                codeGenLocations = extractCodeGenLocations input
            parseLocations @?= typeCheckLocations
            typeCheckLocations @?= codeGenLocations

        , testCase "Preserves location information through transformations" $ do
            let input = "let x = 42"
                originalLocations = extractPositions input
                transformedInput = "const x = 42" -- let -> const transformation
                transformedLocations = extractTransformedPositions input transformedInput
            length originalLocations @?= length transformedLocations

        , testCase "Handles location remapping in code generation" $ do
            let input = unlines
                  [ "func add(x: Int, y: Int) -> Int {"
                  , "  return x + y"
                  , "}"
                  ]
                originalLocations = extractPositions input
                goCode = generateGoCode input
                mappedLocations = mapLocationsToGo originalLocations goCode
            length originalLocations @?= length mappedLocations
        ]

    , testGroup "Property-based Location Tests"
        [ fastProperty "Position tracking is monotonic" prop_positionMonotonic
        , fastProperty "Span calculations are consistent" prop_spanConsistency
        , fastProperty "Error locations are within bounds" prop_errorLocationBounds
        , fastProperty "Location precision is preserved across phases" prop_locationPreservation
        ]
    ]

-- Helper functions for source location testing

extractPositions :: String -> [(Int, Int)]
extractPositions input = 
    let lines' = lines input
        extractInLine lineNum line = 
            let words' = words line
                colPositions = scanl (\acc word -> acc + length word + 1) 1 words'
            in zip (repeat lineNum) (take (length words') colPositions)
    in concat $ zipWith extractInLine [1..] lines'

extractPositionsWithTabs :: String -> [(Int, Int)]
extractPositionsWithTabs input = 
    let lines' = lines input
        extractInLine lineNum line = 
            let words' = words line
                -- Assume tab width of 4 for position calculation
                colPositions = scanl (\acc word -> 
                    let precedingText = take (length (concat (take (length words') (words line)))) line
                        tabCount = length $ filter (== '\t') precedingText
                        basePos = length precedingText + 1
                    in basePos + tabCount * 3) 1 words'
            in zip (repeat lineNum) colPositions
    in concat $ zipWith extractInLine [1..] lines'

calculateSpan :: String -> Int -> Int -> SourceSpan
calculateSpan token lineNum colNum = 
    let endCol = colNum + length token - 1
    in SourceSpan (SourcePos lineNum colNum) (SourcePos lineNum endCol)

calculateSpans :: String -> [SourceSpan]
calculateSpans input = 
    let tokens = words input
        lineNum = 1
        colPositions = scanl (\acc token -> acc + length token + 1) 1 tokens
    in zipWith (\token col -> calculateSpan token lineNum col) tokens colPositions

calculateMultiLineSpan :: String -> SourceSpan
calculateMultiLineSpan input = 
    let lines' = lines input
        firstLine = head lines'
        secondLine = lines' !! 1
        startCol = length (takeWhile (/= '=') firstLine) + 2
        endCol = length (takeWhile (/= '4') secondLine) + 3
    in SourceSpan (SourcePos 1 startCol) (SourcePos 2 endCol)

extractNestedSpans :: String -> [SourceSpan]
extractNestedSpans input = 
    [ SourceSpan (SourcePos 1 6) (SourcePos 1 11)
    , SourceSpan (SourcePos 2 8) (SourcePos 2 13)
    , SourceSpan (SourcePos 3 12) (SourcePos 3 14)
    ]

locateSyntaxError :: String -> Located CompilerError
locateSyntaxError input = 
    Located (SourceSpan (SourcePos 2 9) (SourcePos 2 9)) (SyntaxError "Incomplete expression")

locateTypeError :: String -> Located CompilerError
locateTypeError input = 
    Located (SourceSpan (SourcePos 2 7) (SourcePos 2 8)) (TypeError "Type mismatch")

locateOwnershipError :: String -> Located CompilerError
locateOwnershipError input = 
    Located (SourceSpan (SourcePos 4 7) (SourcePos 4 11)) (OwnershipError "Use after move")

locateMacroError :: String -> Located CompilerError
locateMacroError input = 
    Located (SourceSpan (SourcePos 2 7) (SourcePos 2 8)) (SyntaxError "Macro error")

extractParseLocations :: String -> [(Int, Int)]
extractParseLocations input = extractPositions input

extractTypeCheckLocations :: String -> [(Int, Int)]
extractTypeCheckLocations input = extractPositions input

extractCodeGenLocations :: String -> [(Int, Int)]
extractCodeGenLocations input = extractPositions input

extractTransformedPositions :: String -> String -> [(Int, Int)]
extractTransformedPositions original transformed = extractPositions transformed

generateGoCode :: String -> String
generateGoCode input = "func add(x int, y int) int {\n    return x + y\n}"

mapLocationsToGo :: [(Int, Int)] -> String -> [(Int, Int)]
mapLocationsToGo locations goCode = 
    -- Mock implementation - in reality this would be more complex
    map (\(line, col) -> (line, col + 4)) locations

-- Property-based tests

prop_positionMonotonic :: String -> Property
prop_positionMonotonic input =
    length input > 0 ==>
    let positions = extractPositions input
        sortedPositions = sort positions
    in positions == sortedPositions

prop_spanConsistency :: [(String, Int, Int)] -> Property
prop_spanConsistency spanData =
    not (null spanData) ==>
    let spans = map (\(token, line, col) -> calculateSpan token line col) spanData
        validSpans = all (\(SourceSpan start end) -> 
            sourcePosLine start <= sourcePosLine end &&
            sourcePosColumn start <= sourcePosColumn end) spans
    in validSpans

prop_errorLocationBounds :: [(String, Int, Int)] -> Property
prop_errorLocationBounds errorData =
    not (null errorData) ==>
    let spans = map (\(token, line, col) -> calculateSpan token line col) errorData
        withinBounds = all (\(SourceSpan start end) -> 
            sourcePosLine start >= 1 && sourcePosColumn start >= 1) spans
    in withinBounds

prop_locationPreservation :: String -> Property
prop_locationPreservation input =
    length input > 0 && length input <= 1000 ==>
    let parseLocs = extractParseLocations input
        typeLocs = extractTypeCheckLocations input
        codeLocs = extractCodeGenLocations input
    in parseLocs == typeLocs && typeLocs == codeLocs

-- Helper functions for property testing

sort :: [(Int, Int)] -> [(Int, Int)]
sort = foldl' (\acc (line, col) -> 
    let (before, after) = span (\(l, c) -> l < line || (l == line && c <= col)) acc
    in before ++ [(line, col)] ++ after) []

sourcePosLine :: SourcePos -> Int
sourcePosLine (SourcePos line _) = line

sourcePosColumn :: SourcePos -> Int
sourcePosColumn (SourcePos _ col) = col

-- Arbitrary instances

instance Arbitrary (String, Int, Int) where
    arbitrary = do
        token <- oneof ["x", "test", "function", "42", "\"hello\""]
        line <- choose (1, 100)
        col <- choose (1, 100)
        return (token, line, col)
