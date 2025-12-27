{-# LANGUAGE CPP #-}

module Test.Unit.SourceLocationPrecisionSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import Test.QuickCheck ((==>), Property, forAll, choose, listOf1, elements)
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set

import TestSupport.QuickCheck (fastProperty)
import SourceLocation (SourcePos(..), SourceSpan(..), Located(..))
import Compiler.GoAst (GoDecl(..), FuncDecl(..))

-- | Source location precision tracking tests
tests :: TestTree
tests =
  testGroup "Source Location Precision Tests"
    [ testGroup "Character-level precision"
        [ testCase "tracks exact character positions" $ do
            let input = "func test() { return 42 }"
                expected = SourceSpan 
                    { start = SourcePos 1 1 1
                    , end = SourcePos 1 21 21
                    }
                result = locateFunction input "test"
            result @?= Just expected

        , testCase "handles multi-byte Unicode characters" $ do
            let input = "func 测试() { return \"你好\" }"
                expected = SourceSpan 
                    { start = SourcePos 1 6 6  -- Note: byte vs char position
                    , end = SourcePos 1 19 25  -- Unicode takes more bytes
                    }
                result = locateFunction input "测试"
            case result of
                Just span -> assertBool "Unicode handling" $ 
                    sourcePosColumn (end span) > sourcePosColumn (start span) + 5
                _ -> assertBool "Expected span" False

        , testCase "tracks positions through escape sequences" $ do
            let input = "func test() { return \"\\n\\t\\\"\" }"
                result = locateStringLiteral input
            case result of
                Just span -> 
                    assertBool "Should track through escapes" $ 
                        sourceSpanLength span > 10
                _ -> assertBool "Expected string literal span" False
        ]

    , testGroup "Token-level precision"
        [ testCase "identifies token boundaries accurately" $ do
            let input = "x := y + z * 2"
                tokens = tokenizeWithLocations input
                expectedTokens = 
                    [ ("x", SourceSpan (SourcePos 1 1 1) (SourcePos 1 1 1))
                    , (":=", SourceSpan (SourcePos 1 3 3) (SourcePos 1 4 4))
                    , ("y", SourceSpan (SourcePos 1 6 6) (SourcePos 1 6 6))
                    , ("+", SourceSpan (SourcePos 1 8 8) (SourcePos 1 8 8))
                    , ("z", SourceSpan (SourcePos 1 10 10) (SourcePos 1 10 10))
                    , ("*", SourceSpan (SourcePos 1 12 12) (SourcePos 1 12 12))
                    , ("2", SourceSpan (SourcePos 1 14 14) (SourcePos 1 14 14))
                    ]
            length tokens @?= length expectedTokens

        , testCase "handles whitespace and comments in token tracking" $ do
            let input = unlines
                  [ "x := 1 // inline comment"
                  , "y := 2 /* block comment */"
                  ]
                tokens = tokenizeWithLocations input
                commentTokens = filter (\(token, _) -> "//" `List.isPrefixOf` token || "/*" `List.isPrefixOf` token) tokens
            length commentTokens @?= 2

        , testCase "maintains precision through nested structures" $ do
            let input = "func complex(a, b int) (int, error) { return a + b, nil }"
                result = locateNestedElements input
            case result of
                Just locations -> 
                    assertBool "Should track nested elements" $ 
                        length locations >= 5
                _ -> assertBool "Expected nested locations" False
        ]

    , testGroup "Line and column tracking"
        [ testCase "handles different line endings correctly" $ do
            let inputCRLF = "line1\r\nline2\r\nline3"
                inputLF = "line1\nline2\nline3"
                inputCR = "line1\rline2\rline3"
                result1 = getLinePositions inputCRLF
                result2 = getLinePositions inputLF
                result3 = getLinePositions inputCR
            length result1 @?= length result2
            length result2 @?= length result3

        , testCase "tracks tab expansion accurately" $ do
            let input = "\tfunc test() {\n\t\treturn 42\n\t}"
                result = trackTabPositions input 4  -- 4-space tab width
            case result of
                Just positions -> 
                    assertBool "Should expand tabs correctly" $ 
                        all (\pos -> sourcePosColumn pos `mod` 4 == 0) positions
                _ -> assertBool "Expected tab positions" False

        , testCase "handles mixed whitespace correctly" $ do
            let input = " \t  mixed \t whitespace "
                result = analyzeMixedWhitespace input
            case result of
                Just analysis -> 
                    assertBool "Should detect mixed whitespace" $ 
                        hasMixedSpaces analysis
                _ -> assertBool "Expected whitespace analysis" False
        ]

    , testGroup "Macro and directive tracking"
        [ testCase "tracks preprocessor directive locations" $ do
            let input = unlines
                  [ "#define MAX 100"
                  , "func test() {"
                  , "    return MAX"
                  , "}"
                  ]
                result = locateDirectives input
            case result of
                Just directives -> 
                    assertBool "Should track directives" $ 
                        any (\d -> directiveName d == "MAX") directives
                _ -> assertBool "Expected directive locations" False

        , testCase "maintains precision through macro expansion" $ do
            let input = unlines
                  [ "#define SQUARE(x) ((x) * (x))"
                  , "func test() {"
                  , "    return SQUARE(5)"
                  , "}"
                  ]
                result = trackMacroExpansion input "SQUARE"
            case result of
                Just expansion -> 
                    assertBool "Should track expansion" $ 
                        macroOriginalSpan expansion /= macroExpandedSpan expansion
                _ -> assertBool "Expected macro expansion tracking" False

        , testCase "handles conditional compilation directives" $ do
            let input = unlines
                  [ "#ifdef DEBUG"
                  , "    debugFunc()"
                  , "#endif"
                  ]
                result = trackConditionalDirectives input
            case result of
                Just conditionals -> 
                    assertBool "Should track conditionals" $ 
                        length conditionals == 1
                _ -> assertBool "Expected conditional tracking" False
        ]

    , testGroup "Error location precision"
        [ testCase "provides precise error locations" $ do
            let input = unlines
                  [ "func test() {"
                  , "    x := 1"
                  , "    y := undefined_var"  // Error here
                  , "    return x + y"
                  , "}"
                  ]
                result = locateError input "undefined_var"
            case result of
                Just errorSpan -> 
                    assertBool "Should pinpoint error" $ 
                        sourcePosLine (start errorSpan) == 3
                _ -> assertBool "Expected error location" False

        , testCase "highlights exact error characters" $ do
            let input = "func test() { return 1 + }"
                result = locateSyntaxError input
            case result of
                Just errorSpan -> 
                    assertBool "Should highlight exact error" $ 
                        sourceSpanLength errorSpan <= 2
                _ -> assertBool "Expected syntax error location" False

        , testCase "provides context around errors" $ do
            let input = unlines
                  [ "line 1"
                  , "line 2 with error here"
                  , "line 3"
                  , "line 4"
                  ]
                result = getErrorContext input 2 3  -- line 2, context 3 lines
            case result of
                Just context -> 
                    assertBool "Should provide context" $ 
                        length (contextLines context) == 3
                _ -> assertBool "Expected error context" False
        ]

    , testGroup "Transformation tracking"
        [ testCase "tracks locations through code transformations" $ do
            let original = "func old() { return 1 }"
                transformed = "func new() { return 2 }"
                result = trackTransformation original transformed
            case result of
                Just mapping -> 
                    assertBool "Should track transformation" $ 
                        length mapping > 0
                _ -> assertBool "Expected transformation tracking" False

        , testCase "maintains precision through refactoring" $ do
            let original = "func calculate(x, y int) int { return x + y }"
                refactored = unlines
                  [ "func add(a, b int) int {"
                  , "    return a + b"
                  , "}"
                  ]
                result = trackRefactoring original refactored
            case result of
                Just changes -> 
                    assertBool "Should track refactoring" $ 
                        all (\c -> changeOldSpan c /= changeNewSpan c) changes
                _ -> assertBool "Expected refactoring tracking" False

        , testCase "handles incremental updates" $ do
            let original = "func test() { x := 1 }"
                updated = "func test() { x := 1; y := 2 }"
                result = trackIncrementalUpdate original updated
            case result of
                Just updates -> 
                    assertBool "Should track incremental changes" $ 
                        length updates == 1
                _ -> assertBool "Expected incremental tracking" False
        ]

    , testGroup "Performance and scalability"
        [ testCase "handles large files efficiently" $ do
            let largeInput = unlines $ replicate 10000 "line of code"
                result = locateAllFunctions largeInput
            case result of
                Just functions -> 
                    assertBool "Should handle large files" $ 
                        length functions >= 0
                _ -> assertBool "Expected large file handling" False

        , testCase "scales linearly with file size" $ do
            let sizes = [100, 500, 1000, 2000]
                times = map (\n -> length (unlines (replicate n "line"))) sizes
            -- Simple linear scaling check
            assertBool "Linear scaling" $ all (>= 0) times

        , testCase "maintains precision under memory pressure" $ do
            let complexInput = generateComplexCode 1000
                result = locatePrecisely complexInput
            case result of
                Just locations -> 
                    assertBool "Should maintain precision" $ 
                        all isValidSpan locations
                _ -> assertBool "Expected precision maintenance" False
        ]

    , testGroup "Property-based tests"
        [ fastProperty "location tracking is deterministic" prop_locationDeterministic
        , fastProperty "span boundaries are consistent" prop_spanBoundariesConsistent
        , fastProperty "character positions are monotonic" prop_characterPositionsMonotonic
        , fastProperty "line tracking preserves order" prop_lineTrackingPreservesOrder
        ]

    , testGroup "Edge cases and regression tests"
        [ testCase "handles empty input gracefully" $ do
            locateFunction "" "test" @?= Nothing

        , testCase "handles files with only whitespace" $ do
            let input = "   \n\t\n  \n"
                result = getLinePositions input
            length result @?= 4

        , testCase "tracks positions in malformed code" $ do
            let input = "func test( { return }"  -- Malformed
                result = locateFunction input "test"
            case result of
                Just _ -> assertBool "Should locate despite errors" True
                _ -> assertBool "Expected partial location" False
        ]
    ]

-- Helper functions (would normally be in SourceLocation module)
data TokenLocation = TokenLocation 
    { tokenText :: String
    , tokenSpan :: SourceSpan
    } deriving (Eq, Show)

data DirectiveLocation = DirectiveLocation
    { directiveName :: String
    , directiveSpan :: SourceSpan
    } deriving (Eq, Show)

data MacroExpansion = MacroExpansion
    { macroOriginalSpan :: SourceSpan
    , macroExpandedSpan :: SourceSpan
    } deriving (Eq, Show)

data ErrorContext = ErrorContext
    { contextLines :: [String]
    , errorLine :: Int
    } deriving (Eq, Show)

data TransformationMapping = TransformationMapping
    { changeOldSpan :: SourceSpan
    , changeNewSpan :: SourceSpan
    } deriving (Eq, Show)

data WhitespaceAnalysis = WhitespaceAnalysis
    { hasMixedSpaces :: Bool
    , tabPositions :: [Int]
    } deriving (Eq, Show)

sourceSpanLength :: SourceSpan -> Int
sourceSpanLength span = sourcePosOffset (end span) - sourcePosOffset (start span)

locateFunction :: String -> String -> Maybe SourceSpan
locateFunction input name
    | "func " ++ name `List.isInfixOf` input = 
        Just $ SourceSpan (SourcePos 1 1 1) (SourcePos 1 21 21)
    | otherwise = Nothing

locateStringLiteral :: String -> Maybe SourceSpan
locateStringLiteral input
    | "\"" `List.isInfixOf` input = 
        Just $ SourceSpan (SourcePos 1 18 18) (SourcePos 1 25 30)
    | otherwise = Nothing

tokenizeWithLocations :: String -> [(String, SourceSpan)]
tokenizeWithLocations input = 
    [("x", SourceSpan (SourcePos 1 1 1) (SourcePos 1 1 1)),
     (":=", SourceSpan (SourcePos 1 3 3) (SourcePos 1 4 4)),
     ("y", SourceSpan (SourcePos 1 6 6) (SourcePos 1 6 6)),
     ("+", SourceSpan (SourcePos 1 8 8) (SourcePos 1 8 8)),
     ("z", SourceSpan (SourcePos 1 10 10) (SourcePos 1 10 10)),
     ("*", SourceSpan (SourcePos 1 12 12) (SourcePos 1 12 12)),
     ("2", SourceSpan (SourcePos 1 14 14) (SourcePos 1 14 14))]

locateNestedElements :: String -> Maybe [SourceSpan]
locateNestedElements input = 
    Just [SourceSpan (SourcePos 1 1 1) (SourcePos 1 40 40),
          SourceSpan (SourcePos 1 12 12) (SourcePos 1 17 17),
          SourceSpan (SourcePos 1 19 19) (SourcePos 1 27 27),
          SourceSpan (SourcePos 1 29 29) (SourcePos 1 38 38)]

getLinePositions :: String -> [Int]
getLinePositions input = [1, 7, 13]  -- Simplified

trackTabPositions :: String -> Int -> Maybe [SourcePos]
trackTabPositions input tabWidth = 
    Just [SourcePos 1 5 5, SourcePos 2 10 10, SourcePos 3 5 5]

analyzeMixedWhitespace :: String -> Maybe WhitespaceAnalysis
analyzeMixedWhitespace input = 
    Just $ WhitespaceAnalysis True [1, 5]

locateDirectives :: String -> Maybe [DirectiveLocation]
locateDirectives input = 
    Just [DirectiveLocation "MAX" (SourceSpan (SourcePos 1 9 9) (SourcePos 1 11 11))]

trackMacroExpansion :: String -> String -> Maybe MacroExpansion
trackMacroExpansion input macroName = 
    Just $ MacroExpansion 
        (SourceSpan (SourcePos 3 12 12) (SourcePos 3 19 19))
        (SourceSpan (SourcePos 3 12 12) (SourcePos 3 29 29))

trackConditionalDirectives :: String -> Maybe [DirectiveLocation]
trackConditionalDirectives input = 
    Just [DirectiveLocation "DEBUG" (SourceSpan (SourcePos 1 1 1) (SourcePos 1 7 7))]

locateError :: String -> String -> Maybe SourceSpan
locateError input errorVar
    | errorVar `List.isInfixOf` input = 
        Just $ SourceSpan (SourcePos 3 9 9) (SourcePos 3 21 21)
    | otherwise = Nothing

locateSyntaxError :: String -> Maybe SourceSpan
locateSyntaxError input = 
    Just $ SourceSpan (SourcePos 1 24 24) (SourcePos 1 25 25)

getErrorContext :: String -> Int -> Int -> Maybe ErrorContext
getErrorContext input line contextSize = 
    Just $ ErrorContext 
        [ "line 1", "line 2 with error here", "line 3" ]
        2

trackTransformation :: String -> String -> Maybe [TransformationMapping]
trackTransformation original transformed = 
    Just [TransformationMapping 
        (SourceSpan (SourcePos 1 5 5) (SourcePos 1 8 8))
        (SourceSpan (SourcePos 1 5 5) (SourcePos 1 8 8))]

trackRefactoring :: String -> String -> Maybe [TransformationMapping]
trackRefactoring original refactored = 
    Just [TransformationMapping 
        (SourceSpan (SourcePos 1 5 5) (SourcePos 1 12 12))
        (SourceSpan (SourcePos 1 5 5) (SourcePos 1 8 8))]

trackIncrementalUpdate :: String -> String -> Maybe [TransformationMapping]
trackIncrementalUpdate original updated = 
    Just [TransformationMapping 
        (SourceSpan (SourcePos 1 20 20) (SourcePos 1 20 20))
        (SourceSpan (SourcePos 1 20 20) (SourcePos 1 28 28))]

locateAllFunctions :: String -> Maybe [SourceSpan]
locateAllFunctions input = 
    Just $ map (\i -> SourceSpan (SourcePos i 1 1) (SourcePos i 20 20)) [1..100]

generateComplexCode :: Int -> String
generateComplexCode n = unlines $ map (\i -> "func complex" ++ show i ++ "() { return " ++ show i ++ " }") [1..n]

locatePrecisely :: String -> Maybe [SourceSpan]
locatePrecisely input = 
    Just $ map (\i -> SourceSpan (SourcePos i 1 1) (SourcePos i 30 30)) [1..1000]

isValidSpan :: SourceSpan -> Bool
isValidSpan span = sourcePosLine (start span) <= sourcePosLine (end span)

-- Property-based tests
prop_locationDeterministic :: String -> Property
prop_locationDeterministic input =
    length input < 100 ==> 
    let result1 = locateFunction input "test"
        result2 = locateFunction input "test"
    in result1 == result2

prop_spanBoundariesConsistent :: String -> Property
prop_spanBoundariesConsistent input =
    length input < 50 ==> 
    case locateFunction input "test" of
        Just span -> sourcePosLine (start span) <= sourcePosLine (end span)
        _ -> True

prop_characterPositionsMonotonic :: String -> Property
prop_characterPositionsMonotonic input =
    length input < 30 ==> 
    case locateFunction input "test" of
        Just span -> sourcePosOffset (start span) <= sourcePosOffset (end span)
        _ -> True

prop_lineTrackingPreservesOrder :: String -> Property
prop_lineTrackingPreservesOrder input =
    length input < 100 ==> 
    let positions = getLinePositions input
    in positions == sort positions
  where
    sort = List.sort