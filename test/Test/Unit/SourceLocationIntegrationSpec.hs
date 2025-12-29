module Test.Unit.SourceLocationIntegrationSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, forAll, Gen, arbitrary, choose)
import SourceLocation
  ( SourcePos(..)
  , SourceSpan(..)
  , Located(..)
  , startPos
  , posAfter
  , advancePosBy
  , advancePosByText
  , emptySpan
  , spanFrom
  , spanBetween
  , mergeSpans
  , locatedAt
  , locatedWithSpan
  , locatedValue
  , locatedSpan
  , locatedPos
  , mapLocated
  , withLocationTracking
  , markSpanStart
  , markSpanEnd
  , isValidSpan
  , toErrorLocation
  , toErrorLocationWithSpan
  )
import qualified Data.Text as T

-- | Integration tests for SourceLocation module
tests :: TestTree
tests =
  testGroup "SourceLocation Integration"
    [ testGroup "Location tracking with real text"
        [ testCase "track position through complete source file" $ do
            let source = unlines
                  [ "package main"
                  , ""
                  , "func main() {"
                  , "\tfmt.Println(\"Hello, World!\")"
                  , "}"
                  ]
                positions = scanPositions source
            assertBool "Should track positions correctly" $
                length positions == length source + 1  -- +1 for final position
        
        , testCase "handle mixed tab/space indentation" $ do
            let source = "line1\n\tline2\n  \tline3\n\t\tline4"
                finalPos = advancePosBy source startPos
            finalPos @?= SourcePos 5 1 20
        
        , testCase "track position through unicode text" $ do
            let source = "héllo\nwörld\n🌟 star"
                finalPos = advancePosBy source startPos
            finalPos @?= SourcePos 3 8 18
        
        , testCase "handle empty lines and whitespace" $ do
            let source = "\n  \n\t\nline\n"
                finalPos = advancePosBy source startPos
            finalPos @?= SourcePos 5 1 8
        ]
    
    , testGroup "Span operations and merging"
        [ testCase "merge overlapping spans correctly" $ do
            let span1 = spanBetween (posAt 1 5) (posAt 1 15)
                span2 = spanBetween (posAt 1 10) (posAt 1 20)
                merged = mergeSpans span1 span2
            merged @?= spanBetween (posAt 1 5) (posAt 1 20)
        
        , testCase "merge non-overlapping spans correctly" $ do
            let span1 = spanBetween (posAt 1 1) (posAt 1 10)
                span2 = spanBetween (posAt 2 1) (posAt 2 10)
                merged = mergeSpans span1 span2
            merged @?= spanBetween (posAt 1 1) (posAt 2 10)
        
        , testCase "merge multiple spans" $ do
            let spans = [ spanBetween (posAt 1 5) (posAt 1 8)
                        , spanBetween (posAt 1 10) (posAt 1 15)
                        , spanBetween (posAt 1 12) (posAt 1 20)
                        ]
                merged = foldl mergeSpans (head spans) (tail spans)
            merged @?= spanBetween (posAt 1 5) (posAt 1 20)
        
        , testCase "validate span correctness" $ do
            let validSpan = spanBetween (posAt 1 1) (posAt 1 10)
                invalidSpan = spanBetween (posAt 1 10) (posAt 1 1)
            assertBool "Valid span should pass validation" (isValidSpan validSpan)
            assertBool "Invalid span should fail validation" (not (isValidSpan invalidSpan))
        ]
    
    , testGroup "Located values operations"
        [ testCase "create and manipulate located values" $ do
            let pos = posAt 3 7
                value = "variable"
                located = locatedAt pos value
            locatedValue located @?= value
            locatedPos located @?= pos
        
        , testCase "map functions over located values" $ do
            let pos = posAt 2 4
                value = 42
                located = locatedAt pos value
                doubled = mapLocated (*2) located
            locatedValue doubled @?= 84
            locatedPos doubled @?= pos
        
        , testCase "create located values with spans" $ do
            let span = spanBetween (posAt 1 1) (posAt 1 5)
                value = "hello"
                located = locatedWithSpan span value
            locatedSpan located @?= span
            locatedValue located @?= value
        ]
    
    , testGroup "Location tracking monad"
        [ testCase "track positions with location monad" $ do
            let text = "hello\nworld"
                (result, finalPos) = withLocationTracking startPos $ do
                    start <- markSpanStart
                    _ <- advancePosByText text
                    end <- markSpanEnd
                    return (spanBetween start end)
            result @?= spanBetween (posAt 1 1) (posAt 2 5)
            finalPos @?= posAt 2 5
        
        , testCase "handle complex text with location monad" $ do
            let text = unlines
                  [ "func test() {"
                  , "\treturn 42"
                  , "}"
                  ]
                (spans, finalPos) = withLocationTracking startPos $ do
                    start1 <- markSpanStart
                    _ <- advancePosByText "func test() {"
                    end1 <- markSpanEnd
                    _ <- advancePosByText "\n\treturn 42\n"
                    start2 <- markSpanStart
                    _ <- advancePosByText "}"
                    end2 <- markSpanEnd
                    return [spanBetween start1 end1, spanBetween start2 end2]
            length spans @?= 2
            finalPos @?= posAt 3 2
        ]
    
    , testGroup "Error location conversion"
        [ testCase "convert position to error location" $ do
            let pos = posAt 10 20
                errLoc = toErrorLocation pos
            line errLoc @?= 10
            column errLoc @?= 20
        
        , testCase "convert span to error location with range" $ do
            let span = spanBetween (posAt 5 10) (posAt 7 15)
                errLoc = toErrorLocationWithSpan span
            line errLoc @?= 5
            column errLoc @?= 10
            endLine errLoc @?= Just 7
            endColumn errLoc @?= Just 15
        
        , testCase "handle single-line span error location" $ do
            let span = spanBetween (posAt 3 5) (posAt 3 15)
                errLoc = toErrorLocationWithSpan span
            line errLoc @?= 3
            column errLoc @?= 5
            endLine errLoc @?= Just 3
            endColumn errLoc @?= Just 15
        ]
    
    , testGroup "QuickCheck integration properties"
        [ fastProperty "advancePosBy consistent with posAfter for single chars" $
            \c pos -> advancePosBy [c] pos == posAfter c pos
        
        , fastProperty "mergeSpans is commutative" $
            \span1 span2 -> mergeSpans span1 span2 == mergeSpans span2 span1
        
        , fastProperty "mergeSpans is associative" $
            \span1 span2 span3 -> 
                mergeSpans span1 (mergeSpans span2 span3) == 
                mergeSpans (mergeSpans span1 span2) span3
        
        , fastProperty "locatedValue . locatedAt == id" $
            \pos value -> locatedValue (locatedAt pos value) == value
        
        , fastProperty "mapLocated id == id" $
            \pos value -> mapLocated id (locatedAt pos value) == locatedAt pos value
        ]
    
    , testGroup "Real-world scenarios"
        [ testCase "track locations in function definition" $ do
            let funcDef = "func calculate(x int, y int) int {\n\treturn x + y\n}"
                spans = locateFunctionParts funcDef
            length spans @?= 3  -- name, params, body
        
        , testCase "handle multiline string literals" $ do
            let multiline = "let s = \"line1\\nline2\\tline3\"\nlet x = 42"
                finalPos = advancePosBy multiline startPos
            finalPos @?= SourcePos 2 9
        
        , testCase "track through complex indentation" $ do
            let complexCode = unlines
                  [ "if condition {"
                  , "\tif nested {"
                  , "\t\t// comment"
                  , "\t\tdoSomething()"
                  , "\t}"
                  , "}"
                  ]
                finalPos = advancePosBy complexCode startPos
            finalPos @?= SourcePos 6 1
        ]
    ]

-- Helper functions

-- Generate list of positions for each character in source
scanPositions :: String -> [SourcePos]
scanPositions source = scanl (flip posAfter) startPos source

-- Locate different parts of a function definition
locateFunctionParts :: String -> [SourceSpan]
locateFunctionParts funcDef = 
    let (name, rest1) = locateKeyword "func " funcDef
        (params, rest2) = locateParenthesized rest1
        (returnType, body) = locateKeyword "-> int " rest2
    in [name, params, returnType]
  where
    locateKeyword keyword text = 
        case span keyword `isInfixOf` text of
            True -> (emptySpan startPos, text)
            False -> (emptySpan startPos, text)
    
    locateParenthesized text = 
        case spanBetween startPos startPos `isInfixOf` text of
            True -> (emptySpan startPos, text)
            False -> (emptySpan startPos, text)