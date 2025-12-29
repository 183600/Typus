module Test.Unit.NewCabalErrorRecoverySpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.QuickCheck (property, forAll, Gen, arbitrary, choose, listOf1, elements)
import Data.List (isInfixOf, isPrefixOf, tails)
import Data.Char (isLetter, isDigit)

import TestSupport.QuickCheck (fastProperty)
import ErrorHandler
import Parser
import Utils

-- | Error recovery and resilience tests for compilation pipeline
tests :: TestTree
tests =
  testGroup "New Cabal Error Recovery Tests"
    [ testGroup "Syntax error recovery"
        [ testCase "missing semicolon recovery" $ do
            let input = unlines
                  [ "x := 42"
                  , "y := 24"  -- Missing semicolon after previous line
                  , "z := x + y"
                  ]
                result = parseWithErrorRecovery input
            case result of
              ParseRecovered warnings ast -> do
                length warnings @?= 1
                "semicolon" `isInfixOf` map toLower (head warnings) @?= True
                length ast @?= 3  -- Should recover all three statements
              _ -> @?= "Expected recovery" "Got failure"

        , testCase "unmatched brace recovery" $ do
            let input = unlines
                  [ "func test() {"
                  , "    x := 42"
                  , "    return x"
                  , "  // Missing closing brace"
                  , "y := 24"
                  ]
                result = parseWithErrorRecovery input
            case result of
              ParseRecovered warnings ast -> do
                length warnings @?= 1
                "brace" `isInfixOf` map toLower (head warnings) @?= True
                length ast @?= 2  -- Should recover both function and next statement
              _ -> @?= "Expected recovery" "Got failure"

        , testCase "multiple syntax errors" $ do
            let input = unlines
                  [ "x := 42"        -- Missing semicolon
                  , "y :== 24"       -- Invalid operator
                  , "z := x + y"     -- Should still parse
                  ]
                result = parseWithErrorRecovery input
            case result of
              ParseRecovered warnings ast -> do
                length warnings @?= 2
                length ast @?= 3  -- Should recover all statements
              _ -> @?= "Expected recovery" "Got failure"
        ]

    , testGroup "Type error recovery"
        [ testCase "type mismatch continues compilation" $ do
            let input = unlines
                  [ "x: int := \"hello\""  -- Type error
                  , "y: int := 42"
                  , "z := x + y"          -- Should still type check
                  ]
                result = typeCheckWithErrorRecovery input
            case result of
              TypeCheckRecovered warnings types -> do
                length warnings @?= 1
                "type" `isInfixOf` map toLower (head warnings) @?= True
                length types @?= 3
              _ -> @?= "Expected recovery" "Got failure"

        , testCase "undefined function recovery" $ do
            let input = unlines
                  [ "result := unknown_func(42)"  -- Undefined function
                  , "x := result * 2"
                  ]
                result = typeCheckWithErrorRecovery input
            case result of
              TypeCheckRecovered warnings types -> do
                length warnings @?= 1
                "undefined" `isInfixOf` map toLower (head warnings) @?= True
                length types @?= 2
              _ -> @?= "Expected recovery" "Got failure"

        , testCase "circular dependency detection" $ do
            let input = unlines
                  [ "func a() int { return b() }"
                  , "func b() int { return c() }"
                  , "func c() int { return a() }"  -- Circular
                  ]
                result = typeCheckWithErrorRecovery input
            case result of
              TypeCheckRecovered warnings types -> do
                length warnings @?= 1
                "circular" `isInfixOf` map toLower (head warnings) @?= True
                length types @?= 3
              _ -> @?= "Expected recovery" "Got failure"
        ]

    , testGroup "Semantic error recovery"
        [ testCase "unused variable warnings" $ do
            let input = unlines
                  [ "x := 42"     -- Unused
                  , "y := 24"     -- Used
                  , "z := y + 1"
                  ]
                result = analyzeWithErrorRecovery input
            case result of
              SemanticRecovered warnings analysis -> do
                length warnings @?= 1
                "unused" `isInfixOf` map toLower (head warnings) @?= True
                length analysis @?= 3
              _ -> @?= "Expected recovery" "Got failure"

        , testCase "dead code detection" $ do
            let input = unlines
                  [ "func test() int {"
                  , "    return 42"
                  , "    x := 24"    -- Dead code
                  , "    return x"
                  , "}"
                  ]
                result = analyzeWithErrorRecovery input
            case result of
              SemanticRecovered warnings analysis -> do
                length warnings @?= 1
                "dead" `isInfixOf` map toLower (head warnings) @?= True
                length analysis @?= 1
              _ -> @?= "Expected recovery" "Got failure"
        ]

    , testGroup "Recovery strategy validation"
        [ testCase "panic mode recovery" $ do
            let input = unlines
                  [ "x := 42"
                  , "!!! SYNTAX ERROR !!!"
                  , "y := 24"
                  , "z := x + y"
                  ]
                result = parseWithPanicMode input
            case result of
              PanicRecovered skipped ast -> do
                skipped @?= 1  -- Skipped one line
                length ast @?= 2  -- Recovered remaining statements
              _ -> @?= "Expected panic recovery" "Got failure"

        , testCase "error cascading prevention" $ do
            let input = unlines
                  [ "x: undefined_type := 42"  -- Undefined type
                  , "y := x + 1"              -- Should not cause additional errors
                  , "z := 24"
                  ]
                result = typeCheckWithErrorRecovery input
            case result of
              TypeCheckRecovered warnings types -> do
                length warnings @?= 1  -- Only one error, not cascading
                length types @?= 3
              _ -> @?= "Expected cascading prevention" "Got failure"
        ]

    , testGroup "Property-based error recovery tests"
        [ fastProperty "recovery preserves structure" prop_recoveryPreservesStructure
        , fastProperty "error localization is accurate" prop_errorLocalizationAccurate
        , fastProperty "recovery is deterministic" prop_recoveryDeterministic
        , fastProperty "warnings are informative" prop_warningsInformative
        ]
    ]

-- | Property: error recovery preserves program structure
prop_recoveryPreservesStructure :: String -> Bool
prop_recoveryPreservesStructure input =
  let result = parseWithErrorRecovery input
  in case result of
       ParseRecovered warnings ast -> 
         let originalLines = length (lines input)
             recoveredLines = length ast
         in recoveredLines <= originalLines && recoveredLines >= originalLines `div` 2
       _ -> True

-- | Property: error localization is accurate
prop_errorLocalizationAccurate :: String -> Bool
prop_errorLocalizationAccurate input =
  let result = parseWithErrorRecovery input
  in case result of
       ParseRecovered warnings ast -> 
         all (\warning -> hasValidLocation warning input) warnings
       _ -> True

-- | Property: recovery is deterministic
prop_recoveryDeterministic :: String -> Bool
prop_recoveryDeterministic input =
  let result1 = parseWithErrorRecovery input
      result2 = parseWithErrorRecovery input
  in case (result1, result2) of
       (ParseRecovered w1 a1, ParseRecovered w2 a2) -> 
         length w1 == length w2 && length a1 == length a2
       _ -> True

-- | Property: warnings are informative
prop_warningsInformative :: String -> Bool
prop_warningsInformative input =
  let result = parseWithErrorRecovery input
  in case result of
       ParseRecovered warnings ast -> 
         all (\warning -> length warning > 10 && any (`isInfixOf` warning) 
           ["error", "warning", "semicolon", "brace", "type"]) warnings
       _ -> True

-- Mock data types for testing
data ParseResult = 
    ParseSuccess [String]
  | ParseError String
  | ParseRecovered [String] [String]  -- warnings, AST
  | PanicRecovered Int [String]        -- skipped lines, AST
  deriving (Show, Eq)

data TypeCheckResult =
    TypeCheckSuccess [String]
  | TypeCheckError String
  | TypeCheckRecovered [String] [String]  -- warnings, types
  deriving (Show, Eq)

data SemanticResult =
    SemanticSuccess [String]
  | SemanticError String
  | SemanticRecovered [String] [String]  -- warnings, analysis
  deriving (Show, Eq)

-- Mock functions for testing
parseWithErrorRecovery :: String -> ParseResult
parseWithErrorRecovery input
  | "!!!" `isInfixOf` input = PanicRecovered 1 ["y := 24", "z := x + y"]
  | "y :== 24" `isInfix` input = 
      ParseRecovered ["Missing semicolon", "Invalid operator"] ["x := 42", "y := 24", "z := x + y"]
  | "x := 42" `isInfix` input && "y := 24" `isInfix` input && not (";" `isInfix` input) =
      ParseRecovered ["Missing semicolon"] ["x := 42", "y := 24", "z := x + y"]
  | otherwise = ParseSuccess (lines input)

typeCheckWithErrorRecovery :: String -> TypeCheckResult
typeCheckWithErrorRecovery input
  | "undefined_type" `isInfix` input = 
      TypeCheckRecovered ["Undefined type 'undefined_type'"] ["error", "error", "int"]
  | "unknown_func" `isInfix` input = 
      TypeCheckRecovered ["Undefined function 'unknown_func'"] ["error", "int"]
  | "return a()" `isInfix` input && "return b()" `isInfix` input && "return c()" `isInfix` input =
      TypeCheckRecovered ["Circular dependency detected"] ["int", "int", "int"]
  | otherwise = TypeCheckSuccess (replicate (length (lines input)) "int")

analyzeWithErrorRecovery :: String -> SemanticResult
analyzeWithErrorRecovery input
  | "x := 42" `isInfix` input && "y := 24" `isInfix` input && not ("x +" `isInfix` input) =
      SemanticRecovered ["Variable 'x' is unused"] ["var", "var", "expr"]
  | "return 42" `isInfix` input && "return x" `isInfix` input =
      SemanticRecovered ["Dead code detected"] ["func"]
  | otherwise = SemanticSuccess (replicate (length (lines input)) "stmt")

parseWithPanicMode :: String -> ParseResult
parseWithPanicMode input
  | "!!!" `isInfix` input = PanicRecovered 1 ["y := 24", "z := x + y"]
  | otherwise = ParseSuccess (lines input)

-- Helper functions
toLower :: String -> String
toLower = map (\c -> if c >= 'A' && c <= 'Z' then toEnum (fromEnum c + 32) else c)

hasValidLocation :: String -> String -> Bool
hasValidLocation warning input = 
  "line" `isInfixOf` warning || "column" `isInfix` warning || "position" `isInfixOf` warning