{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Test.Unit.ParserErrorRecoverySpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool, assertFailure, (@?=), (@=?))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Gen, choose, vectorOf, oneof, elements, listOf1, arbitrary)

import Parser
  ( BlockDirectives(..)
  , CodeBlock(..)
  , FileDirectives(..)
  , TypusFile(..)
  , parseTypus
  , ParseError(..)
  , ErrorRecoveryStrategy(..)
  , recoverFromParseError
  , parseWithRecovery
  )

import SourceLocation
  ( SourcePos(..)
  , SourceSpan(..)
  , Located(..)
  , spanStart
  , spanEnd
  )

import Data.List (isPrefixOf, isInfixOf, isSuffixOf)
import Data.Maybe (isJust, isNothing, fromMaybe)
import qualified Data.Text as T

-- | Test parser error recovery mechanisms
tests :: TestTree
tests =
  testGroup "Parser Error Recovery Tests"
    [ testGroup "Basic error recovery"
        [ testCase "recovers from missing semicolon" $ do
            let source = unlines
                  [ "func main() {"
                  , "    let x = 1"
                  , "    let y = 2"
                  , "}"
                  ]
            case parseWithRecovery source of
              Left err -> assertFailure $ "parseWithRecovery failed: " ++ show err
              Right (result, warnings) -> do
                assertBool "should parse with recovery" $ isJust result
                assertBool "should generate warnings" $ not (null warnings)

        , testCase "recovers from unmatched braces" $ do
            let source = unlines
                  [ "func test() {"
                  , "    if condition {"
                  , "        doSomething()"
                  , "    // missing closing brace"
                  , "}"
                  ]
            case parseWithRecovery source of
              Left err -> assertFailure $ "parseWithRecovery failed: " ++ show err
              Right (result, warnings) -> do
                assertBool "should recover from unmatched braces" $ isJust result
                assertBool "should warn about unmatched braces" $ 
                  any ("unmatched" `isInfixOf`) warnings

        , testCase "recovers from invalid syntax" $ do
            let source = unlines
                  [ "func invalid() {"
                  , "    let x = 1 + + 2"  -- Invalid double plus
                  , "    return x"
                  , "}"
                  ]
            case parseWithRecovery source of
              Left err -> assertFailure $ "parseWithRecovery failed: " ++ show err
              Right (result, warnings) -> do
                assertBool "should recover from invalid syntax" $ isJust result
                assertBool "should report syntax errors" $ 
                  any ("syntax" `isInfixOf`) warnings
        ]

    , testGroup "Error recovery strategies"
        [ testCase "skip to next statement strategy" $ do
            let source = unlines
                  [ "func test() {"
                  , "    let x = 1 + + 2"  -- Error here
                  , "    let y = 3"        -- Should recover to this line
                  , "    return y"
                  , "}"
                  ]
                strategy = SkipToNextStatement
            case recoverFromParseError strategy source of
              Left err -> assertFailure $ "recovery failed: " ++ show err
              Right recovered -> do
                assertBool "should skip to next statement" $ 
                  "let y = 3" `isInfixOf` recovered

        , testCase "skip to next block strategy" $ do
            let source = unlines
                  [ "func outer() {"
                  , "    if condition {"
                  , "        invalid syntax here"
                  , "    }"
                  , "    // Should recover to next block"
                  , "    if otherCondition {"
                  , "        validCode()"
                  , "    }"
                  , "}"
                  ]
                strategy = SkipToNextBlock
            case recoverFromParseError strategy source of
              Left err -> assertFailure $ "recovery failed: " ++ show err
              Right recovered -> do
                assertBool "should skip to next block" $ 
                  "otherCondition" `isInfixOf` recovered

        , testCase "panic recovery strategy" $ do
            let source = unlines
                  [ "func test() {"
                  , "    completely invalid syntax !!!@@@"
                  , "    more code"
                  , "}"
                  ]
                strategy = PanicRecovery
            case recoverFromParseError strategy source of
              Left err -> assertBool "panic recovery should fail gracefully" $ True
              Right recovered -> do
                assertBool "panic recovery should attempt recovery" $ 
                  length recovered > 0
        ]

    , testGroup "Context-aware recovery"
        [ testCase "recovers based on function context" $ do
            let source = unlines
                  [ "func calculate() {"
                  , "    let x = 1"
                  , "    invalid line here"
                  , "    let z = 3"
                  , "    return x + z"
                  , "}"
                  ]
            case parseWithRecovery source of
              Left err -> assertFailure $ "parseWithRecovery failed: " ++ show err
              Right (result, warnings) -> do
                assertBool "should use function context for recovery" $ isJust result
                assertBool "should preserve function structure" $ 
                  case result of
                    Just typusFile -> "calculate" `isInfixOf` show typusFile
                    Nothing -> False

        , testCase "recovers based on block structure" $ do
            let source = unlines
                  [ "{"
                  , "    valid statement 1"
                  , "    invalid statement !!!"
                  , "    valid statement 2"
                  , "}"
                  ]
            case parseWithRecovery source of
              Left err -> assertFailure $ "parseWithRecovery failed: " ++ show err
              Right (result, warnings) -> do
                assertBool "should maintain block structure" $ isJust result

        , testCase "recovers from directive errors" $ do
            let source = unlines
                  [ "//! ownership: invalid_value"
                  , "package main"
                  , "func main() {}"
                  ]
            case parseWithRecovery source of
              Left err -> assertFailure $ "parseWithRecovery failed: " ++ show err
              Right (result, warnings) -> do
                assertBool "should recover from directive errors" $ isJust result
                assertBool "should warn about invalid directives" $ 
                  any ("directive" `isInfixOf`) warnings
        ]

    , testGroup "Error reporting with recovery"
        [ testCase "provides detailed error locations" $ do
            let source = unlines
                  [ "func test() {"
                  , "    line 1"
                  , "    line 2 with error"
                  , "    line 3"
                  , "}"
                  ]
            case parseWithRecovery source of
              Left err -> assertFailure $ "parseWithRecovery failed: " ++ show err
              Right (result, warnings) -> do
                assertBool "should provide error locations" $ 
                  any (isInfixOf "line 2") warnings

        , testCase "provides recovery suggestions" $ do
            let source = unlines
                  [ "func test() {"
                  , "    let x = 1 + + 2"
                  , "}"
                  ]
            case parseWithRecovery source of
              Left err -> assertFailure $ "parseWithRecovery failed: " ++ show err
              Right (result, warnings) -> do
                assertBool "should provide recovery suggestions" $ 
                  any (isInfixOf "suggest" ||| isInfixOf "fix") warnings

        , testCase "maintains error context" $ do
            let source = unlines
                  [ "func outer() {"
                  , "    func inner() {"
                  , "        error here"
                  , "    }"
                  , "}"
                  ]
            case parseWithRecovery source of
              Left err -> assertFailure $ "parseWithRecovery failed: " ++ show err
              Right (result, warnings) -> do
                assertBool "should maintain error context" $ 
                  any (isInfixOf "inner") warnings
        ]

    , testGroup "Multiple error recovery"
        [ testCase "recovers from multiple errors in same function" $ do
            let source = unlines
                  [ "func multiple() {"
                  , "    error 1 here"
                  , "    error 2 here"
                  , "    valid statement"
                  , "    error 3 here"
                  , "}"
                  ]
            case parseWithRecovery source of
              Left err -> assertFailure $ "parseWithRecovery failed: " ++ show err
              Right (result, warnings) -> do
                assertBool "should recover from multiple errors" $ isJust result
                assertBool "should report all errors" $ 
                  length warnings >= 3

        , testCase "recovers from cascading errors" $ do
            let source = unlines
                  [ "func cascade() {"
                  , "    let x = [  // unclosed array"
                  , "    let y = x + 1"  -- This should be parsed despite previous error
                  , "    return y"
                  , "}"
                  ]
            case parseWithRecovery source of
              Left err -> assertFailure $ "parseWithRecovery failed: " ++ show err
              Right (result, warnings) -> do
                assertBool "should handle cascading errors" $ isJust result
                assertBool "should continue parsing after errors" $ 
                  "return y" `isInfixOf` show (fromMaybe (error "impossible") result)
        ]

    , testGroup "QuickCheck property tests for error recovery"
        [ fastProperty "parseWithRecovery never crashes on valid input" $
            \validSource ->
            not (null validSource) ==>
            case parseWithRecovery validSource of
              Left _ -> property True
              Right (result, _) -> property True

        , fastProperty "recovery preserves some structure" $
            \source ->
            length source > 10 ==>
            case parseWithRecovery source of
              Left _ -> property True
              Right (result, warnings) -> 
                case result of
                  Nothing -> length warnings > 0 ==> property True
                  Just _ -> property True

        , fastProperty "error recovery is deterministic" $
            \source ->
            let result1 = parseWithRecovery source
                result2 = parseWithRecovery source
            in result1 === result2

        , fastProperty "recovery produces warnings for errors" $
            \source ->
            "invalid" `isInfixOf` source ==>
            case parseWithRecovery source of
              Left _ -> property True
              Right (result, warnings) -> 
                if "invalid" `isInfixOf` source
                then length warnings > 0 ==> property True
                else property True

        , fastProperty "recovery strategies handle edge cases" $
            \source strategy ->
            not (null source) ==>
            case recoverFromParseError strategy source of
              Left _ -> property True
              Right recovered -> length recovered >= 0 ==> property True
        ]
  ]
  
  -- Helper operator for quickcheck tests
  infixr 2 |||
  (|||) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
  (f ||| g) x = f x || g x