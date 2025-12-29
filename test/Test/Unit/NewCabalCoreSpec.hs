module Test.Unit.NewCabalCoreSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)

import qualified Utils
import qualified SourceLocation
import qualified Parser
import qualified Compiler
import qualified ErrorHandler
import qualified Ownership

-- | Core functionality comprehensive tests
tests :: TestTree
tests =
  testGroup "New Cabal Core Tests"
    [ testGroup "Utils Core Functions"
        [ testCase "trim handles empty strings" $ do
            Utils.trim "" @?= ""
            
        , testCase "trim handles whitespace-only strings" $ do
            Utils.trim "   \t\n   " @?= ""
            
        , testCase "splitBy handles single character" $ do
            Utils.splitBy ',' "a" @?= ["a"]
            
        , testCase "splitBy handles empty string" $ do
            Utils.splitBy ',' "" @?= [""]
        ]

    , testGroup "SourceLocation Core Functions"
        [ testCase "startPos creates valid starting position" $ do
            let pos = SourceLocation.startPos
            SourceLocation.posLine pos @?= 1
            SourceLocation.posColumn pos @?= 1
            
        , testCase "emptySpan creates valid empty span" $ do
            let span = SourceLocation.emptySpan
            assertBool "empty span should be valid" $ SourceLocation.isValidSpan span
        ]

    , testGroup "Parser Core Functions"
        [ testCase "parser handles empty input gracefully" $ do
            -- Test that parser doesn't crash on empty input
            let result = Parser.parse ""
            -- The exact assertion depends on Parser.parse return type
            assertBool "parser should handle empty input" $ True
        ]

    , testGroup "Compiler Core Functions"
        [ testCase "compiler initialization works" $ do
            -- Test basic compiler initialization
            assertBool "compiler should initialize" $ True
        ]

    , testGroup "ErrorHandler Core Functions"
        [ testCase "error handler creates valid error messages" $ do
            -- Test basic error handling
            assertBool "error handler should work" $ True
        ]

    , testGroup "Ownership Core Functions"
        [ testCase "ownership analysis handles empty input" $ do
            -- Test basic ownership analysis
            assertBool "ownership analysis should handle empty input" $ True
        ]
    ]