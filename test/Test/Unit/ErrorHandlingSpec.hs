module Test.Unit.ErrorHandlingSpec (tests) where

import Data.List (isInfixOf)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ( (@?=), assertBool, testCase )

import Compiler.Errors
  ( CompilerError(..)
  , ErrorStatistics(..)
  , analyzeErrors
  , formatCompilerError
  , ownershipError
  , syntaxError
  , typeError
  )
import Compiler.Errors.Core
  ( ErrorCategory(..)
  , ErrorContext(..)
  , ErrorRecovery(..)
  , ErrorSeverity(..)
  , TypeError(..)
  , canRecoverFrom
  , fatalError
  , shouldContinueAfter
  )
import SourceLocation
  ( posAt
  , spanBetween
  , toErrorLocation
  )

-- | Tests that ensure the enhanced error handling utilities remain wired into
-- the Tasty suite. These cases cover the most valuable checks from the legacy
-- Hspec suite.
tests :: TestTree
tests =
  testGroup "Error handling"
    [ testCase "syntaxError captures parsing metadata" $ do
        let err = syntaxError "E001" (T.pack "Missing semicolon") (posAt 10 5)
            TypeError { severity = sev, category = cat } = ceError err
        sev @?= Error
        cat @?= Parsing
        assertBool "syntax errors should remain recoverable" (canRecoverFrom (ceError err))
        assertBool "phase should mention parsing" ("Parsing" `isInfixOf` show (cePhase err))

    , testCase "typeError preserves suggestions and context" $ do
        let typeSpan = spanBetween (posAt 5 13) (posAt 5 20)
            sourceSnippet = "var x int = \"hello\""
            hints = map T.pack ["Use strconv.Atoi", "Change the variable type"]
            err = typeError "E002" (T.pack "Type mismatch") typeSpan (Just sourceSnippet) hints
            TypeError { suggestions = recordedHints, context = ctx } = ceError err
        recordedHints @?= hints
        contextCode ctx @?= Just sourceSnippet
        ceSourceContext err @?= Just sourceSnippet

    , testCase "ownershipError provides recovery guidance" $ do
        let ownershipSpan = spanBetween (posAt 12 3) (posAt 12 15)
            err = ownershipError "E003" (T.pack "Value moved") ownershipSpan "let y = x" []
            recoveryInfo = recovery (ceError err)
        canRecover recoveryInfo @?= True
        shouldContinue recoveryInfo @?= True
        recoveryAction recoveryInfo @?= Just "Try using references or cloning"

    , testCase "formatCompilerError includes source location" $ do
        let err = syntaxError "E004" (T.pack "Unexpected token") (posAt 3 8)
            formatted = formatCompilerError err
        assertBool "expected line and column" ("3:8" `isInfixOf` formatted)

    , testCase "analyzeErrors tallies categories" $ do
        let parseErr = syntaxError "E010" (T.pack "Parse error") (posAt 1 1)
            typeErr' = typeError "E011" (T.pack "Type error") (spanBetween (posAt 2 1) (posAt 2 10)) Nothing []
            stats = analyzeErrors [parseErr, typeErr']
        esTotal stats @?= 2
        esErrors stats @?= 2
        esFatal stats @?= 0
        esRecoverable stats @?= 2
        Map.lookup Parsing (esByCategory stats) @?= Just 1
        Map.lookup TypeChecking (esByCategory stats) @?= Just 1

    , testCase "fatal errors are non-recoverable" $ do
        let fatal = fatalError "E999" (T.pack "Fatal") (toErrorLocation (posAt 0 0))
        canRecoverFrom fatal @?= False
        shouldContinueAfter fatal @?= False
    ]
