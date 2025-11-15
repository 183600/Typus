module Test.Unit.ErrorHandlingSpec (tests) where

import Control.Monad.Except (throwError)
import Control.Monad.State (runStateT)
import Data.List (isInfixOf)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ( (@?=), assertBool, testCase )

import Compiler.Errors
  ( CompilerError(..)
  , ErrorStatistics(..)
  , analyzeErrors
  , collectErrors
  , continueWith
  , formatCompilerError
  , formatCompilerErrors
  , generateDetailedReport
  , makeUserFriendly
  , ownershipError
  , recoverFrom
  , runCompilerM
  , suggestFix
  , syntaxError
  , typeError
  , withRecovery
  , withSourceLocation
  )
import Compiler.Errors.Core
  ( ErrorCategory(..)
  , ErrorContext(..)
  , ErrorLocation(..)
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
        let srcSpan = spanBetween (posAt 5 13) (posAt 5 20)
            sourceSnippet = "var x int = \"hello\""
            hints = map T.pack ["Use strconv.Atoi", "Change the variable type"]
            err = typeError "E002" (T.pack "Type mismatch") srcSpan (Just sourceSnippet) hints
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

    , testCase "formatCompilerErrors groups output by phase" $ do
        let parseErr = syntaxError "E020" (T.pack "Parse grouping error") (posAt 7 3)
            typeSpan = spanBetween (posAt 9 1) (posAt 9 5)
            checkingErr = typeError "E021" (T.pack "Type checking grouping") typeSpan Nothing []
            formatted = formatCompilerErrors [parseErr, checkingErr]
        assertBool "expected parsing phase header" ("ParsingPhase (1 errors)" `isInfixOf` formatted)
        assertBool "expected type checking phase header" ("TypeCheckingPhase (1 errors)" `isInfixOf` formatted)

    , testCase "generateDetailedReport highlights fatal recommendations" $ do
        let parseErr = syntaxError "E022" (T.pack "Parse failure") (posAt 11 2)
            typeSpan = spanBetween (posAt 12 1) (posAt 12 4)
            fatalBase = typeError "E023" (T.pack "Critical mismatch") typeSpan Nothing []
            fatalErr = fatalBase { ceError = (ceError fatalBase) { severity = Fatal } }
            report = generateDetailedReport [parseErr, fatalErr]
        assertBool "includes summary header" ("=== Error Summary ===" `isInfixOf` report)
        assertBool "counts total errors" ("Total Errors: 2" `isInfixOf` report)
        assertBool "suggests resolving fatal errors" ("Fix fatal errors first" `isInfixOf` report)

    , testCase "makeUserFriendly simplifies error presentation" $ do
        let srcSpan = spanBetween (posAt 14 1) (posAt 14 6)
            rawErr = typeError "E024" (T.pack "Type mismatch in assignment") srcSpan Nothing [T.pack "Review variable types"]
            friendly = makeUserFriendly rawErr
            TypeError { message = friendlyMsg, suggestions = friendlySuggestions } = ceError friendly
        friendlyMsg @?= T.pack "Type error: The types don't match. Make sure you're using the right type of value."
        friendlySuggestions @?= [T.pack "💡 Review variable types"]

    , testCase "suggestFix provides guidance for type errors" $ do
        let srcSpan = spanBetween (posAt 16 1) (posAt 16 3)
            err = typeError "E025" (T.pack "Another mismatch") srcSpan Nothing []
            hints = suggestFix err
        assertBool "should mention checking variable types" (T.pack "Check the types of your variables" `elem` hints)

    , testCase "collectErrors captures recovered compiler errors" $ do
        let err = syntaxError "E026" (T.pack "Intermediate failure") (posAt 18 3)
        collectErrors (recoverFrom err) @?= Left [err]

    , testCase "continueWith records error state while returning fallback" $ do
        let err = syntaxError "E027" (T.pack "Continue with fallback") (posAt 20 5)
        runStateT (continueWith "fallback" err) [] @?= Right ("fallback", [err])

    , testCase "withRecovery falls back when action throws" $ do
        let err = syntaxError "E028" (T.pack "Recoverable failure") (posAt 22 7)
        runCompilerM (withRecovery (throwError [err]) (99 :: Int)) @?= Right 99

    , testCase "withSourceLocation updates span boundaries" $ do
        let err = syntaxError "E029" (T.pack "Needs new span") (posAt 24 2)
            newSpan = spanBetween (posAt 30 4) (posAt 31 9)
            updated = withSourceLocation err newSpan
            loc = location (ceError updated)
        line loc @?= 30
        endLine loc @?= Just 31
        endColumn loc @?= Just 9
    ]
