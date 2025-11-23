module Test.Unit.AnalyzerStateSpec (tests) where

import Analyzer.State
import Analyzer.Types
import qualified Dependencies as Dep
import qualified Ownership as Own
import Control.Monad.Except (runExceptT)
import Control.Monad.State (runStateT)
import qualified Data.Map.Strict as Map
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

initialState :: Bool -> Bool -> AnalyzerState
initialState = newIntegratedAnalyzer

execute :: AnalyzerState -> IntegratedAnalyzer a -> IO (a, AnalyzerState)
execute st action = do
    result <- runExceptT (runStateT action st)
    case result of
        Left err -> assertFailure ("Analyzer action failed: " ++ err)
        Right value -> pure value

mkSymbol :: String -> SymbolInfo
mkSymbol name = SymbolInfo
    { symbolName = name
    , symbolType = Nothing
    , ownershipState = Nothing
    , symbolScope = 0
    , isMoved = False
    , isBorrowed = False
    , constraints = []
    }

sampleErrors :: [CombinedError]
sampleErrors =
    [ IntegrationError "fatal error" Fatal
    , OwnershipErrorCombined Warning (Own.UseAfterMove "alpha")
    , DependentTypeErrorCombined Info (Dep.TypeNotFound "ValueType")
    , CrossAnalyzerError "follow up" Error []
    ]

summaryExpectation :: String
summaryExpectation = unlines
    [ "Analysis Summary:"
    , "================="
    , "Errors: 2"
    , "Warnings: 1"
    , "Info: 1"
    , "Total symbols: 2"
    ]

tests :: TestTree
tests =
    testGroup "Analyzer.State"
        [ testCase "newIntegratedAnalyzer preserves flags and defaults" $ do
            let state = initialState True False
            enableOwnership (analysisContext state) @?= True
            enableDependentTypes (analysisContext state) @?= False
            analysisPhase (analysisContext state) @?= InitialPhase
            assertBool "symbol table should begin empty" (Map.null (symbolTable state))

        , testCase "setPhase updates the analysis context" $ do
            (_, updated) <- execute (initialState True True) (setPhase OwnershipPhase)
            analysisPhase (analysisContext updated) @?= OwnershipPhase

        , testCase "ifEnableOwnership gates analyzer actions" $ do
            (resultDisabled, _) <- execute (initialState False True) (ifEnableOwnership "skip" (pure "executed"))
            resultDisabled @?= "skip"
            (resultEnabled, _) <- execute (initialState True True) (ifEnableOwnership "skip" (pure "executed"))
            resultEnabled @?= "executed"

        , testCase "ifEnableDependentTypes respects the feature flag" $ do
            (resultDisabled, _) <- execute (initialState True False) (ifEnableDependentTypes "fallback" (pure "ran"))
            resultDisabled @?= "fallback"
            (resultEnabled, _) <- execute (initialState True True) (ifEnableDependentTypes "fallback" (pure "ran"))
            resultEnabled @?= "ran"

        , testCase "adding ownership and dependent type errors accumulates combined errors" $ do
            let ownershipErr = Own.UseAfterMove "resource"
                dependentErr = Dep.ParseError "bad refinement"
            (_, updated) <- execute (initialState True True) $ do
                addOwnershipError Warning ownershipErr
                addDependentTypeError Info dependentErr
            ownershipErrorsAcc updated @?= [(Warning, ownershipErr)]
            dependentTypeErrorsAcc updated @?= [(Info, dependentErr)]
            combinedErrorsAcc updated @?=
                [ OwnershipErrorCombined Warning ownershipErr
                , DependentTypeErrorCombined Info dependentErr
                ]

        , testCase "filterWarnings and filterInfo traverse nested combined errors" $ do
            let nestedErrors =
                    [ IntegrationError "top warning" Warning
                    , CrossAnalyzerError "info cluster" Info
                        [ IntegrationError "nested warning" Warning
                        , OwnershipErrorCombined Info (Own.ParseError "bad embed")
                        ]
                    ]
            filterWarnings nestedErrors @?= ["top warning", "nested warning"]
            filterInfo nestedErrors @?= ["info cluster", "ParseError \"bad embed\""]

        , testCase "getCombinedErrors mirrors analyzer state and summary counts severities" $ do
            let state = (initialState True True)
                    { combinedErrorsAcc = sampleErrors
                    , symbolTable = Map.fromList [("alpha", mkSymbol "alpha"), ("beta", mkSymbol "beta")]
                    }
            getCombinedErrors state @?= sampleErrors
            getAnalysisSummary state @?= summaryExpectation
        ]
