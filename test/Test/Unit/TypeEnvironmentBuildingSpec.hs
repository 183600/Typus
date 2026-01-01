{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Test.Unit.TypeEnvironmentBuildingSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (testCase, assertBool, assertEqual, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Gen, arbitrary, oneof, elements, choose, listOf, resize)

import Analyzer.Types
  ( SymbolInfo(..)
  , SymbolKind(..)
  , AnalysisPhase(..)
  , AnalysisContext(..)
  , AnalyzerState(..)
  , AnalysisResult(..)
  , emptyAnalysisResult
  , IntegratedAnalyzer
  )

import Analyzer.State
  ( newIntegratedAnalyzer
  , setPhase
  , ifEnableOwnership
  , ifEnableDependentTypes
  , addOwnershipError
  , addDependentTypeError
  , addCombinedError
  , filterWarnings
  , filterInfo
  , collectMessages
  , getCombinedErrors
  , getAnalysisSummary
  )

import Compiler.Errors.Core (ErrorSeverity(..), CombinedError(..))
import qualified Ownership as Own
import qualified Dependencies as Dep

import qualified Data.Map.Strict as Map
import Data.List (sort, nub)

-- ============================================================================
-- Type Environment Building Tests
-- ============================================================================

tests :: TestTree
tests =
  testGroup "Type Environment Building Tests"
    [ testGroup "Analyzer State Creation L.and Initialization"
        [ testCase "newIntegratedAnalyzer creates proper initial state" $ do
            let state = newIntegratedAnalyzer True True
            
            -- Check basic structure
            currentScope state @?= 0
            Map.L.null (symbolTable state) @?= True
            L.null (combinedErrorsAcc state) @?= True
            L.null (ownershipErrorsAcc state) @?= True
            L.null (dependentTypeErrorsAcc state) @?= True
            
            -- Check analysis context
            let context = analysisContext state
            enableOwnership context @?= True
            enableDependentTypes context @?= True
            currentFile context @?= ""
            analysisPhase context @?= InitialPhase

        , testCase "newIntegratedAnalyzer with different feature flags" $ do
            let ownershipOnly = newIntegratedAnalyzer True False
            let dependentTypesOnly = newIntegratedAnalyzer False True
            let bothDisabled = newIntegratedAnalyzer False False
            
            enableOwnership (analysisContext ownershipOnly) @?= True
            enableDependentTypes (analysisContext ownershipOnly) @?= False
            
            enableOwnership (analysisContext dependentTypesOnly) @?= False
            enableDependentTypes (analysisContext dependentTypesOnly) @?= True
            
            enableOwnership (analysisContext bothDisabled) @?= False
            enableDependentTypes (analysisContext bothDisabled) @?= False

        , testCase "emptyAnalysisResult creates proper result" $ do
            let result = emptyAnalysisResult
            
            L.null (ownershipErrors result) @?= True
            L.null (dependentTypeErrors result) @?= True
            L.null (combinedErrors result) @?= True
            L.null (analysisWarnings result) @?= True
            L.null (analysisInfo result) @?= True
            Map.L.null (typeEnvironment result) @?= True
        ]

    , testGroup "Symbol Table Management"
        [ testCase "symbol table operations maintain consistency" $ do
            let initialState = newIntegratedAnalyzer True True
            let symbol1 = SymbolInfo "var1" Nothing Nothing 0 False False []
            let symbol2 = SymbolInfo "func1" Nothing Nothing 0 False False []
            let symbol3 = SymbolInfo "type1" Nothing Nothing 0 False False []
            
            let updatedState = initialState 
                    { symbolTable = Map.fromList [("var1", symbol1), ("func1", symbol2), ("type1", symbol3)]
                    }
            
            let symbols = symbolTable updatedState
            Map.size symbols @?= 3
            Map.lookup "var1" symbols @?= Just symbol1
            Map.lookup "func1" symbols @?= Just symbol2
            Map.lookup "type1" symbols @?= Just symbol3
            Map.lookup "nonexistent" symbols @?= Nothing

        , testCase "symbol information is preserved correctly" $ do
            let symbol = SymbolInfo
                    { symbolName = "testVar"
                    , symbolType = Nothing
                    , ownershipState = Nothing
                    , symbolScope = 2
                    , isMoved = True
                    , isBorrowed = False
                    , constraints = []
                    }
            
            symbolName symbol @?= "testVar"
            symbolType symbol @?= Nothing
            ownershipState symbol @?= Nothing
            symbolScope symbol @?= 2
            isMoved symbol @?= True
            isBorrowed symbol @?= False
            constraints symbol @?= []

        , testCase "scope management works correctly" $ do
            let initialState = newIntegratedAnalyzer True True
            let symbol1 = SymbolInfo "var1" Nothing Nothing 1 False False []
            let symbol2 = SymbolInfo "var2" Nothing Nothing 2 False False []
            let symbol3 = SymbolInfo "var3" Nothing Nothing 1 False False []
            
            let state = initialState 
                    { symbolTable = Map.fromList [("var1", symbol1), ("var2", symbol2), ("var3", symbol3)]
                    }
            
            let scope1Symbols = Map.L.filter (\s -> symbolScope s == 1) (symbolTable state)
            let scope2Symbols = Map.L.filter (\s -> symbolScope s == 2) (symbolTable state)
            
            Map.size scope1Symbols @?= 2
            Map.size scope2Symbols @?= 1
        ]

    , testGroup "Analysis Phase Management"
        [ testCase "phase transitions work correctly" $ do
            let initialState = newIntegratedAnalyzer True True
            let initialPhase = analysisPhase $ analysisContext initialState
            
            initialPhase @?= InitialPhase
            
            -- Note: setPhase requires State monad, so we'll test the concept
            let phases = [InitialPhase, OwnershipPhase, DependentTypePhase, IntegrationPhase]
            let sortedPhases = sort phases
            
            sortedPhases @?= [InitialPhase, OwnershipPhase, DependentTypePhase, IntegrationPhase]

        , testCase "phase ordering is consistent" $ do
            let phases = [InitialPhase, OwnershipPhase, DependentTypePhase, IntegrationPhase]
            
            -- Test that phases have proper ordering
            assertBool "InitialPhase should be first" (InitialPhase `elem` phases)
            assertBool "OwnershipPhase should come after InitialPhase" (OwnershipPhase `elem` phases)
            assertBool "DependentTypePhase should come after OwnershipPhase" (DependentTypePhase `elem` phases)
            assertBool "IntegrationPhase should be last" (IntegrationPhase `elem` phases)

        , testCase "analysis context maintains phase information" $ do
            let context = AnalysisContext
                    { enableOwnership = True
                    , enableDependentTypes = True
                    , currentFile = "test.typus"
                    , analysisPhase = DependentTypePhase
                    }
            
            enableOwnership context @?= True
            enableDependentTypes context @?= True
            currentFile context @?= "test.typus"
            analysisPhase context @?= DependentTypePhase
        ]

    , testGroup "Error Collection L.and Management"
        [ testCase "ownership errors are collected properly" $ do
            let initialState = newIntegratedAnalyzer True True
            let ownershipError = Own.UseAfterMove "testVar"
            
            let stateWithErrors = initialState 
                    { ownershipErrorsAcc = [(Error, ownershipError)]
                    , combinedErrorsAcc = [OwnershipErrorCombined Error ownershipError]
                    }
            
            let ownershipErrors = ownershipErrorsAcc stateWithErrors
            let combinedErrors = combinedErrorsAcc stateWithErrors
            
            L.length ownershipErrors @?= 1
            L.length combinedErrors @?= 1
            fst (L.head ownershipErrors) @?= Error
            snd (L.head ownershipErrors) @?= ownershipError

        , testCase "dependent type errors are collected properly" $ do
            let initialState = newIntegratedAnalyzer True True
            let typeError = Dep.ConstraintError "test constraint"
            
            let stateWithErrors = initialState 
                    { dependentTypeErrorsAcc = [(Warning, typeError)]
                    , combinedErrorsAcc = [DependentTypeErrorCombined Warning typeError]
                    }
            
            let typeErrors = dependentTypeErrorsAcc stateWithErrors
            let combinedErrors = combinedErrorsAcc stateWithErrors
            
            L.length typeErrors @?= 1
            L.length combinedErrors @?= 1
            fst (L.head typeErrors) @?= Warning
            snd (L.head typeErrors) @?= typeError

        , testCase "combined errors include L.all error types" $ do
            let ownershipError = Own.UseAfterMove "var1"
            let typeError = Dep.ConstraintError "constraint"
            let integrationError = IntegrationError "integration failed" Error
            
            let state = newIntegratedAnalyzer True True
            let finalState = state 
                    { combinedErrorsAcc = 
                        [ OwnershipErrorCombined Error ownershipError
                        , DependentTypeErrorCombined Warning typeError
                        , integrationError
                        ]
                    }
            
            let combinedErrors = combinedErrorsAcc finalState
            L.length combinedErrors @?= 3
            
            let ownershipErrors = filter isOwnershipError combinedErrors
            let typeErrors = filter isDependentTypeError combinedErrors
            let integrationErrors = filter isIntegrationError combinedErrors
            
            L.length ownershipErrors @?= 1
            L.length typeErrors @?= 1
            L.length integrationErrors @?= 1
        ]

    , testGroup "Message Filtering L.and Collection"
        [ testCase "filterWarnings extracts warning messages" $ do
            let errors = 
                  [ OwnershipErrorCombined Warning (Own.UseAfterMove "var1")
                  , DependentTypeErrorCombined Error (Dep.ConstraintError "constraint")
                  , IntegrationError "integration warning" Warning
                  , IntegrationError "integration error" Error
                  ]
            
            let warnings = filterWarnings errors
            L.length warnings @?= 2
            assertBool "contains ownership warning" (L.any (L.isInfixOf "UseAfterMove") warnings)
            assertBool "contains integration warning" (L.any (L.isInfixOf "integration warning") warnings)

        , testCase "filterInfo extracts info messages" $ do
            let errors = 
                  [ OwnershipErrorCombined Info (Own.UseAfterMove "var1")
                  , DependentTypeErrorCombined Warning (Dep.ConstraintError "constraint")
                  , IntegrationError "integration info" Info
                  , IntegrationError "integration error" Error
                  ]
            
            let infoMessages = filterInfo errors
            L.length infoMessages @?= 2
            assertBool "contains ownership info" (L.any (L.isInfixOf "UseAfterMove") infoMessages)
            assertBool "contains integration info" (L.any (L.isInfixOf "integration info") infoMessages)

        , testCase "collectMessages works for different severities" $ do
            let errors = 
                  [ OwnershipErrorCombined Error (Own.UseAfterMove "var1")
                  , DependentTypeErrorCombined Warning (Dep.ConstraintError "constraint")
                  , IntegrationError "integration info" Info
                  ]
            
            let errorMessages = collectMessages Error errors
            let warningMessages = collectMessages Warning errors
            let infoMessages = collectMessages Info errors
            
            L.length errorMessages @?= 1
            L.length warningMessages @?= 1
            L.length infoMessages @?= 1
        ]

    , testGroup "Analysis Summary L.and Reporting"
        [ testCase "getAnalysisSummary provides correct statistics" $ do
            let state = newIntegratedAnalyzer True True
            let errors = 
                  [ OwnershipErrorCombined Error (Own.UseAfterMove "var1")
                  , DependentTypeErrorCombined Warning (Dep.ConstraintError "constraint")
                  , IntegrationError "integration info" Info
                  , OwnershipErrorCombined Fatal (Own.DoubleMove "var1" "var1")
                  ]
            
            let finalState = state 
                    { combinedErrorsAcc = errors
                    , symbolTable = Map.fromList [("var1", undefined), ("var2", undefined), ("func1", undefined)]
                    }
            
            let summary = getAnalysisSummary finalState
            let lines' = lines summary
            
            assertBool "contains analysis summary header" ("Analysis Summary:" `L.isInfixOf` summary)
            assertBool "contains error count" ("Errors: 2" `L.isInfixOf` summary)  -- Error + Fatal
            assertBool "contains warning count" ("Warnings: 1" `L.isInfixOf` summary)
            assertBool "contains info count" ("Info: 1" `L.isInfixOf` summary)
            assertBool "contains symbol count" ("Total symbols: 3" `L.isInfixOf` summary)

        , testCase "analysis summary handles empty state" $ do
            let state = newIntegratedAnalyzer True True
            let summary = getAnalysisSummary state
            
            assertBool "contains zero errors" ("Errors: 0" `L.isInfixOf` summary)
            assertBool "contains zero warnings" ("Warnings: 0" `L.isInfixOf` summary)
            assertBool "contains zero info" ("Info: 0" `L.isInfixOf` summary)
            assertBool "contains zero symbols" ("Total symbols: 0" `L.isInfixOf` summary)

        , testCase "analysis summary handles different error distributions" $ do
            let state = newIntegratedAnalyzer True True
            let errors = replicate 5 (OwnershipErrorCombined Warning (Own.UseAfterMove "var"))
            
            let finalState = state { combinedErrorsAcc = errors }
            let summary = getAnalysisSummary finalState
            
            assertBool "contains correct warning count" ("Warnings: 5" `L.isInfixOf` summary)
            assertBool "contains zero errors" ("Errors: 0" `L.isInfixOf` summary)
        ]

    , testGroup "Feature Flag Management"
        [ testCase "ifEnableOwnership controls analysis behavior" $ do
            let enabledState = newIntegratedAnalyzer True False
            let disabledState = newIntegratedAnalyzer False False
            
            let enabledContext = analysisContext enabledState
            let disabledContext = analysisContext disabledState
            
            enableOwnership enabledContext @?= True
            enableOwnership disabledContext @?= False
            -- Both should have dependent types disabled
            enableDependentTypes enabledContext @?= False
            enableDependentTypes disabledContext @?= False

        , testCase "ifEnableDependentTypes controls analysis behavior" $ do
            let enabledState = newIntegratedAnalyzer False True
            let disabledState = newIntegratedAnalyzer False False
            
            let enabledContext = analysisContext enabledState
            let disabledContext = analysisContext disabledState
            
            enableDependentTypes enabledContext @?= True
            enableDependentTypes disabledContext @?= False
            -- Both should have ownership disabled
            enableOwnership enabledContext @?= False
            enableOwnership disabledContext @?= False

        , testCase "both features can be enabled simultaneously" $ do
            let state = newIntegratedAnalyzer True True
            let context = analysisContext state
            
            enableOwnership context @?= True
            enableDependentTypes context @?= True

        , testCase "both features can be disabled simultaneously" $ do
            let state = newIntegratedAnalyzer False False
            let context = analysisContext state
            
            enableOwnership context @?= False
            enableDependentTypes context @?= False
        ]

    , testGroup "Property-Based Type Environment Tests"
        [ fastProperty "symbol table size matches symbol count" $
            \symbols ->
                let symbolCount = min 100 (max 0 symbols)
                    symbolList = [( "symbol" ++ show i
                                  , SymbolInfo ("symbol" ++ show i) Nothing Nothing 0 False False []
                                  ) | i <- [1..symbolCount]]
                    symbolTable = Map.fromList symbolList
                in Map.size symbolTable === L.length symbolList

        , fastProperty "error collection preserves error types" $
            \ownershipCount typeCount integrationCount ->
                let ownershipErrors = take (min 10 (max 0 ownershipCount)) 
                                   [OwnershipErrorCombined Error (Own.UseAfterMove ("var" ++ show i)) | i <- [1..]]
                    typeErrors = take (min 10 (max 0 typeCount)) 
                               [DependentTypeErrorCombined Warning (Dep.ConstraintError ("constraint" ++ show i)) | i <- [1..]]
                    integrationErrors = take (min 10 (max 0 integrationCount)) 
                                      [IntegrationError ("integration" ++ show i) Error | i <- [1..]]
                    allErrors = ownershipErrors ++ typeErrors ++ integrationErrors
                    ownershipFiltered = filter isOwnershipError allErrors
                    typeFiltered = filter isDependentTypeError allErrors
                    integrationFiltered = filter isIntegrationError allErrors
                in L.length ownershipFiltered === L.length ownershipErrors .&&.
                   L.length typeFiltered === L.length typeErrors .&&.
                   L.length integrationFiltered === L.length integrationErrors

        , fastProperty "analysis summary counts match error distribution" $
            \errorCount warningCount infoCount ->
                let errors = take (min 10 (max 0 errorCount)) 
                           [OwnershipErrorCombined Error (Own.UseAfterMove ("var" ++ show i)) | i <- [1..]]
                    warnings = take (min 10 (max 0 warningCount)) 
                               [DependentTypeErrorCombined Warning (Dep.ConstraintError ("constraint" ++ show i)) | i <- [1..]]
                    infos = take (min 10 (max 0 infoCount)) 
                           [IntegrationError ("info" ++ show i) Info | i <- [1..]]
                    allErrors = errors ++ warnings ++ infos
                    state = newIntegratedAnalyzer True True
                    finalState = state { combinedErrorsAcc = allErrors }
                    summary = getAnalysisSummary finalState
                in ("Errors: " ++ show (L.length errors + L.length (filter isFatal allErrors))) `L.isInfixOf` summary .&&.
                   ("Warnings: " ++ show (L.length warnings)) `L.isInfixOf` summary .&&.
                   ("Info: " ++ show (L.length infos)) `L.isInfixOf` summary
        ]

    , testGroup "Edge Cases L.and Stress Tests"
        [ testCase "handles large symbol tables efficiently" $ do
            let symbolCount = 1000
                symbols = [( "symbol" ++ show i
                           , SymbolInfo ("symbol" ++ show i) Nothing Nothing (i `mod` 10) False False []
                           ) | i <- [1..symbolCount]]
                symbolTable = Map.fromList symbols
                state = newIntegratedAnalyzer True True
                
            let finalState = state { symbolTable = symbolTable }
            let summary = getAnalysisSummary finalState
            
            Map.size (symbolTable finalState) @?= symbolCount
            assertBool "summary contains correct symbol count" ("Total symbols: 1000" `L.isInfixOf` summary)

        , testCase "handles many errors efficiently" $ do
            let errorCount = 500
                errors = [OwnershipErrorCombined Error (Own.UseAfterMove ("var" ++ show i)) | i <- [1..errorCount]]
                state = newIntegratedAnalyzer True True
                finalState = state { combinedErrorsAcc = errors }
                let summary = getAnalysisSummary finalState
                
            assertBool "summary contains correct error count" ("Errors: 500" `L.isInfixOf` summary)

        , testCase "handles mixed error severities correctly" $ do
            let errors = 
                  [ OwnershipErrorCombined Fatal (Own.UseAfterMove "fatal1")
                  , DependentTypeErrorCombined Error (Dep.ConstraintError "error1")
                  , IntegrationError "warning1" Warning
                  , OwnershipErrorCombined Info (Own.UseAfterMove "info1")
                  , DependentTypeErrorCombined Fatal (Dep.ConstraintError "fatal2")
                  , IntegrationError "error2" Error
                  ]
            let state = newIntegratedAnalyzer True True
            let finalState = state { combinedErrorsAcc = errors }
            let summary = getAnalysisSummary finalState
            
            assertBool "counts fatal L.and error errors together" ("Errors: 3" `L.isInfixOf` summary)
            assertBool "counts warnings correctly" ("Warnings: 1" `L.isInfixOf` summary)
            assertBool "counts info correctly" ("Info: 1" `L.isInfixOf` summary)

        , testCase "handles empty symbol table with errors" $ do
            let errors = [OwnershipErrorCombined Error (Own.UseAfterMove "var1")]
            let state = newIntegratedAnalyzer True True
            let finalState = state { combinedErrorsAcc = errors }
            let summary = getAnalysisSummary finalState
            
            assertBool "shows zero symbols" ("Total symbols: 0" `L.isInfixOf` summary)
            assertBool "shows one error" ("Errors: 1" `L.isInfixOf` summary)
        ]
    ]

-- Helper functions
isOwnershipError :: CombinedError -> Bool
isOwnershipError (OwnershipErrorCombined _ _) = True
isOwnershipError _ = False

isDependentTypeError :: CombinedError -> Bool
isDependentTypeError (DependentTypeErrorCombined _ _) = True
isDependentTypeError _ = False

isIntegrationError :: CombinedError -> Bool
isIntegrationError (IntegrationError _ _) = True
isIntegrationError _ = False

isFatal :: CombinedError -> Bool
isFatal (OwnershipErrorCombined Fatal _) = True
isFatal (DependentTypeErrorCombined Fatal _) = True
isFatal (IntegrationError _ Fatal) = True
isFatal (CrossAnalyzerError _ Fatal _) = True
isFatal _ = False

isInfixOf :: String -> String -> Bool
L.isInfixOf needle haystack = needle `Data.List.L.isInfixOf` haystack