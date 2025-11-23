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
import qualified Test.QuickCheck as QC

import TestSupport.QuickCheck (fastProperty)

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

        , testGroup "Property-based guarantees"
            [ fastProperty "filterWarnings matches a manual traversal" prop_filterWarningsMatchesManual
            , fastProperty "filterInfo matches a manual traversal" prop_filterInfoMatchesManual
            ]
        ]

prop_filterWarningsMatchesManual :: [ArbCombinedError] -> QC.Property
prop_filterWarningsMatchesManual forest =
    let errors = map unArbCombined forest
        expected = manualMessages Warning errors
        actual = filterWarnings errors
    in QC.counterexample (mismatch actual expected) (actual == expected)

prop_filterInfoMatchesManual :: [ArbCombinedError] -> QC.Property
prop_filterInfoMatchesManual forest =
    let errors = map unArbCombined forest
        expected = manualMessages Info errors
        actual = filterInfo errors
    in QC.counterexample (mismatch actual expected) (actual == expected)

mismatch :: Show a => a -> a -> String
mismatch actual expected =
    "actual = " <> show actual <> "\nexpected = " <> show expected

newtype ArbCombinedError = ArbCombinedError { unArbCombined :: CombinedError }
    deriving (Show)

instance QC.Arbitrary ArbCombinedError where
    arbitrary = ArbCombinedError <$> genCombinedError
    shrink (ArbCombinedError err) = ArbCombinedError <$> shrinkCombinedError err

genCombinedError :: QC.Gen CombinedError
genCombinedError = QC.sized go
  where
    go n
        | n <= 0 = genLeaf
        | otherwise = QC.frequency
            [ (4, genLeaf)
            , (1, genCross (n `div` 2))
            ]

    genLeaf = QC.oneof
        [ OwnershipErrorCombined <$> genSeverity <*> genOwnershipError
        , DependentTypeErrorCombined <$> genSeverity <*> genDependentTypeError
        , IntegrationError <$> genMessage <*> genSeverity
        ]

    genCross sizeHint = do
        msg <- genMessage
        sev <- genSeverity
        subs <- QC.resize sizeHint (QC.listOf (go (sizeHint `div` 2)))
        pure (CrossAnalyzerError msg sev subs)

shrinkCombinedError :: CombinedError -> [CombinedError]
shrinkCombinedError (CrossAnalyzerError msg sev subs) =
    IntegrationError msg sev : [CrossAnalyzerError msg sev subs' | subs' <- QC.shrinkList shrinkCombinedError subs]
shrinkCombinedError _ = []

genSeverity :: QC.Gen ErrorSeverity
genSeverity = QC.elements [Fatal, Error, Warning, Info]

genOwnershipError :: QC.Gen Own.OwnershipError
genOwnershipError = QC.oneof
    [ Own.UseAfterMove <$> genVarName
    , Own.DoubleMove <$> genVarName <*> genVarName
    , Own.BorrowWhileMoved <$> genVarName
    , Own.MutBorrowWhileBorrowed <$> genVarName
    , Own.BorrowWhileMutBorrowed <$> genVarName
    , Own.MultipleMutBorrows <$> genVarName
    , Own.UseWhileMutBorrowed <$> genVarName
    , Own.OutOfScope <$> genVarName
    , Own.BorrowError <$> genVarName
    , Own.CrossFunctionMove <$> genVarName <*> genVarName
    , Own.ParameterMoveMismatch <$> genVarName
    , Own.ControlFlowError <$> genMessage
    , Own.PathSensitiveError <$> genMessage
    , Own.LoopOwnershipError <$> genMessage
    ]

genDependentTypeError :: QC.Gen Dep.DependentTypeError
genDependentTypeError = QC.oneof
    [ Dep.TypeNotFound <$> genVarName
    , Dep.InvalidTypeArgument <$> genVarName
    , Dep.AmbiguousType <$> genVarName
    , Dep.ParseError <$> genMessage
    , Dep.SemanticError <$> genMessage
    ]

genVarName :: QC.Gen String
genVarName = QC.listOf1 (QC.elements ['a'..'z'])

genMessage :: QC.Gen String
genMessage = QC.listOf1 (QC.elements (['a'..'z'] ++ ['0'..'9'] ++ " _-"))

manualMessages :: ErrorSeverity -> [CombinedError] -> [String]
manualMessages sev = concatMap (flatten sev)
  where
    flatten target err = case err of
        OwnershipErrorCombined s e | s == target -> [show e]
        OwnershipErrorCombined _ _ -> []
        DependentTypeErrorCombined s e | s == target -> [show e]
        DependentTypeErrorCombined _ _ -> []
        IntegrationError msg s | s == target -> [msg]
        IntegrationError _ _ -> []
        CrossAnalyzerError msg s subs ->
            (if s == target then [msg] else []) ++ concatMap (flatten target) subs
