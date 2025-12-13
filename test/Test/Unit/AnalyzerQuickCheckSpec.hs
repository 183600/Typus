{-# LANGUAGE CPP #-}

module Test.Unit.AnalyzerQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import TestSupport.Arbitrary
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property)

import Analyzer.Types
  ( SymbolInfo(..)
  , SymbolKind(..)
  , AnalysisResult(..)
  , AnalysisPhase(..)
  , AnalysisContext(..)
  , AnalyzerState(..)
  , ErrorSeverity(..)
  )
import qualified Dependencies as Dep
import qualified Ownership as Own
import qualified Data.Map.Strict as Map

-- Property: SymbolInfo with default values
prop_symbolinfo_default :: String -> Property
prop_symbolinfo_default name =
  let symbolInfo = SymbolInfo name Nothing Nothing 0 False False []
  in symbolName symbolInfo === name &&
     symbolType symbolInfo === Nothing &&
     ownershipState symbolInfo === Nothing &&
     symbolScope symbolInfo === 0 &&
     isMoved symbolInfo === False &&
     isBorrowed symbolInfo === False &&
     constraints symbolInfo === []

-- Property: SymbolInfo with all values set
prop_symbolinfo_complete :: String -> Dep.TypeVar -> Own.OwnershipType -> Int -> [Dep.Constraint] -> Property
prop_symbolinfo_complete name typeVar ownership scope constraintList =
  let symbolInfo = SymbolInfo name (Just typeVar) (Just ownership) scope True True constraintList
  in symbolName symbolInfo === name &&
     symbolType symbolInfo === Just typeVar &&
     ownershipState symbolInfo === Just ownership &&
     symbolScope symbolInfo === scope &&
     isMoved symbolInfo === True &&
     isBorrowed symbolInfo === True &&
     constraints symbolInfo === constraintList

-- Property: AnalysisResult with empty collections
prop_analysisresult_empty :: Property
prop_analysisresult_empty =
  let result = AnalysisResult [] [] [] [] [] Map.empty
  in null (ownershipErrors result) &&
     null (dependentTypeErrors result) &&
     null (combinedErrors result) &&
     null (analysisWarnings result) &&
     null (analysisInfo result) &&
     Map.null (typeEnvironment result)

-- Property: AnalysisResult with values
prop_analysisresult_with_values :: [Dep.DependentTypeError] -> [String] -> [String] -> Property
prop_analysisresult_with_values typeErrors warnings info =
  let ownershipErrs = [(Error, UseAfterMove "test")]
      combinedErrs = [OwnershipErrorCombined Error (UseAfterMove "test")]
      typeEnv = Map.singleton "Test" (Dep.TVCon "Int")
      result = AnalysisResult ownershipErrs (map ((,) Error) typeErrors) combinedErrs warnings info typeEnv
  in not (null (ownershipErrors result)) &&
     length (dependentTypeErrors result) === length typeErrors &&
     not (null (combinedErrors result)) &&
     analysisWarnings result === warnings &&
     analysisInfo result === info &&
     Map.size (typeEnvironment result) === 1

-- Property: AnalysisContext values are preserved
prop_analysiscontext_preserves :: Bool -> Bool -> String -> AnalysisPhase -> Property
prop_analysiscontext_preserves ownership deps file phase =
  let context = AnalysisContext ownership deps file phase
  in enableOwnership context === ownership &&
     enableDependentTypes context === deps &&
     currentFile context === file &&
     analysisPhase context === phase

-- Property: AnalyzerState with basic values
prop_analyzerstate_basic :: AnalysisContext -> Int -> Property
prop_analyzerstate_basic context scope =
  let state = AnalyzerState undefined undefined scope Map.empty context [] [] []
  in currentScope state === scope &&
     analysisContext state === context &&
     Map.null (symbolTable state) &&
     null (combinedErrorsAcc state) &&
     null (ownershipErrorsAcc state) &&
     null (dependentTypeErrorsAcc state)

-- Property: AnalysisPhase equality
prop_analysisphase_eq :: AnalysisPhase -> AnalysisPhase -> Bool
prop_analysisphase_eq phase1 phase2 = phase1 == phase2

-- Property: SymbolKind equality
prop_symbolkind_eq :: SymbolKind -> SymbolKind -> Bool
prop_symbolkind_eq kind1 kind2 = kind1 == kind2

-- Property: ErrorSeverity ordering
prop_errorseverity_ordering :: ErrorSeverity -> ErrorSeverity -> Property
prop_errorseverity_ordering sev1 sev2 =
  let result = compare sev1 sev2
  in (result == LT || result == EQ || result == GT) === True

-- Property: SymbolInfo with moved and borrowed flags
prop_symbolinfo_moved_borrowed :: String -> Bool -> Bool -> Property
prop_symbolinfo_moved_borrowed name moved borrowed =
  let symbolInfo = SymbolInfo name Nothing Nothing 0 moved borrowed []
  in isMoved symbolInfo === moved &&
     isBorrowed symbolInfo === borrowed

-- Property: SymbolInfo with constraints
prop_symbolinfo_constraints :: String -> [Dep.Constraint] -> Property
prop_symbolinfo_constraints name constraintList =
  let symbolInfo = SymbolInfo name Nothing Nothing 0 False False constraintList
  in constraints symbolInfo === constraintList &&
     length (constraints symbolInfo) === length constraintList

-- Property: AnalysisResult type environment operations
prop_analysisresult_typeenv :: [(String, Dep.TypeVar)] -> Property
prop_analysisresult_typeenv pairs =
  let typeEnv = Map.fromList pairs
      result = AnalysisResult [] [] [] [] [] typeEnv
  in Map.size (typeEnvironment result) === length pairs &&
     all (\(k, v) -> Map.lookup k (typeEnvironment result) == Just v) pairs

-- Property: AnalysisContext with different phases
prop_analysiscontext_phases :: Bool -> Bool -> String -> Property
prop_analysiscontext_phases ownership deps file =
  let phases = [InitialPhase, OwnershipPhase, DependentTypePhase, IntegrationPhase]
      contexts = map (\phase -> AnalysisContext ownership deps file phase) phases
  in all (\ctx -> enableOwnership ctx == ownership && enableDependentTypes ctx == deps) contexts &&
     all (\ctx -> currentFile ctx == file) contexts

-- Property: AnalyzerState symbol table operations
prop_analyzerstate_symboltable :: [(String, SymbolInfo)] -> AnalysisContext -> Property
prop_analyzerstate_symboltable pairs context =
  let symTable = Map.fromList pairs
      state = AnalyzerState undefined undefined 0 symTable context [] [] []
  in Map.size (symbolTable state) === length pairs &&
     all (\(k, v) -> Map.lookup k (symbolTable state) == Just v) pairs

-- Property: AnalysisResult error accumulation
prop_analysisresult_errors :: [Own.OwnershipError] -> [Dep.DependentTypeError] -> Property
prop_analysisresult_errors ownErrors depErrors =
  let ownErrs = map ((,) Error) ownErrors
      depErrs = map ((,) Warning) depErrors
      result = AnalysisResult ownErrs depErrs [] [] [] Map.empty
  in length (ownershipErrors result) === length ownErrors &&
     length (dependentTypeErrors result) === length depErrors

-- Property: AnalysisResult combined errors
prop_analysisresult_combined :: [CombinedError] -> Property
prop_analysisresult_combined combinedErrs =
  let result = AnalysisResult [] [] combinedErrs [] [] Map.empty
  in combinedErrors result === combinedErrs &&
     length (combinedErrors result) === length combinedErrs

-- Property: AnalysisResult warnings and info
prop_analysisresult_warnings_info :: [String] -> [String] -> Property
prop_analysisresult_warnings_info warnings info =
  let result = AnalysisResult [] [] [] warnings info Map.empty
  in analysisWarnings result === warnings &&
     analysisInfo result === info

-- Property: SymbolInfo scope changes
prop_symbolinfo_scope :: String -> Int -> Property
prop_symbolinfo_scope name scope =
  let symbolInfo = SymbolInfo name Nothing Nothing scope False False []
  in symbolScope symbolInfo === scope

-- Property: SymbolInfo type operations
prop_symbolinfo_type :: String -> Maybe Dep.TypeVar -> Property
prop_symbolinfo_type name maybeType =
  let symbolInfo = SymbolInfo name maybeType Nothing 0 False False []
  in symbolType symbolInfo === maybeType

-- Property: SymbolInfo ownership state
prop_symbolinfo_ownership :: String -> Maybe Own.OwnershipType -> Property
prop_symbolinfo_ownership name maybeOwnership =
  let symbolInfo = SymbolInfo name Nothing maybeOwnership 0 False False []
  in ownershipState symbolInfo === maybeOwnership

-- Property: AnalysisContext with different settings
prop_analysiscontext_settings :: Property
prop_analysiscontext_settings =
  let contexts = 
        [ AnalysisContext True True "test1.typus" InitialPhase
        , AnalysisContext True False "test2.typus" OwnershipPhase
        , AnalysisContext False True "test3.typus" DependentTypePhase
        , AnalysisContext False False "test4.typus" IntegrationPhase
        ]
  in all (\ctx -> enableOwnership ctx || enableDependentTypes ctx) contexts &&
     all (\ctx -> not (null (currentFile ctx))) contexts

-- Property: AnalyzerState error accumulation
prop_analyzerstate_errors :: [CombinedError] -> [Own.OwnershipError] -> [Dep.DependentTypeError] -> AnalysisContext -> Property
prop_analyzerstate_errors combined own dep context =
  let ownErrs = map ((,) Error) own
      depErrs = map ((,) Warning) dep
      state = AnalyzerState undefined undefined 0 Map.empty context combined ownErrs depErrs
  in combinedErrorsAcc state === combined &&
     ownershipErrorsAcc state === ownErrs &&
     dependentTypeErrorsAcc state === depErrs

-- Property: SymbolInfo with all fields
prop_symbolinfo_all_fields :: String -> Maybe Dep.TypeVar -> Maybe Own.OwnershipType -> Int -> Bool -> Bool -> [Dep.Constraint] -> Property
prop_symbolinfo_all_fields name maybeType maybeOwnership scope moved borrowed constraints =
  let symbolInfo = SymbolInfo name maybeType maybeOwnership scope moved borrowed constraints
  in symbolName symbolInfo === name &&
     symbolType symbolInfo === maybeType &&
     ownershipState symbolInfo === maybeOwnership &&
     symbolScope symbolInfo === scope &&
     isMoved symbolInfo === moved &&
     isBorrowed symbolInfo === borrowed &&
     constraints symbolInfo === constraints

-- Property: AnalysisResult comprehensive
prop_analysisresult_comprehensive :: [Own.OwnershipError] -> [Dep.DependentTypeError] -> [CombinedError] -> [String] -> [String] -> [(String, Dep.TypeVar)] -> Property
prop_analysisresult_comprehensive ownErrors depErrors combinedErrs warnings info typePairs =
  let ownErrs = map ((,) Error) ownErrors
      depErrs = map ((,) Warning) depErrors
      typeEnv = Map.fromList typePairs
      result = AnalysisResult ownErrs depErrs combinedErrs warnings info typeEnv
  in ownershipErrors result === ownErrs &&
     dependentTypeErrors result === depErrs &&
     combinedErrors result === combinedErrs &&
     analysisWarnings result === warnings &&
     analysisInfo result === info &&
     typeEnvironment result === typeEnv

-- Property: AnalyzerState comprehensive
prop_analyzerstate_comprehensive :: Int -> [(String, SymbolInfo)] -> [CombinedError] -> [Own.OwnershipError] -> [Dep.DependentTypeError] -> Bool -> Bool -> String -> AnalysisPhase -> Property
prop_analyzerstate_comprehensive scope symbolPairs combined own dep ownership deps file phase =
  let symTable = Map.fromList symbolPairs
      ownErrs = map ((,) Error) own
      depErrs = map ((,) Warning) dep
      context = AnalysisContext ownership deps file phase
      state = AnalyzerState undefined undefined scope symTable context combined ownErrs depErrs
  in currentScope state === scope &&
     symbolTable state === symTable &&
     combinedErrorsAcc state === combined &&
     ownershipErrorsAcc state === ownErrs &&
     dependentTypeErrorsAcc state === depErrs &&
     analysisContext state === context

-- Property: AnalysisPhase ordering
prop_analysisphase_ordering :: Property
prop_analysisphase_ordering =
  let phases = [InitialPhase, OwnershipPhase, DependentTypePhase, IntegrationPhase]
      ordered = zip phases (tail phases)
  in all (\(p1, p2) -> p1 < p2) ordered

-- Property: SymbolKind exhaustive
prop_symbolkind_exhaustive :: SymbolKind -> Property
prop_symbolkind_exhaustive kind =
  let isKnownKind = kind `elem` [SymbolVariable, SymbolFunction, SymbolType, SymbolConstant, SymbolPackage, SymbolModule]
  in isKnownKind === True

-- Property: ErrorSeverity exhaustive
prop_errorseverity_exhaustive :: ErrorSeverity -> Property
prop_errorseverity_exhaustive sev =
  let isKnownSev = sev `elem` [Error, Warning, Info]
  in isKnownSev === True

tests :: TestTree
tests = testGroup "Analyzer QuickCheck tests"
  [ fastProperty "SymbolInfo with default values" prop_symbolinfo_default
  , fastProperty "SymbolInfo with all values set" prop_symbolinfo_complete
  , fastProperty "AnalysisResult with empty collections" prop_analysisresult_empty
  , fastProperty "AnalysisResult with values" prop_analysisresult_with_values
  , fastProperty "AnalysisContext values are preserved" prop_analysiscontext_preserves
  , fastProperty "AnalyzerState with basic values" prop_analyzerstate_basic
  , fastProperty "AnalysisPhase equality" prop_analysisphase_eq
  , fastProperty "SymbolKind equality" prop_symbolkind_eq
  , fastProperty "ErrorSeverity ordering" prop_errorseverity_ordering
  , fastProperty "SymbolInfo with moved and borrowed flags" prop_symbolinfo_moved_borrowed
  , fastProperty "SymbolInfo with constraints" prop_symbolinfo_constraints
  , fastProperty "AnalysisResult type environment operations" prop_analysisresult_typeenv
  , fastProperty "AnalysisContext with different phases" prop_analysiscontext_phases
  , fastProperty "AnalyzerState symbol table operations" prop_analyzerstate_symboltable
  , fastProperty "AnalysisResult error accumulation" prop_analysisresult_errors
  , fastProperty "AnalysisResult combined errors" prop_analysisresult_combined
  , fastProperty "AnalysisResult warnings and info" prop_analysisresult_warnings_info
  , fastProperty "SymbolInfo scope changes" prop_symbolinfo_scope
  , fastProperty "SymbolInfo type operations" prop_symbolinfo_type
  , fastProperty "SymbolInfo ownership state" prop_symbolinfo_ownership
  , fastProperty "AnalysisContext with different settings" prop_analysiscontext_settings
  , fastProperty "AnalyzerState error accumulation" prop_analyzerstate_errors
  , fastProperty "SymbolInfo with all fields" prop_symbolinfo_all_fields
  , fastProperty "AnalysisResult comprehensive" prop_analysisresult_comprehensive
  , fastProperty "AnalyzerState comprehensive" prop_analyzerstate_comprehensive
  , fastProperty "AnalysisPhase ordering" prop_analysisphase_ordering
  , fastProperty "SymbolKind exhaustive" prop_symbolkind_exhaustive
  , fastProperty "ErrorSeverity exhaustive" prop_errorseverity_exhaustive
  ]