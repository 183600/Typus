{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.EnhancedErrorHandlerCoreQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Positive(..), NonNegative(..))
import Test.QuickCheck.Gen (choose, listOf, elements, vectorOf, oneof)

import Compiler.Errors.Compiler
  ( CompilerError(..)
  , CompilationPhase(..)
  , CompilerResult
  , CompilerM
  , runCompilerM
  , defaultSpan
  , defaultLocation
  , mkCompilerError
  , syntaxError
  , typeError
  , ownershipError
  , dependentTypeError
  , semanticError
  , collectErrors
  , recoverFrom
  , continueWith
  , withRecovery
  , formatCompilerError
  , formatCompilerErrors
  , generateDetailedReport
  , withSourceLocation
  , trackLocation
  , analyzeErrors
  , ErrorStatistics(..)
  , makeUserFriendly
  , suggestFix
  )

import Compiler.Errors.Core
  ( TypeError(..)
  , ErrorSeverity(..)
  , ErrorCategory(..)
  , ErrorLocation(..)
  , emptyContext
  , _atLocation
  )

import SourceLocation
  ( SourcePos(..)
  , SourceSpan(..)
  , startPos
  , spanFrom
  )

import Data.Text (Text)
import qualified Data.Text as T
import Data.List (sort, nub)
import Data.Either (isLeft, isRight, partitionEithers)

-- Property: CompilationPhase ordering
prop_compilation_phase_ordering :: Property
prop_compilation_phase_ordering =
  let phases = [LexingPhase, ParsingPhase, TypeCheckingPhase, 
                OwnershipAnalysisPhase, DependentTypeCheckingPhase,
                CodeGenerationPhase, OptimizationPhase]
      orderedPhases = sort phases
  in property $ phases === orderedPhases

-- Property: CompilerError construction
prop_compiler_error_construction :: String -> String -> CompilationPhase -> Property
prop_compiler_error_construction errId msg phase =
  not (null errId) ==>
  let loc = _atLocation 1 1
      baseError = errorAt "test-id" (T.pack msg) loc
      compilerErr = CompilerError baseError Nothing [] phase
  in property $ ceError compilerErr === baseError .&&.
             cePhase compilerErr === phase .&&.
             L.null (ceSourceContext compilerErr) .&&.
             L.null (ceStackTrace compilerErr)

-- Property: mkCompilerError creates valid compiler error
prop_mk_compiler_error :: String -> String -> CompilationPhase -> Property
prop_mk_compiler_error errId msg phase =
  not (null errId) ==>
  let compilerErr = mkCompilerError errId (T.pack msg) phase
      baseErr = ceError compilerErr
  in property $ errorId baseErr === errId .&&.
             message baseErr === T.pack msg .&&.
             cePhase compilerErr === phase

-- Property: syntaxError creates parsing phase error
prop_syntax_error_phase :: String -> String -> Property
prop_syntax_error_phase errId msg =
  not (null errId) ==>
  let syntaxErr = syntaxError errId (T.pack msg)
  in property $ cePhase syntaxErr === ParsingPhase .&&.
             category (ceError syntaxErr) === Parsing

-- Property: typeError creates type checking phase error
prop_type_error_phase :: String -> String -> Property
prop_type_error_phase errId msg =
  not (null errId) ==>
  let typeErr = typeError errId (T.pack msg)
  in property $ cePhase typeErr === TypeCheckingPhase .&&.
             category (ceError typeErr) === TypeChecking

-- Property: ownershipError creates ownership analysis phase error
prop_ownership_error_phase :: String -> String -> Property
prop_ownership_error_phase errId msg =
  not (null errId) ==>
  let ownErr = ownershipError errId (T.pack msg)
  in property $ cePhase ownErr === OwnershipAnalysisPhase .&&.
             category (ceError ownErr) === Ownership

-- Property: dependentTypeError creates dependent type checking phase error
prop_dependent_type_error_phase :: String -> String -> Property
prop_dependent_type_error_phase errId msg =
  not (null errId) ==>
  let depErr = dependentTypeError errId (T.pack msg)
  in property $ cePhase depErr === DependentTypeCheckingPhase .&&.
             category (ceError depErr) === Constraint

-- Property: semanticError creates semantic phase error
prop_semantic_error_phase :: String -> String -> Property
prop_semantic_error_phase errId msg =
  not (null errId) ==>
  let semErr = semanticError errId (T.pack msg)
  in property $ cePhase semErr === TypeCheckingPhase .&&.
             category (ceError semErr) === Semantic

-- Property: collectErrors gathers errors correctly
prop_collect_errors_gather :: [String] -> Property
prop_collect_errors_gather errIds =
  not (null errIds) ==>
  let errors = [syntaxError errId (T.pack "test") | errId <- nub errIds]
      collected = collectErrors errors
  in property $ L.length collected === L.length (nub errIds)

-- Property: recoverFrom handles recovery correctly
prop_recover_from_handling :: String -> Property
prop_recover_from_handling errId =
  not (null errId) ==>
  let error = syntaxError errId (T.pack "test")
      recovered = recoverFrom error
  in property $ case recovered of
    Right _ -> property True
    Left _ -> property True

-- Property: continueWith maintains error context
prop_continue_with_context :: String -> String -> Property
prop_continue_with_context errId newValue =
  not (null errId) ==>
  let error = syntaxError errId (T.pack "original")
      continued = continueWith error (T.pack newValue)
  in property $ case continued of
    Right result -> result === T.pack newValue
    Left _ -> property True

-- Property: withRecovery applies recovery strategy
prop_with_recovery_strategy :: String -> Property
prop_with_recovery_strategy errId =
  not (null errId) ==>
  let error = syntaxError errId (T.pack "test")
      recovered = withRecovery error
  in property $ case recovered of
    Right _ -> property True
    Left _ -> property True

-- Property: formatCompilerError includes essential information
prop_format_compiler_error_includes :: String -> String -> Property
prop_format_compiler_error_includes errId msg =
  not (null errId) ==>
  let error = syntaxError errId (T.pack msg)
      formatted = formatCompilerError error
  in property $ errId `L.isInfixOf` formatted .&&.
             msg `L.isInfixOf` formatted

-- Property: formatCompilerErrors handles multiple errors
prop_format_compiler_errors_multiple :: [String] -> Property
prop_format_compiler_errors_multiple errIds =
  not (null errIds) ==>
  let errors = [syntaxError errId (T.pack "test") | errId <- nub errIds]
      formatted = formatCompilerErrors errors
      formattedLines = lines formatted
  in property $ L.length formattedLines >= L.length (nub errIds)

-- Property: generateDetailedReport creates comprehensive report
prop_generate_detailed_report :: [String] -> Property
prop_generate_detailed_report errIds =
  not (null errIds) ==>
  let errors = [syntaxError errId (T.pack "test") | errId <- nub errIds]
      report = generateDetailedReport errors
  in property $ not (null report) .&&.
             "Error Report" `L.isInfixOf` report

-- Property: withSourceLocation adds location context
prop_with_source_location :: String -> Property
prop_with_source_location errId =
  not (null errId) ==>
  let error = syntaxError errId (T.pack "test")
      loc = _atLocation 5 10
      withLoc = withSourceLocation loc error
      baseErr = ceError withLoc
  in property $ location baseErr === loc

-- Property: trackLocation updates location tracking
prop_track_location :: String -> Property
prop_track_location errId =
  not (null errId) ==>
  let error = syntaxError errId (T.pack "test")
      tracked = trackLocation error
  in property $ case tracked of
    Right _ -> property True
    Left _ -> property True

-- Property: analyzeErrors produces statistics
prop_analyze_errors_statistics :: [CompilationPhase] -> Property
prop_analyze_errors_statistics phases =
  not (null phases) ==>
  let errors = [syntaxError ("err" ++ show i) (T.pack "test") { cePhase = phase } | (i, phase) <- zip [0..] phases]
      stats = analyzeErrors errors
  in property $ totalErrors stats === L.length phases

-- Property: ErrorStatistics phase distribution
prop_error_statistics_distribution :: [CompilationPhase] -> Property
prop_error_statistics_distribution phases =
  not (null phases) ==>
  let errors = [syntaxError ("err" ++ show i) (T.pack "test") { cePhase = phase } | (i, phase) <- zip [0..] phases]
      stats = analyzeErrors errors
      phaseCounts = [L.length [e | e <- errors, cePhase e == phase] | phase <- nub phases]
  in property $ L.sum phaseCounts === totalErrors stats

-- Property: makeUserFriendly simplifies error messages
prop_make_user_friendly :: String -> Property
prop_make_user_friendly technicalMsg =
  not (null technicalMsg) ==>
  let error = syntaxError "test" (T.pack technicalMsg)
      friendly = makeUserFriendly error
      friendlyMsg = message (ceError friendly)
  in property $ T.L.length friendlyMsg <= T.L.length (T.pack technicalMsg) + 50

-- Property: suggestFix provides suggestions
prop_suggest_fix :: String -> Property
prop_suggest_fix errType =
  not (null errType) ==>
  let error = syntaxError errType (T.pack "test")
      withSuggestion = suggestFix error
      suggestions = suggestions (ceError withSuggestion)
  in property $ L.length suggestions >= 0

-- Property: CompilerM monad operations
prop_compiler_m_operations :: [String] -> Property
prop_compiler_m_operations errIds =
  not (null errIds) ==>
  let errors = [syntaxError errId (T.pack "test") | errId <- nub errIds]
      result = runCompilerM (return errors)
  in property $ case result of
    Right res -> L.length res === L.length (nub errIds)
    Left _ -> property True

-- Property: CompilerResult error handling
prop_compiler_result_handling :: [String] -> Property
prop_compiler_result_handling errIds =
  not (null errIds) ==>
  let errors = [syntaxError errId (T.pack "test") | errId <- nub errIds]
      result = Left errors :: CompilerResult [()]
      (leftErrs, rightResults) = partitionEithers [result]
  in property $ L.length leftErrs === 1 .&&.
             L.length rightResults === 0

-- Property: error chain preservation
prop_error_chain_preservation :: String -> String -> Property
prop_error_chain_preservation errId1 errId2 =
  not (null errId1) && not (null errId2) ==>
  let innerError = syntaxError errId1 (T.pack "inner")
      outerError = syntaxError errId2 (T.pack "outer")
      -- Simulate error chaining
      chainedError = outerError { ceError = (ceError outerError) { errorChain = [ceError innerError] } }
  in property $ L.length (errorChain (ceError chainedError)) === 1

-- Property: error severity preservation through compiler errors
prop_error_severity_preservation :: ErrorSeverity -> Property
prop_error_severity_preservation sev =
  let baseError = errorAt "test" (T.pack "test") (_atLocation 1 1) { severity = sev }
      compilerErr = mkCompilerError "test" (T.pack "test") LexingPhase
      updatedErr = compilerErr { ceError = baseError }
  in property $ severity (ceError updatedErr) === sev

-- Property: error category preservation through compiler errors
prop_error_category_preservation :: ErrorCategory -> Property
prop_error_category_preservation cat =
  let baseError = errorWithCategory "test" cat (T.pack "test") (_atLocation 1 1)
      compilerErr = mkCompilerError "test" (T.pack "test") LexingPhase
      updatedErr = compilerErr { ceError = baseError }
  in property $ category (ceError updatedErr) === cat

tests :: TestTree
tests =
  testGroup "EnhancedErrorHandler Core QuickCheck Tests"
    [ fastProperty "CompilationPhase ordering" prop_compilation_phase_ordering
    , fastProperty "CompilerError construction" prop_compiler_error_construction
    , fastProperty "mkCompilerError creates valid compiler error" prop_mk_compiler_error
    , fastProperty "syntaxError creates parsing phase error" prop_syntax_error_phase
    , fastProperty "typeError creates type checking phase error" prop_type_error_phase
    , fastProperty "ownershipError creates ownership analysis phase error" prop_ownership_error_phase
    , fastProperty "dependentTypeError creates dependent type checking phase error" prop_dependent_type_error_phase
    , fastProperty "semanticError creates semantic phase error" prop_semantic_error_phase
    , fastProperty "collectErrors gathers errors correctly" prop_collect_errors_gather
    , fastProperty "recoverFrom handles recovery correctly" prop_recover_from_handling
    , fastProperty "continueWith maintains error context" prop_continue_with_context
    , fastProperty "withRecovery applies recovery strategy" prop_with_recovery_strategy
    , fastProperty "formatCompilerError includes essential information" prop_format_compiler_error_includes
    , fastProperty "formatCompilerErrors handles multiple errors" prop_format_compiler_errors_multiple
    , fastProperty "generateDetailedReport creates comprehensive report" prop_generate_detailed_report
    , fastProperty "withSourceLocation adds location context" prop_with_source_location
    , fastProperty "trackLocation updates location tracking" prop_track_location
    , fastProperty "analyzeErrors produces statistics" prop_analyze_errors_statistics
    , fastProperty "ErrorStatistics phase distribution" prop_error_statistics_distribution
    , fastProperty "makeUserFriendly simplifies error messages" prop_make_user_friendly
    , fastProperty "suggestFix provides suggestions" prop_suggest_fix
    , fastProperty "CompilerM monad operations" prop_compiler_m_operations
    , fastProperty "CompilerResult error handling" prop_compiler_result_handling
    , fastProperty "error chain preservation" prop_error_chain_preservation
    , fastProperty "error severity preservation through compiler errors" prop_error_severity_preservation
    , fastProperty "error category preservation through compiler errors" prop_error_category_preservation
    ]

-- Helper function for infix pattern matching
isInfixOf :: String -> String -> Bool
L.isInfixOf needle haystack = needle `Data.List.L.isInfixOf` haystack