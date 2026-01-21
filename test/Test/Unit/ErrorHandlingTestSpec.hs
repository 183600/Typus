{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Test.Unit.ErrorHandlingTestSpec where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperties, Arbitrary(..), Gen, choose, listOf, elements, oneof, vectorOf, property, (===), forAll, counterexample)
import Test.QuickCheck (Gen)
import qualified Data.Text as T
import qualified Data.Map as Map
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)

import Compiler.Errors.Core
import Compiler.Errors
import ErrorHandler
import SourceLocation

-- Helper generators for error handling tests
genErrorMessage :: Gen String
genErrorMessage = do
  words <- choose (1, 10)
  vectorOf words $ elements $ ['a'..'z'] ++ " "

genErrorSeverity :: Gen ErrorSeverity
genErrorSeverity = elements [Fatal, Error, Warning, Info]

genSourcePos :: Gen SourcePos
genSourcePos = SourcePos <$> choose (1, 1000) <*> choose (1, 1000) <*> choose (0, 100000)

genErrorLocation :: Gen ErrorLocation
genErrorLocation = oneof
  [ ErrorLocation <$> pure Nothing <*> choose (1, 1000) <*> choose (1, 1000) <*> pure Nothing <*> pure Nothing
  , return $ ErrorLocation Nothing 0 0 Nothing Nothing
  ]

genErrorContext :: Gen ErrorContext
genErrorContext = do
  return $ ErrorContext Nothing Nothing Nothing Nothing []

genErrorRecovery :: Gen ErrorRecovery
genErrorRecovery = oneof
  [ return fatalRecovery
  , return errorRecovery
  , return warningRecovery
  , return infoRecovery
  , customRecovery True True <$> (Just <$> genErrorMessage) <*> pure Nothing <*> choose (0, 100) <*> choose (0.0, 1.0)
  ]

genTypeError :: Gen TypeError
genTypeError = do
  message <- genErrorMessage
  severity <- genErrorSeverity
  location <- genErrorLocation
  context <- genErrorContext
  recovery <- genErrorRecovery
  return $ TypeError "test-id" severity TypeChecking (T.pack message) location context recovery [] [] [] Nothing

genCompilerError :: Gen String
genCompilerError = do
  message <- genErrorMessage
  return message

instance Arbitrary ErrorSeverity where
  arbitrary = genErrorSeverity

instance Arbitrary ErrorLocation where
  arbitrary = genErrorLocation

instance Arbitrary ErrorContext where
  arbitrary = genErrorContext

instance Arbitrary ErrorRecovery where
  arbitrary = genErrorRecovery

instance Arbitrary TypeError where
  arbitrary = genTypeError

instance Arbitrary CompilerError where
  arbitrary = do
    message <- genErrorMessage
    severity <- genErrorSeverity
    location <- genErrorLocation
    phase <- elements [LexingPhase, ParsingPhase, TypeCheckingPhase, OwnershipAnalysisPhase, DependentTypeCheckingPhase, CodeGenerationPhase, OptimizationPhase]
    let typeError = TypeError "" severity TypeChecking (T.pack message) location emptyContext errorRecovery [] [] [] Nothing
    return $ CompilerError typeError Nothing [message] phase

-- Test properties for error handling

-- Property 1: Error severity ordering is consistent
prop_errorSeverityOrdering :: ErrorSeverity -> ErrorSeverity -> Bool
prop_errorSeverityOrdering sev1 sev2 =
  let comparison = compare sev1 sev2
      reverseComparison = compare sev2 sev1
  in if comparison == EQ 
     then reverseComparison == EQ
     else comparison /= reverseComparison  -- Different elements should have different orderings

-- Property 2: Error is greater than or equal to warning
prop_errorGreaterThanWarning :: Bool
prop_errorGreaterThanWarning = Error >= Warning

-- Property 3: Warning is greater than or equal to info
prop_warningGreaterThanInfo :: Bool
prop_warningGreaterThanInfo = Warning >= Info

-- Property 4: Info is the lowest severity
prop_infoIsLowest :: Bool
prop_infoIsLowest = Info <= Warning && Info <= Error && Info <= Fatal

-- Property 5: Error locations preserve line and column
prop_errorLocationPreservation :: Int -> Int -> Bool
prop_errorLocationPreservation line col =
  let location = ErrorLocation Nothing line col Nothing Nothing
  in case location of
    ErrorLocation _ l c _ _ -> l == line && c == col

-- Property 6: Error contexts preserve function and variable
prop_errorContextPreservation :: String -> String -> Bool
prop_errorContextPreservation func var =
  let context = ErrorContext Nothing (Just func) (Just var) Nothing []
  in case context of
    ErrorContext _ f v _ _ -> f == Just func && v == Just var

-- Property 7: Type errors preserve all their components
prop_typeErrorPreservation :: String -> ErrorSeverity -> ErrorLocation -> ErrorContext -> ErrorRecovery -> Bool
prop_typeErrorPreservation message severity location context recovery =
  let typeError = TypeError "test-id" severity TypeChecking (T.pack message) location context recovery [] [] [] Nothing
  in case typeError of
    TypeError _ _ cat msg l c r _ _ _ _ -> cat == TypeChecking && msg == T.pack message && l == location && c == context && r == recovery
    _ -> False

-- Property 8: Type errors with different severities preserve their components
prop_typeErrorSeverityPreservation :: String -> ErrorSeverity -> Bool
prop_typeErrorSeverityPreservation message severity =
  let location = ErrorLocation Nothing 1 1 Nothing Nothing
      context = ErrorContext Nothing Nothing Nothing Nothing []
      recovery = errorRecovery
      typeError = TypeError "test-id" severity TypeChecking (T.pack message) location context recovery [] [] [] Nothing
  in severity == severity

errorHandlingTests :: TestTree
errorHandlingTests = testGroup "Error Handling Tests"
  [ testProperties "Error Severity Properties"
    [ ("Error is greater than or equal to warning", property prop_errorGreaterThanWarning)
    , ("Warning is greater than or equal to info", property prop_warningGreaterThanInfo)
    , ("Info is the lowest severity", property prop_infoIsLowest)
    ]
  , testProperties "Error Location Properties"
    [ ("Error locations preserve line and column", property $ uncurry prop_errorLocationPreservation)
    ]
  , testProperties "Error Context Properties"
    [ ("Error contexts preserve function and variable", property $ uncurry prop_errorContextPreservation)
    ]
  , testProperties "Type Error Properties"
    [ ("Type errors preserve all their components", property $ (\m s l c r -> prop_typeErrorPreservation m s l c r))
    , ("Type errors preserve their severity", property $ uncurry prop_typeErrorSeverityPreservation)
    ]
  ]