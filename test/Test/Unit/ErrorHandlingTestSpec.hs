{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module ErrorHandlingTestSpec where

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
genErrorSeverity = elements [Error, Warning, Info, Hint]

genSourcePos :: Gen SourcePos
genSourcePos = SourcePos <$> choose (1, 1000) <*> choose (1, 1000) <*> choose (0, 100000)

genErrorLocation :: Gen ErrorLocation
genErrorLocation = oneof
  [ ErrorLocation <$> genSourcePos <*> genSourcePos
  , return UnknownLocation
  ]

genErrorContext :: Gen ErrorContext
genErrorContext = do
  phase <- elements [Parsing, TypeChecking, OwnershipAnalysis, CodeGeneration, Optimization]
  message <- genErrorMessage
  return $ ErrorContext phase message

genErrorRecovery :: Gen ErrorRecovery
genErrorRecovery = oneof
  [ return NoRecovery
  , SkipToken <$> genErrorMessage
  , InsertToken <$> genErrorMessage
  , ReplaceToken <$> genErrorMessage <*> genErrorMessage
  , RetryWithAlternative <$> genErrorMessage
  ]

genTypeError :: Gen TypeError
genTypeError = do
  message <- genErrorMessage
  severity <- genErrorSeverity
  location <- genErrorLocation
  context <- genErrorContext
  recovery <- genErrorRecovery
  return $ TypeError message severity location context recovery

genCompilerError :: Gen CompilerError
genCompilerError = do
  message <- genErrorMessage
  severity <- genErrorSeverity
  location <- genErrorLocation
  return $ CompilerError message severity location

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
  arbitrary = genCompilerError

-- Test properties for error handling

-- Property 1: Error severity ordering is consistent
prop_errorSeverityOrdering :: ErrorSeverity -> ErrorSeverity -> Bool
prop_errorSeverityOrdering sev1 sev2 =
  let comparison = compare sev1 sev2
      reverseComparison = compare sev2 sev1
  in if comparison == EQ 
     then reverseComparison == EQ
     else comparison * reverseComparison < 0

-- Property 2: Error is greater than or equal to warning
prop_errorGreaterThanWarning :: Bool
prop_errorGreaterThanWarning = Error >= Warning

-- Property 3: Warning is greater than or equal to info
prop_warningGreaterThanInfo :: Bool
prop_warningGreaterThanInfo = Warning >= Info

-- Property 4: Info is greater than or equal to hint
prop_infoGreaterThanHint :: Bool
prop_infoGreaterThanHint = Info >= Hint

-- Property 5: Error locations preserve start and end positions
prop_errorLocationPreservation :: SourcePos -> SourcePos -> Bool
prop_errorLocationPreservation start end =
  let location = ErrorLocation start end
  in case location of
    ErrorLocation s e -> s == start && e == end
    _ -> False

-- Property 6: Error contexts preserve phase and message
prop_errorContextPreservation :: CompilationPhase -> String -> Bool
prop_errorContextPreservation phase message =
  let context = ErrorContext phase message
  in case context of
    ErrorContext p m -> p == phase && m == message
    _ -> False

-- Property 7: Type errors preserve all their components
prop_typeErrorPreservation :: String -> ErrorSeverity -> ErrorLocation -> ErrorContext -> ErrorRecovery -> Bool
prop_typeErrorPreservation message severity location context recovery =
  let typeError = TypeError message severity location context recovery
  in case typeError of
    TypeError m s l c r -> m == message && s == severity && l == location && c == context && r == recovery
    _ -> False

-- Property 8: Compiler errors preserve their components
prop_compilerErrorPreservation :: String -> ErrorSeverity -> ErrorLocation -> Bool
prop_compilerErrorPreservation message severity location =
  let compilerError = CompilerError message severity location
  in case compilerError of
    CompilerError m s l -> m == message && s == severity && l == location
    _ -> False

errorHandlingTests :: TestTree
errorHandlingTests = testGroup "Error Handling Tests"
  [ testProperties "Error Severity Properties"
    [ ("Error severity ordering is consistent", prop_errorSeverityOrdering)
    , ("Error is greater than or equal to warning", prop_errorGreaterThanWarning)
    , ("Warning is greater than or equal to info", prop_warningGreaterThanInfo)
    , ("Info is greater than or equal to hint", prop_infoGreaterThanHint)
    ]
  , testProperties "Error Location Properties"
    [ ("Error locations preserve start and end positions", prop_errorLocationPreservation)
    ]
  , testProperties "Error Context Properties"
    [ ("Error contexts preserve phase and message", prop_errorContextPreservation)
    ]
  , testProperties "Type Error Properties"
    [ ("Type errors preserve all their components", prop_typeErrorPreservation)
    ]
  , testProperties "Compiler Error Properties"
    [ ("Compiler errors preserve their components", prop_compilerErrorPreservation)
    ]
  ]