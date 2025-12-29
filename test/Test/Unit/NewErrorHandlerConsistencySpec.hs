{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Unit.NewErrorHandlerConsistencySpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import Test.Tasty.QuickCheck (testProperty, Property, Arbitrary(..), Gen, choose, oneof, listOf, vectorOf, forAll, elements)
import qualified Data.Text as T
import qualified Data.List as List
import Data.Time (UTCTime, getCurrentTime)
import Data.Maybe (isJust, isNothing)

import Compiler.Errors.Core
import SourceLocation (SourcePos(..), SourceSpan(..), startPos)
import TestSupport.QuickCheck (fastProperty)

-- | Test error handling consistency properties
tests :: TestTree
tests =
  testGroup "New Error Handler Consistency Tests"
    [ testGroup "Error collection properties"
        [ testCase "new error collector starts empty" $ do
            collector <- newErrorCollector
            hasErrors collector @?= False
            hasWarnings collector @?= False
            getAllMessages collector @?= []

        , testCase "adding errors increases error count" $ do
            collector <- newErrorCollector
            let error1 = errorAt startPos "First error"
                error2 = errorAt startPos "Second error"
            collector1 <- addError error1 collector
            collector2 <- addError error2 collector1
            hasErrors collector2 @?= True
            length (getErrors collector2) @?= 2

        , testCase "adding warnings increases warning count" $ do
            collector <- newErrorCollector
            let warning1 = warningAt startPos "First warning"
                warning2 = warningAt startPos "Second warning"
            collector1 <- addWarning warning1 collector
            collector2 <- addWarning warning2 collector1
            hasWarnings collector2 @?= True
            length (getWarnings collector2) @?= 2

        , testCase "info messages don't affect error/warning flags" $ do
            collector <- newErrorCollector
            let info1 = infoAt startPos "Info message"
            collector1 <- addInfo info1 collector
            hasErrors collector1 @?= False
            hasWarnings collector1 @?= False
            length (getInfo collector1) @?= 1
        ]

    , testGroup "Error formatting consistency"
        [ testCase "formatError includes message and location" $ do
            let error = errorAt (SourcePos 5 10) "Test error message"
                formatted = formatError error
            formatted `assertBool` ("Test error message" `T.isInfixOf` formatted)
            formatted `assertBool` ("5:10" `T.isInfixOf` formatted)

        , testCase "formatErrors maintains order" $ do
            let error1 = errorAt (SourcePos 1 1) "First error"
                error2 = errorAt (SourcePos 2 1) "Second error"
                error3 = errorAt (SourcePos 3 1) "Third error"
                errors = [error1, error2, error3]
                formatted = formatErrors errors
            formatted `assertBool` ("First error" `T.isInfixOf` formatted)
            formatted `assertBool` ("Second error" `T.isInfixOf` formatted)
            formatted `assertBool` ("Third error" `T.isInfixOf` formatted)
            -- Check order: first should appear before second
            let firstPos = T.findIndex (== 'F') formatted
                secondPos = T.findIndex (== 'S') formatted
            case (firstPos, secondPos) of
              (Just fp, Just sp) -> assertBool "Order should be preserved" (fp < sp)
              _ -> assertBool "Should find both errors" False
        ]

    , testGroup "Error recovery properties"
        [ testCase "can recover from warnings" $ do
            let warning = warningAt startPos "Recoverable warning"
            canRecoverFrom warning @?= True

        , testCase "can recover from info messages" $ do
            let info = infoAt startPos "Information"
            canRecoverFrom info @?= True

        , testCase "should continue after warnings" $ do
            let warning = warningAt startPos "Continue after warning"
            shouldContinueAfter warning @?= True

        , testCase "should continue after info" $ do
            let info = infoAt startPos "Continue after info"
            shouldContinueAfter info @?= True
        ]

    , testGroup "Error location consistency"
        [ testCase "errorAt creates correct location" $ do
            let pos = SourcePos 10 20
                error = errorAt pos "Location test"
            case errorLocation error of
              ErrorLocation span -> do
                sourceLine (spanStart span) @?= 10
                sourceColumn (spanStart span) @?= 20
              _ -> assertBool "Should have ErrorLocation" False

        , testCase "errorAtWithTimestamp includes timestamp" $ do
            time <- getCurrentTime
            let error = errorAtWithUTCTime time startPos "Timestamp test"
            case errorTimestamp error of
              Just ts -> ts @?= time
              Nothing -> assertBool "Should have timestamp" False
        ]

    , testGroup "Error category properties"
        [ fastProperty "errorWithCategory preserves category" prop_errorWithCategoryPreservesCategory
        , fastProperty "warningWithCategory preserves category" prop_warningWithCategoryPreservesCategory
        , fastProperty "infoWithCategory preserves category" prop_infoWithCategoryPreservesCategory
        ]

    , testGroup "Error context properties"
        [ testCase "emptyContext has no entries" $ do
            let ctx = emptyContext
            ctx @?= ErrorContext []

        , testCase "error context formatting includes context info" $ do
            let ctx = ErrorContext [("variable", "x"), ("type", "int")]
                error = TypeError "Test error" ErrorSeverityError ErrorCategoryTypeChecking 
                              (ErrorLocation (spanFrom startPos 5)) ctx Nothing
                formatted = formatError error
            formatted `assertBool` ("variable" `T.isInfixOf` formatted)
            formatted `assertBool` ("type" `T.isInfixOf` formatted)
        ]

    , testGroup "Combined error properties"
        [ testCase "combined errors include all individual errors" $ do
            let error1 = errorAt startPos "Error 1"
                error2 = errorAt startPos "Error 2"
                combined = CombinedError [error1, error2] ErrorSeverityError ErrorCategoryGeneral
            case combinedErrors combined of
              errors -> length errors @?= 2

        , fastProperty "combined error severity is maximum" prop_combinedErrorSeverityMaximum
        ]

    , testGroup "Property-based consistency tests"
        [ fastProperty "error formatting is deterministic" prop_errorFormattingDeterministic
        , fastProperty "error collection maintains order" prop_errorCollectionMaintainsOrder
        , fastProperty "error filtering works correctly" prop_errorFilteringWorks
        , fastProperty "error location tracking is consistent" prop_errorLocationTrackingConsistent
        ]
    ]

-- Property: errorWithCategory preserves category
prop_errorWithCategoryPreservesCategory :: String -> ErrorCategory -> Property
prop_errorWithCategoryPreservesCategory msg category =
  let error = errorWithCategory category startPos msg
  in errorCategory error == category

-- Property: warningWithCategory preserves category
prop_warningWithCategoryPreservesCategory :: String -> ErrorCategory -> Property
prop_warningWithCategoryPreservesCategory msg category =
  let warning = warningWithCategory category startPos msg
  in errorCategory warning == category

-- Property: infoWithCategory preserves category
prop_infoWithCategoryPreservesCategory :: String -> ErrorCategory -> Property
prop_infoWithCategoryPreservesCategory msg category =
  let info = infoWithCategory category startPos msg
  in errorCategory info == category

-- Property: combined error severity is maximum
prop_combinedErrorSeverityMaximum :: Positive Int -> Property
prop_combinedErrorSeverityMaximum (Positive n) =
  let errors = take n $ cycle 
        [ errorAt startPos "Error"
        , warningAt startPos "Warning"
        , infoAt startPos "Info"
        ]
      combined = CombinedError errors ErrorSeverityError ErrorCategoryGeneral
  in case errors of
       [] -> property True
       _ -> errorSeverity combined >= maximum (map errorSeverity errors)

-- Property: error formatting is deterministic
prop_errorFormattingDeterministic :: String -> SourcePos -> Property
prop_errorFormattingDeterministic msg pos =
  let error = errorAt pos msg
      formatted1 = formatError error
      formatted2 = formatError error
  in formatted1 == formatted2

-- Property: error collection maintains order
prop_errorCollectionMaintainsOrder :: Positive Int -> Property
prop_errorCollectionMaintainsOrder (Positive n) =
  let messages = ["Error " ++ show i | i <- [1..n]]
      errors = [errorAt (posAtLineCol i 1) msg | (i, msg) <- zip [1..] messages]
  in property $ do
       collector <- newErrorCollector
       let addAll errs coll = foldM (\c e -> addError e c) coll errs
       finalCollector <- addAll errors collector
       let collectedErrors = getErrors finalCollector
           collectedMessages = map errorMessage collectedErrors
       return $ collectedMessages == messages

-- Property: error filtering works correctly
prop_errorFilteringWorks :: Positive Int -> Property
prop_errorFilteringWorks (Positive n) =
  let messages = ["Error " ++ show i | i <- [1..n]]
      errors = [errorAt startPos msg | msg <- messages]
      hasEvenLength = even . length . errorMessage
      filtered = filter hasEvenLength errors
  in length filtered <= n && all hasEvenLength filtered

-- Property: error location tracking is consistent
prop_errorLocationTrackingConsistent :: Positive Int -> Positive Int -> String -> Property
prop_errorLocationTrackingConsistent (Positive line) (Positive col) msg =
  let pos = posAtLineCol line col
      error = errorAt pos msg
  in case errorLocation error of
       ErrorLocation span -> 
         sourceLine (spanStart span) == line && 
         sourceColumn (spanStart span) == col
       _ -> False

-- Helper wrapper for positive integers
newtype Positive a = Positive a
  deriving (Show, Eq)

instance (Arbitrary a, Num a, Ord a) => Arbitrary (Positive a) where
  arbitrary = Positive <$> choose (1, 20)

-- Helper function to create SourcePos
posAtLineCol :: Int -> Int -> SourcePos
posAtLineCol line col = SourcePos line col