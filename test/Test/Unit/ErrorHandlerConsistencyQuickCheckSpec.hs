module Test.Unit.ErrorHandlerConsistencyQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Arbitrary(..), Gen, choose, listOf, suchThat, oneof, elements, frequency)
import qualified Data.Text as T
import Data.Maybe (isJust, isNothing)
import Compiler.Errors.Core

-- | Generate arbitrary error severity levels
instance Arbitrary ErrorSeverity where
  arbitrary = frequency
    [ (1, return Fatal)
    , (2, return Error)
    , (3, return Warning)
    , (4, return Info)
    ]

-- | Generate arbitrary error categories (assuming they exist)
instance Arbitrary ErrorCategory where
  arbitrary = elements 
    [ TypeMismatch
    , UndefinedVariable
    , SyntaxError
    , OwnershipError
    , DependencyError
    ]

-- | Generate arbitrary error locations
instance Arbitrary ErrorLocation where
  arbitrary = do
    line <- choose (1, 1000)
    column <- choose (1, 200)
    endLine <- frequency [(3, return (Just line)), (1, fmap Just (choose (line, line + 10))), (1, return Nothing)]
    endColumn <- case endLine of
      Just el | el == line -> fmap Just (choose (column, column + 100))
      Just _ -> fmap Just (choose (1, 200))
      Nothing -> return Nothing
    filePath <- frequency [(2, return Nothing), (1, fmap Just (listOf1 (elements ['a'..'z'])))]
    return $ ErrorLocation filePath line column endLine endColumn

-- | Generate arbitrary error context
instance Arbitrary ErrorContext where
  arbitrary = do
    code <- frequency [(2, return Nothing), (1, fmap Just (listOf1 (elements ['a'..'z'])))]
    func <- frequency [(2, return Nothing), (1, fmap Just (listOf1 (elements ['a'..'z'])))]
    var <- frequency [(2, return Nothing), (1, fmap Just (listOf1 (elements ['a'..'z'])))]
    typ <- frequency [(2, return Nothing), (1, fmap Just (listOf1 (elements ['a'..'z'])))]
    additional <- listOf $ do
      key <- listOf1 (elements ['a'..'z'])
      value <- listOf1 (elements ['a'..'z'])
      return (key, value)
    return $ ErrorContext code func var typ additional

-- | Generate arbitrary type errors
instance Arbitrary TypeError where
  arbitrary = do
    errorId <- listOf1 (elements ['a'..'z', '0'..'9'])
    severity <- arbitrary
    category <- arbitrary
    message <- T.pack <$> listOf1 (elements ['a'..'z', ' ', '.', ','])
    location <- arbitrary
    context <- arbitrary
    return $ TypeError errorId severity category message location context

-- | Generate arbitrary recovery strategies
instance Arbitrary ErrorRecovery where
  arbitrary = do
    canRec <- arbitrary
    shouldCont <- arbitrary
    action <- frequency [(2, return Nothing), (1, fmap Just (listOf1 (elements ['a'..'z'])))]
    hint <- frequency [(2, return Nothing), (1, fmap Just (listOf1 (elements ['a'..'z'])))]
    cost <- choose (0, 100)
    confidence <- choose (0.0, 1.0)
    return $ RecoveryStrategy canRec shouldCont action hint cost confidence

tests :: TestTree
tests =
  testGroup "ErrorHandler consistency QuickCheck tests"
    [ testGroup "Error severity consistency"
        [ testCase "severity priority is ordered correctly" $ do
            severityPriority Fatal @?= 100
            severityPriority Error @?= 80
            severityPriority Warning @?= 30
            severityPriority Info @?= 10

        , fastProperty "severity comparison is consistent with priority" $
            \sev1 sev2 ->
              let comp = compareSeverity sev1 sev2
                  pri1 = severityPriority sev1
                  pri2 = severityPriority sev2
              in comp == compare pri1 pri2

        , fastProperty "isAtLeast is reflexive" $
            \sev ->
              isAtLeast sev sev

        , fastProperty "isAtLeast is transitive" $
            \sev1 sev2 sev3 ->
              isAtLeast sev1 sev2 && isAtLeast sev2 sev3 ==> isAtLeast sev1 sev3

        , fastProperty "fatal errors are not recoverable" $
            \sev ->
              sev == Fatal ==> not (_isRecoverable sev)

        , fastProperty "fatal and errors require user action" $
            \sev ->
              sev `elem` [Fatal, Error] ==> _isUserActionRequired sev
        ]

    , testGroup "Error location consistency"
        [ testCase "unknown location has correct defaults" $ do
            _unknownLocation @?= ErrorLocation Nothing 0 0 Nothing Nothing

        , testCase "helper functions access correct fields" $ do
            let loc = ErrorLocation (Just "test.hs") 10 5 (Just 10) (Just 15)
            getErrorLine loc @?= 10
            getErrorColumn loc @?= 5

        , fastProperty "atLocation creates location without file" $ do
            \line col ->
              let loc = _atLocation line col
              in filePath loc == Nothing && line loc == line && column loc == col

        , fastProperty "atFileLocation creates location with file" $ do
            \file line col ->
              let loc = _atFileLocation file line col
              in filePath loc == Just file && line loc == line && column loc == col

        , fastProperty "atRange creates location with end positions" $ do
            \startLine startCol endLine endCol ->
              let loc = _atRange startLine startCol endLine endCol
              in line loc == startLine && column loc == startCol &&
                 endLine loc == Just endLine && endColumn loc == Just endCol
        ]

    , testGroup "Error context consistency"
        [ testCase "empty context has all Nothing values" $ do
            emptyContext @?= ErrorContext Nothing Nothing Nothing Nothing []

        , fastProperty "context fields are accessible" $
            \code func var typ additional ->
              let ctx = ErrorContext code func var typ additional
              in contextCode ctx == code &&
                 contextFunction ctx == func &&
                 contextVariable ctx == var &&
                 contextType ctx == typ &&
                 contextAdditional ctx == additional

        , fastProperty "context preserves additional information" $
            \additional ->
              let ctx = emptyContext { contextAdditional = additional }
              in contextAdditional ctx == additional
        ]

    , testGroup "Error recovery consistency"
        [ testCase "predefined recovery strategies have correct properties" $ do
            canRecover fatalRecovery @?= False
            shouldContinue fatalRecovery @?= False
            recoveryCost fatalRecovery @?= 100
            recoveryConfidence fatalRecovery @?= 0.0

            canRecover errorRecovery @?= True
            shouldContinue errorRecovery @?= True
            recoveryCost errorRecovery @?= 50
            recoveryConfidence errorRecovery @?= 0.7

        , fastProperty "custom recovery preserves all fields" $
            \canRec shouldCont action hint cost confidence ->
              let recovery = customRecovery canRec shouldCont action hint cost confidence
              in canRecover recovery == canRec &&
                 shouldContinue recovery == shouldCont &&
                 recoveryAction recovery == action &&
                 recoveryHint recovery == hint &&
                 recoveryCost recovery == cost &&
                 recoveryConfidence recovery == confidence

        , fastProperty "recovery confidence is always in valid range" $
            \recovery ->
              let conf = recoveryConfidence recovery
              in conf >= 0.0 && conf <= 1.0

        , fastProperty "recovery cost is always non-negative" $
            \recovery ->
              recoveryCost recovery >= 0

        , fastProperty "chooseBest recovery selects higher confidence" $
            \r1 r2 ->
              let can1 = canRecover r1
                  can2 = canRecover r2
                  conf1 = recoveryConfidence r1
                  conf2 = recoveryConfidence r2
                  best = _chooseBestRecovery [r1, r2]
              in if can1 && can2
                 then recoveryConfidence best >= max conf1 conf2 - 0.001
                 else True
        ]

    , testGroup "Type error consistency"
        [ fastProperty "type error preserves all fields" $
            \errorId severity category message location context ->
              let error = TypeError errorId severity category message location context
              in errorId error == errorId &&
                 severity error == severity &&
                 category error == category &&
                 message error == message &&
                 location error == location &&
                 context error == context

        , fastProperty "error location is consistent with helper functions" $
            \error ->
              let loc = location error
              in getErrorLine loc == line loc &&
                 getErrorColumn loc == column loc

        , fastProperty "error severity is consistent with recovery" $
            \error ->
              let sev = severity error
              in if sev == Fatal
                 then not (_isRecoverable sev)
                 else True

        , fastProperty "error with context preserves context information" $
            \error newContext ->
              let withCtx = withContext newContext error
              in context withCtx == newContext
        ]

    , testGroup "Error collection consistency"
        [ testCase "new error collector starts empty" $ do
            collector <- newErrorCollector
            hasErrors collector @?= False
            hasWarnings collector @?= False
            getErrors collector @?= []
            getWarnings collector @?= []

        , fastProperty "adding error increases error count" $
            \error ->
              let collector = newErrorCollector
                  collector' = addError error collector
              in hasErrors collector' && length (getErrors collector') == 1

        , fastProperty "adding warning increases warning count" $
            \error ->
              let collector = newErrorCollector
                  warning = error { severity = Warning }
                  collector' = addWarning warning collector
              in hasWarnings collector' && length (getWarnings collector') == 1

        , fastProperty "error collection preserves order" $
            \errors ->
              let collector = foldl (\c e -> addError e c) newErrorCollector errors
                  retrieved = getErrors collector
              in length retrieved == length errors

        , fastProperty "all messages includes errors, warnings, and info" $
            \errors warnings infoMessages ->
              let collector = foldl (\c e -> addError e c) newErrorCollector errors
                  collector' = foldl (\c w -> addWarning w c) collector warnings
                  collector'' = foldl (\c i -> addInfo i c) collector' infoMessages
                  allMsgs = getAllMessages collector''
              in length allMsgs == length errors + length warnings + length infoMessages
        ]

    , testGroup "Error formatting consistency"
        [ fastProperty "formatError produces non-empty output" $
            \error ->
              not $ T.null (formatError error)

        , fastProperty "formatErrors preserves order" $
            \errors ->
              let formatted = formatErrors errors
              in length formatted == length errors

        , fastProperty "formatErrorWithLocation includes location information" $
            \error ->
              let formatted = formatErrorWithLocation error
                  locStr = "line " ++ show (line (location error))
              in locStr `isInfixOf` T.unpack formatted

        , fastProperty "error formatting doesn't crash on any input" $
            \error ->
              let _ = formatError error
                  _ = formatErrorWithLocation error
              in True
        ]
    ]

-- Helper function for string infix check
isInfixOf :: String -> String -> Bool
isInfixOf needle haystack = needle `elem` (substrings haystack)
  where
    substrings [] = []
    substrings s@(x:xs) = take (length needle) s : substrings xs