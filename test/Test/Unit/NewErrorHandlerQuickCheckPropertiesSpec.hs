{-# LANGUAGE CPP #-}

module Test.Unit.NewErrorHandlerQuickCheckPropertiesSpec (tests) where

import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.QuickCheck (Property, (===), forAll, Gen, choose, listOf, elements, suchThat)

import TestSupport.QuickCheck (fastProperty)

import Compiler.Errors.Core
  ( TypeError(..)
  , ErrorSeverity(..)
  , ErrorCategory(..)
  , ErrorLocation(..)
  , ErrorContext(..)
  , ErrorRecovery(..)
  , emptyContext
  , errorAt
  , errorWithCategory
  , warningAt
  , infoAt
  , fatalError
  , errorWithSuggestions
  , withLocation
  , withContext
  , withSuggestions
  , withRelatedErrors
  , wrapError
  , hasCategory
  , filterByCategory
  , filterBySeverity
  , getErrorStatistics
  , formatError
  , formatErrorWithLocation
  , formatErrors
  , canRecoverFrom
  , shouldContinueAfter
  , severityPriority
  , isAtLeast
  , errorRecovery
  , fatalRecovery
  , warningRecovery
  , infoRecovery
  , customRecovery
  , ErrorLocation(..)
  , line
  , column
  , filePath
  , endLine
  , endColumn
  )

-- QuickCheck generators
genErrorSeverity :: Gen ErrorSeverity
genErrorSeverity = elements [Fatal, Error, Warning, Info]

genErrorCategory :: Gen ErrorCategory
genErrorCategory = elements [TypeChecking, Ownership, Parsing, Semantic, Runtime, Constraint, Inference, Integration, Unknown]

genErrorLocation :: Gen ErrorLocation
genErrorLocation = do
  lineNum <- choose (1, 1000)
  colNum <- choose (1, 200)
  endLineNum <- choose (lineNum, lineNum + 10)
  endColNum <- choose (colNum, colNum + 100)
  filePathStr <- elements [Nothing, Just "test.typus", Just "src/main.typus", Just "lib/utils.typus"]
  return $ ErrorLocation filePathStr lineNum colNum (Just endLineNum) (Just endColNum)

genErrorContext :: Gen ErrorContext
genErrorContext = do
  code <- elements [Nothing, Just "func test() {}", Just "x := 42", Just "return x + y"]
  func <- elements [Nothing, Just "main", Just "test", Just "helper"]
  var <- elements [Nothing, Just "x", Just "result", Just "data"]
  typ <- elements [Nothing, Just "int", Just "string", Just "bool"]
  additional <- listOf ((,) <$> elements ["key1", "key2", "key3"] <*> elements ["value1", "value2", "value3"])
  return $ ErrorContext code func var typ additional

genErrorRecovery :: Gen ErrorRecovery
genErrorRecovery = do
  canRec <- elements [True, False]
  shouldCont <- elements [True, False]
  action <- elements [Nothing, Just "retry", Just "skip", Just "fallback"]
  hint <- elements [Nothing, Just "check input", Just "try again", Just "use alternative"]
  cost <- choose (0, 100)
  confidence <- choose (0.0, 1.0)
  return $ RecoveryStrategy canRec shouldCont action hint cost confidence

genText :: Gen T.Text
genText = T.pack <$> listOf (elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " ")

genTypeError :: Gen TypeError
genTypeError = do
  errId <- elements ["ERR001", "ERR002", "ERR003", "PARSE_ERR", "TYPE_ERR"]
  sev <- genErrorSeverity
  cat <- genErrorCategory
  msg <- genText
  loc <- genErrorLocation
  ctx <- genErrorContext
  recovery <- genErrorRecovery
  suggestions <- listOf genText
  relatedErrors <- listOf genTypeError
  errorChain <- listOf genTypeError
  timestamp <- elements [Nothing, Just "2023-01-01 12:00:00", Just "2023-12-31 23:59:59"]
  
  return $ TypeError errId sev cat msg loc ctx recovery suggestions relatedErrors errorChain timestamp

-- | QuickCheck property tests for ErrorHandler module
tests :: TestTree
tests =
  testGroup "NewErrorHandler QuickCheck Properties"
    [ testGroup "Error creation properties"
        [ fastProperty "errorAt creates error with Error severity" $
            forAll genText $ \msg ->
              forAll genErrorLocation $ \loc ->
                let err = errorAt "TEST001" msg loc
                in severity err === Error &&
                   message err === msg &&
                   location err === loc

        , fastProperty "errorWithCategory preserves category" $
            forAll genText $ \msg ->
              forAll genErrorCategory $ \cat ->
                forAll genErrorLocation $ \loc ->
                  let err = errorWithCategory "TEST002" cat msg loc
                  in category err === cat

        , fastProperty "warningAt creates error with Warning severity" $
            forAll genText $ \msg ->
              forAll genErrorLocation $ \loc ->
                let err = warningAt "WARN001" msg loc
                in severity err === Warning &&
                   message err === msg &&
                   location err === loc

        , fastProperty "infoAt creates error with Info severity" $
            forAll genText $ \msg ->
              forAll genErrorLocation $ \loc ->
                let err = infoAt "INFO001" msg loc
                in severity err === Info &&
                   message err === msg &&
                   location err === loc

        , fastProperty "fatalError creates error with Fatal severity and fatal recovery" $
            forAll genText $ \msg ->
              forAll genErrorLocation $ \loc ->
                let err = fatalError "FATAL001" msg loc
                in severity err === Fatal &&
                   recovery err === fatalRecovery
        ]

    , testGroup "Error modification properties"
        [ fastProperty "withLocation updates location" $
            forAll genTypeError $ \err ->
              forAll genErrorLocation $ \newLoc ->
                let updatedErr = withLocation err newLoc
                in location updatedErr === newLoc &&
                   message updatedErr === message err &&
                   severity updatedErr === severity err

        , fastProperty "withContext updates context" $
            forAll genTypeError $ \err ->
              forAll genErrorContext $ \newCtx ->
                let updatedErr = withContext err newCtx
                in context updatedErr === newCtx &&
                   message updatedErr === message err &&
                   severity updatedErr === severity err

        , fastProperty "withSuggestions prepends suggestions" $
            forAll genTypeError $ \err ->
              forAll (listOf genText) $ \newSuggestions ->
                let updatedErr = withSuggestions newSuggestions err
                in take (length newSuggestions) (suggestions updatedErr) === newSuggestions &&
                   length (suggestions updatedErr) >= length (suggestions err)

        , fastProperty "wrapError adds to message and error chain" $
            forAll genTypeError $ \innerErr ->
              forAll genText $ \wrapperMsg ->
                let wrappedErr = wrapError wrapperMsg innerErr
                in message wrappedErr === wrapperMsg <> " " <> message innerErr &&
                   innerErr `elem` errorChain wrappedErr
        ]

    , testGroup "Error filtering properties"
        [ fastProperty "filterByCategory returns only errors with matching category" $
            forAll (listOf genTypeError) $ \errors ->
              forAll genErrorCategory $ \cat ->
                let filtered = filterByCategory cat errors
                in all (\e -> category e == cat) filtered

        , fastProperty "filterBySeverity returns only errors with matching severity" $
            forAll (listOf genTypeError) $ \errors ->
              forAll genErrorSeverity $ \sev ->
                let filtered = filterBySeverity sev errors
                in all (\e -> severity e == sev) filtered

        , fastProperty "hasCategory is equivalent to category check" $
            forAll genTypeError $ \err ->
              forAll genErrorCategory $ \cat ->
                hasCategory cat err === (category err == cat)
        ]

    , testGroup "Error statistics properties"
        [ fastProperty "getErrorStatistics total count matches input length" $
            forAll (listOf genTypeError) $ \errors ->
              let stats = getErrorStatistics errors
              in Map.lookup "total" stats === Just (length errors)

        , fastProperty "getErrorStatistics counts are non-negative" $
            forAll (listOf genTypeError) $ \errors ->
              let stats = getErrorStatistics errors
              in all (>= 0) (Map.elems stats)

        , fastProperty "getErrorStatistics severity counts sum to total" $
            forAll (listOf genTypeError) $ \errors ->
              let stats = getErrorStatistics errors
                  fatalCount = Map.findWithDefault 0 "fatal" stats
                  errorCount = Map.findWithDefault 0 "errors" stats
                  warningCount = Map.findWithDefault 0 "warnings" stats
                  infoCount = Map.findWithDefault 0 "info" stats
                  totalCount = Map.findWithDefault 0 "total" stats
              in fatalCount + errorCount + warningCount + infoCount === totalCount

        , fastProperty "getErrorStatistics category counts sum to total" $
            forAll (listOf genTypeError) $ \errors ->
              let stats = getErrorStatistics errors
                  categoryCounts = Map.filterWithKey (\k _ -> k `elem` 
                    ["typeChecking", "ownership", "parsing", "semantic", "runtime", 
                     "constraint", "inference", "integration", "unknown"]) stats
                  categorySum = sum categoryCounts
                  totalCount = Map.findWithDefault 0 "total" stats
              in categorySum === totalCount
        ]

    , testGroup "Error formatting properties"
        [ fastProperty "formatError includes severity string" $
            forAll genTypeError $ \err ->
              let formatted = formatError err
                  severityStr = case severity err of
                    Fatal -> "FATAL"
                    Error -> "ERROR"
                    Warning -> "WARNING"
                    Info -> "INFO"
              in severityStr `elem` words formatted

        , fastProperty "formatError includes category string" $
            forAll genTypeError $ \err ->
              let formatted = formatError err
                  categoryStr = "[" ++ show (category err) ++ "]"
              in categoryStr `isInfixOf` formatted

        , fastProperty "formatErrorWithLocation includes location information" $
            forAll genTypeError $ \err ->
              let formatted = formatErrorWithLocation err
                  locStr = show (line (location err)) ++ ":" ++ show (column (location err))
              in locStr `isInfixOf` formatted

        , fastProperty "formatErrors preserves all errors" $
            forAll (listOf genTypeError) $ \errors ->
              let formatted = formatErrors errors
                  formattedLines = lines formatted
              in length formattedLines >= length errors

        , fastProperty "formatErrors is idempotent for single error" $
            forAll genTypeError $ \err ->
              formatError err === formatErrors [err]
        ]

    , testGroup "Error recovery properties"
        [ fastProperty "canRecoverFrom matches recovery.canRecover" $
            forAll genTypeError $ \err ->
              canRecoverFrom err === canRecover (recovery err)

        , fastProperty "shouldContinueAfter matches recovery.shouldContinue" $
            forAll genTypeError $ \err ->
              shouldContinueAfter err === shouldContinue (recovery err)

        , fastProperty "fatalRecovery cannot recover and should not continue" $
            not (canRecover fatalRecovery) && not (shouldContinue fatalRecovery)

        , fastProperty "errorRecovery can recover and should continue" $
            canRecover errorRecovery && shouldContinue errorRecovery

        , fastProperty "warningRecovery can recover and should continue" $
            canRecover warningRecovery && shouldContinue warningRecovery

        , fastProperty "infoRecovery can recover and should continue" $
            canRecover infoRecovery && shouldContinue infoRecovery

        , fastProperty "customRecovery preserves provided parameters" $
            forAll (elements [True, False]) $ \canRec ->
              forAll (elements [True, False]) $ \shouldCont ->
                forAll (elements [Nothing, Just "action"]) $ \action ->
                  forAll (elements [Nothing, Just "hint"]) $ \hint ->
                    forAll (choose (0, 100)) $ \cost ->
                      forAll (choose (0.0, 1.0)) $ \confidence ->
                        let recovery = customRecovery canRec shouldCont action hint cost confidence
                        in canRecover recovery === canRec &&
                           shouldContinue recovery === shouldCont &&
                           recoveryAction recovery === action &&
                           recoveryHint recovery === hint &&
                           recoveryCost recovery === cost &&
                           recoveryConfidence recovery === confidence
        ]

    , testGroup "Severity properties"
        [ fastProperty "severityPriority is consistent with ordering" $
            forAll genErrorSeverity $ \sev1 ->
              forAll genErrorSeverity $ \sev2 ->
                if severityPriority sev1 > severityPriority sev2
                then sev1 > sev2
                else if severityPriority sev1 < severityPriority sev2
                then sev1 < sev2
                else sev1 === sev2

        , fastProperty "isAtLeast is reflexive" $
            forAll genErrorSeverity $ \sev ->
              isAtLeast sev sev

        , fastProperty "isAtLeast is transitive" $
            forAll genErrorSeverity $ \sev1 ->
              forAll genErrorSeverity $ \sev2 ->
                forAll genErrorSeverity $ \sev3 ->
                  if isAtLeast sev1 sev2 && isAtLeast sev2 sev3
                  then isAtLeast sev1 sev3
                  else True

        , fastProperty "Fatal is at least all severities" $
            forAll genErrorSeverity $ \sev ->
              isAtLeast Fatal sev

        , fastProperty "No severity is at least Fatal except Fatal itself" $
            forAll genErrorSeverity $ \sev ->
              if sev == Fatal
              then isAtLeast sev Fatal
              else not (isAtLeast sev Fatal)

        , fastProperty "severityPriority ordering: Fatal > Error > Warning > Info" $
            severityPriority Fatal > severityPriority Error &&
            severityPriority Error > severityPriority Warning &&
            severityPriority Warning > severityPriority Info
        ]

    , testGroup "Context and location properties"
        [ fastProperty "emptyContext has all fields as Nothing or empty" $
            context emptyContext === ErrorContext Nothing Nothing Nothing Nothing []

        , fastProperty "ErrorLocation line and column are positive" $
            forAll genErrorLocation $ \loc ->
              line loc > 0 && column loc > 0

        , fastProperty "ErrorLocation endLine and endColumn are >= start values" $
            forAll genErrorLocation $ \loc ->
              let startLine = line loc
                  startCol = column loc
              in case (endLine loc, endColumn loc) of
                   (Just endL, Just endC) -> endL >= startLine && endC >= startCol
                   _ -> True

        , fastProperty "filePath is either Nothing or non-empty string" $
            forAll genErrorLocation $ \loc ->
              case filePath loc of
                Just path -> not (null path)
                Nothing -> True
        ]

    , testGroup "Edge case properties"
        [ fastProperty "errorWithSuggestions with empty list preserves original suggestions" $
            forAll genTypeError $ \err ->
              let updatedErr = withSuggestions [] err
              in suggestions updatedErr === suggestions err

        , fastProperty "wrapError with empty wrapper adds only space" $
            forAll genTypeError $ \err ->
              let wrappedErr = wrapError "" err
              in message wrappedErr === " " <> message err &&
                 err `elem` errorChain wrappedErr

        , fastProperty "filterByCategory on empty list returns empty list" $
            forAll genErrorCategory $ \cat ->
              filterByCategory cat [] === []

        , fastProperty "filterBySeverity on empty list returns empty list" $
            forAll genErrorSeverity $ \sev ->
              filterBySeverity sev [] === []

        , fastProperty "getErrorStatistics on empty list has zero counts" $
            let stats = getErrorStatistics []
            in all (== 0) (Map.elems stats) &&
               Map.lookup "total" stats === Just 0
        ]
  ]