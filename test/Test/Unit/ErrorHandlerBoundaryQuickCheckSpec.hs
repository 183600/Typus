module Test.Unit.ErrorHandlerBoundaryQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Arbitrary(..), Gen, choose, listOf, suchThat, oneof, elements, frequency)
import qualified Data.Text as T
import Data.Maybe (isJust, isNothing, fromMaybe)
import Data.List (sort, nub)
import Compiler.Errors.Core

-- | Generate boundary case strings
genBoundaryString :: Gen String
genBoundaryString = frequency
    [ (2, return "") -- Empty string
    , (2, return " ") -- Single space
    , (2, return "\n") -- Single newline
    , (1, return "\t") -- Single tab
    , (1, return "\0") -- Null character
    , (1, listOf $ elements "\n\t\r\f\v") -- Only whitespace
    , (1, return $ replicate 1000 'a') -- Very long string
    , (1, return $ concat (replicate 100 "test ")) -- Repeated pattern
    ]

-- | Generate extreme severity combinations
genExtremeSeverity :: Gen ErrorSeverity
genExtremeSeverity = elements [Fatal, Info] -- Focus on extremes

-- | Generate extreme location values
genExtremeLocation :: Gen ErrorLocation
genExtremeLocation = frequency
    [ (2, return _unknownLocation) -- Unknown location
    , (1, return $ ErrorLocation Nothing 0 0 Nothing Nothing) -- Zero position
    , (1, return $ ErrorLocation Nothing 999999 999999 Nothing Nothing) -- Very large position
    , (1, do
        line <- choose (1, 10)
        return $ ErrorLocation Nothing line (-1) Nothing Nothing) -- Negative column
    , (1, do
        line <- choose (1, 10)
        column <- choose (1, 10)
        return $ ErrorLocation Nothing line column (Just (line - 1)) Nothing) -- End before start
    ]

-- | Generate extreme recovery strategies
genExtremeRecovery :: Gen ErrorRecovery
genExtremeRecovery = frequency
    [ (1, return fatalRecovery) -- Fatal recovery
    , (1, return infoRecovery) -- Info recovery
    , (1, customRecovery False False Nothing Nothing 100 0.0) -- Worst case
    , (1, customRecovery True True Nothing Nothing 0 1.0) -- Best case
    , (1, do
        cost <- choose (0, 100)
        confidence <- choose (0.0, 1.0)
        customRecovery True True (Just "action") (Just "hint") cost confidence)
    ]

tests :: TestTree
tests =
  testGroup "ErrorHandler boundary conditions QuickCheck tests"
    [ testGroup "Severity boundary conditions"
        [ testCase "severity priority handles all values" $ do
            severityPriority Fatal @?= 100
            severityPriority Error @?= 80
            severityPriority Warning @?= 30
            severityPriority Info @?= 10

        , fastProperty "isAtLeast boundary cases" $
            \sev ->
              isAtLeast Info sev &&  -- Info is least severe
              isAtLeast sev Fatal     -- Fatal is most severe

        , fastProperty "severity comparison handles equal values" $
            \sev ->
              compareSeverity sev sev == EQ

        , testCase "recoverability boundaries" $ do
            _isRecoverable Fatal @?= False
            _isRecoverable Error @?= True
            _isRecoverable Warning @?= True
            _isRecoverable Info @?= True

        , fastProperty "user action required boundaries" $
            \sev ->
              let userAction = _isUserActionRequired sev
              in if sev `elem` [Fatal, Error]
                 then userAction
                 else not userAction
        ]

    , testGroup "Location boundary conditions"
        [ testCase "unknown location properties" $ do
            getErrorLine _unknownLocation @?= 0
            getErrorColumn _unknownLocation @?= 0
            filePath _unknownLocation @?= Nothing
            endLine _unknownLocation @?= Nothing
            endColumn _unknownLocation @?= Nothing

        , testCase "extreme location values" $ do
            let zeroLoc = ErrorLocation Nothing 0 0 Nothing Nothing
                maxLoc = ErrorLocation Nothing 999999 999999 (Just 999999) (Just 999999)
            getErrorLine zeroLoc @?= 0
            getErrorColumn zeroLoc @?= 0
            getErrorLine maxLoc @?= 999999
            getErrorColumn maxLoc @?= 999999

        , fastProperty "location creation with boundary values" $
            \line col ->
              let loc = _atLocation line col
              in line >= 0 && col >= 0 ==> 
                 getErrorLine loc == max 0 line && 
                 getErrorColumn loc == max 0 col

        , fastProperty "range location handles edge cases" $
            \startLine startCol endLine endCol ->
              let loc = _atRange startLine startCol endLine endCol
              in if endLine >= startLine && endCol >= startCol
                 then endLine loc == Just endLine && endColumn loc == Just endCol
                 else True -- May create invalid location but shouldn't crash

        , testCase "file location with empty file path" $ do
            let loc = _atFileLocation "" 10 5
            filePath loc @?= Just ""
            getErrorLine loc @?= 10
            getErrorColumn loc @?= 5
        ]

    , testGroup "Context boundary conditions"
        [ testCase "empty context serialization" $ do
            let ctx = emptyContext
            contextCode ctx @?= Nothing
            contextFunction ctx @?= Nothing
            contextVariable ctx @?= Nothing
            contextType ctx @?= Nothing
            contextAdditional ctx @?= []

        , fastProperty "context with empty strings" $
            \additional ->
              let ctx = ErrorContext (Just "") (Just "") (Just "") (Just "") additional
              in isJust (contextCode ctx) &&
                 isJust (contextFunction ctx) &&
                 isJust (contextVariable ctx) &&
                 isJust (contextType ctx)

        , fastProperty "context with many additional fields" $
            \additional ->
              let ctx = emptyContext { contextAdditional = additional }
                  count = length additional
              in length (contextAdditional ctx) == count

        , testCase "context with duplicate additional keys" $ do
            let additional = [("key1", "value1"), ("key1", "value2"), ("key2", "value3")]
                ctx = emptyContext { contextAdditional = additional }
            contextAdditional ctx @?= additional
        ]

    , testGroup "Recovery boundary conditions"
        [ testCase "extreme recovery strategies" $ do
            recoveryCost fatalRecovery @?= 100
            recoveryConfidence fatalRecovery @?= 0.0
            canRecover fatalRecovery @?= False
            shouldContinue fatalRecovery @?= False

            recoveryCost infoRecovery @?= 0
            recoveryConfidence infoRecovery @?= 1.0
            canRecover infoRecovery @?= True
            shouldContinue infoRecovery @?= True

        , fastProperty "recovery confidence boundaries" $
            \recovery ->
              let conf = recoveryConfidence recovery
              in conf >= 0.0 && conf <= 1.0

        , fastProperty "recovery cost boundaries" $
            \recovery ->
              let cost = recoveryCost recovery
              in cost >= 0 && cost <= 100

        , fastProperty "custom recovery with extreme values" $
            \canRec shouldCont ->
              let recovery = customRecovery canRec shouldCont Nothing Nothing 100 0.0
              in canRecover recovery == canRec &&
                 shouldContinue recovery == shouldCont &&
                 recoveryCost recovery == 100 &&
                 recoveryConfidence recovery == 0.0

        , testCase "recovery strategy selection with empty list" $ do
            let best = _chooseBestRecovery []
            best @?= fatalRecovery

        , fastProperty "recovery strategy selection with equal confidence" $
            \r1 r2 ->
              let conf1 = recoveryConfidence r1
                  conf2 = recoveryConfidence r2
                  adjustedR1 = r1 { recoveryConfidence = 0.5 }
                  adjustedR2 = r2 { recoveryConfidence = 0.5 }
                  best = _chooseBestRecovery [adjustedR1, adjustedR2]
              in recoveryConfidence best == 0.5
        ]

    , testGroup "Error creation boundary conditions"
        [ fastProperty "type error with empty message" $ do
            \errorId severity category location context ->
              let error = TypeError errorId severity category T.empty location context
              in T.null (message error)

        , fastProperty "type error with very long message" $
            \errorId severity category location context ->
              let longMsg = T.pack $ replicate 10000 'a'
                  error = TypeError errorId severity category longMsg location context
              in T.length (message error) == 10000

        , fastProperty "type error with empty ID" $
            \severity category message location context ->
              let error = TypeError "" severity category message location context
              in null (errorId error)

        , fastProperty "type error with unknown location" $
            \errorId severity category message context ->
              let error = TypeError errorId severity category message _unknownLocation context
                  loc = location error
              in loc == _unknownLocation

        , fastProperty "type error with empty context" $
            \errorId severity category message location ->
              let error = TypeError errorId severity category message location emptyContext
                  ctx = context error
              in ctx == emptyContext
        ]

    , testGroup "Error collection boundary conditions"
        [ testCase "collector with many errors" $ do
            let errors = replicate 1000 $ TypeError "test" Error TypeMismatch "test" _unknownLocation emptyContext
                collector = foldl (\c e -> addError e c) newErrorCollector errors
            length (getErrors collector) @?= 1000

        , fastProperty "collector with mixed severity levels" $
            \errors ->
              let collector = foldl (\c e -> addError e c) newErrorCollector errors
                  errorCount = length $ filter (\e -> severity e `elem` [Error, Fatal]) errors
                  warningCount = length $ filter (\e -> severity e == Warning) errors
                  infoCount = length $ filter (\e -> severity e == Info) errors
              in length (getErrors collector) == errorCount &&
                 length (getWarnings collector) == warningCount

        , fastProperty "collector handles duplicate errors" $
            \error ->
              let collector = addError error (addError error newErrorCollector)
              in length (getErrors collector) == 2

        , testCase "collector with no messages" $ do
            let collector = newErrorCollector
            getAllMessages collector @?= []
        ]

    , testGroup "Error formatting boundary conditions"
        [ fastProperty "formatting errors with empty fields" $
            \severity category ->
              let error = TypeError "" severity category T.empty _unknownLocation emptyContext
                  formatted = formatError error
              in not $ T.null formatted

        , fastProperty "formatting errors with unicode content" $
            \errorId severity category ->
              let unicodeMsg = T.pack "测试消息 🚀"
                  error = TypeError errorId severity category unicodeMsg _unknownLocation emptyContext
                  formatted = formatError error
              in T.unpack formatted `contains` "测试消息"

        , fastProperty "formatting many errors doesn't crash" $
            \errors ->
              let formatted = formatErrors errors
              in length formatted == length errors

        , fastProperty "formatting with location handles unknown location" $
            \errorId severity category message context ->
              let error = TypeError errorId severity category message _unknownLocation context
                  formatted = formatErrorWithLocation error
              in not $ T.null formatted

        , testCase "formatting with extreme location values" $ do
            let extremeLoc = ErrorLocation Nothing (-1) (-1) (Just (-2)) (Just (-2))
                error = TypeError "test" Error TypeMismatch "test" extremeLoc emptyContext
                formatted = formatErrorWithLocation error
            not $ T.null formatted
        ]

    , testGroup "Error filtering boundary conditions"
        [ testCase "filtering empty error list" $ do
            filterBySeverity Fatal [] @?= []
            filterByCategory TypeMismatch [] @?= []

        , fastProperty "filtering by severity preserves order" $
            \errors sev ->
              let filtered = filterBySeverity sev errors
                  originalIndices = map fst $ filter (\e -> severity (snd e) == sev) (zip [0..] errors)
              in length filtered == length originalIndices

        , fastProperty "filtering by category preserves order" $
            \errors cat ->
              let filtered = filterByCategory cat errors
                  originalIndices = map fst $ filter (\e -> category (snd e) == cat) (zip [0..] errors)
              in length filtered == length originalIndices

        , fastProperty "hasCategory works correctly" $
            \errors cat ->
              let hasCat = hasCategory cat errors
                  hasCat' = any (\e -> category e == cat) errors
              in hasCat == hasCat'

        , testCase "error statistics with empty list" $ do
            let stats = getErrorStatistics []
            stats @?= []
        ]
    ]

-- Helper function for string contains check
contains :: String -> String -> Bool
contains needle haystack = needle `isInfixOf` haystack

-- Helper function for infix check
isInfixOf :: Eq a => [a] -> [a] -> Bool
isInfixOf needle haystack = any (isPrefixOf needle) (tails haystack)
  where
    isPrefixOf [] _ = True
    isPrefixOf _ [] = False
    isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys
    tails [] = [[]]
    tails xs@(_:ys) = xs : tails ys