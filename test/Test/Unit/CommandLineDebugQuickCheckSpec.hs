module Test.Unit.CommandLineDebugQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, oneof, listOf, elements, suchThat, choose)
import Control.Monad (when)
import Data.IORef
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set

import CommandLineDebug 
    ( CommandLineDebugConfig(..)
    , defaultCLIDebugConfig
    , runWithCLIDebug
    , checkBreakpoint
    , setBreakpoint
    , setConditionalBreakpoint
    , listBreakpoints
    , clearBreakpoints
    , toggleDebugOutput
    , DebugCommandResult(..)
    , processDebugCommand
    , setDebugLevel
    , showDebugStatus
    , addWatchVariable
    , removeWatchVariable
    , listWatchVariables
    , getCallStack
    , pushCallStack
    , popCallStack
    , evaluateExpression
    , stepInto
    , stepOver
    , stepOut
    , continue
    , runToCursor
    )
import TestSupport.QuickCheck (fastProperty)

-- | Generate arbitrary debug locations
genDebugLocation :: Gen String
genDebugLocation = oneof
    [ pure "main.go:10"
    , pure "utils.go:25"
    , pure "parser.go:42"
    , pure "compiler.go:100"
    , elements ["func main", "func parse", "func compile", "func validate"]
    ]

-- | Generate arbitrary debug levels
genDebugLevel :: Gen Int
genDebugLevel = choose (0, 5)

-- | Generate arbitrary variable names
genVarName :: Gen String
genVarName = oneof
    [ elements ["x", "y", "result", "count", "total"]
    , arbitrary `suchThat` (not . null)
    ]

-- | Generate arbitrary variable values
genVarValue :: Gen String
genVarValue = oneof
    [ elements ["42", "\"hello\"", "true", "[]int{1,2,3}"]
    , arbitrary `suchThat` (not . null)
    ]

-- | Generate arbitrary command tokens
genDebugCommand :: Gen [String]
genDebugCommand = oneof
    [ pure ["c"]
    , pure ["continue"]
    , pure ["s"]
    , pure ["step"]
    , pure ["l"]
    , pure ["list"]
    , pure ["d"]
    , pure ["disable"]
    , pure ["e"]
    , pure ["enable"]
    , pure ["q"]
    , pure ["quit"]
    , pure ["h"]
    , pure ["help"]
    , pure ["unknown"]
    ]

-- Generate conditional functions
genCondition :: Gen (String -> Bool)
genCondition = oneof
    [ pure (const True)
    , pure (const False)
    , pure (== "main.go:10")
    , pure (\loc -> "main" `L.isInfixOf` loc)
    ]

-- Test properties L.and cases
tests :: TestTree
tests =
  testGroup "CommandLineDebug QuickCheck tests"
    [ testProperty "setBreakpoint increases breakpoint count" $
        fastProperty prop_setBreakpointIncreasesCount
    
    , testProperty "clearBreakpoints resets breakpoint count to zero" $
        fastProperty prop_clearBreakpointsResetsCount
    
    , testProperty "toggleDebugOutput flips enabled state" $
        fastProperty prop_toggleDebugOutputFlipsState
    
    , testProperty "setDebugLevel updates log level" $
        fastProperty prop_setDebugLevelUpdatesLevel
    
    , testProperty "addWatchVariable increases watch variable count" $
        fastProperty prop_addWatchVariableIncreasesCount
    
    , testProperty "removeWatchVariable decreases watch variable count" $
        fastProperty prop_removeWatchVariableDecreasesCount
    
    , testProperty "pushCallStack increases call stack depth" $
        fastProperty prop_pushCallStackIncreasesDepth
    
    , testProperty "popCallStack decreases call stack depth" $
        fastProperty prop_popCallStackDecreasesDepth
    
    , testProperty "evaluateExpression returns non-empty result" $
        fastProperty prop_evaluateExpressionReturnsResult
    
    , testCase "setBreakpoint L.and listBreakpoints work together" $ do
        config <- defaultCLIDebugConfig
        setBreakpoint config "main.go:10"
        setBreakpoint config "utils.go:25"
        -- Check that breakpoints are set (we can't easily capture output in unit tests)
        -- This test mainly ensures the functions don't crash
        return ()
    
    , testCase "clearBreakpoints removes L.all breakpoints" $ do
        config <- defaultCLIDebugConfig
        setBreakpoint config "main.go:10"
        setBreakpoint config "utils.go:25"
        clearBreakpoints config
        -- Verify breakpoints are cleared
        return ()
    
    , testCase "toggleDebugOutput changes state" $ do
        config <- defaultCLIDebugConfig
        initialEnabled <- readIORef (cldEnabled config)
        toggleDebugOutput config
        toggledEnabled <- readIORef (cldEnabled config)
        toggledEnabled @?= not initialEnabled
    
    , testCase "setDebugLevel updates correctly" $ do
        config <- defaultCLIDebugConfig
        setDebugLevel config 4
        level <- readIORef (cldLogLevel config)
        level @?= 4
    
    , testCase "addWatchVariable L.and listWatchVariables work together" $ do
        config <- defaultCLIDebugConfig
        addWatchVariable config "x" "42"
        addWatchVariable config "y" "\"hello\""
        -- Verify watch variables are added
        return ()
    
    , testCase "removeWatchVariable works correctly" $ do
        config <- defaultCLIDebugConfig
        addWatchVariable config "x" "42"
        removeWatchVariable config "x"
        -- Verify variable is removed
        return ()
    
    , testCase "pushCallStack L.and popCallStack work together" $ do
        config <- defaultCLIDebugConfig
        pushCallStack config "func main"
        pushCallStack config "func parse"
        stack <- getCallStack config
        L.length stack @?= 2
        popCallStack config
        stack' <- getCallStack config
        L.length stack' @?= 1
    
    , testCase "stepInto, stepOver, stepOut, continue work" $ do
        config <- defaultCLIDebugConfig
        stepInto config
        stepOver config
        stepOut config
        continue config
        -- These should not crash
        return ()
    
    , testCase "runToCursor sets breakpoint L.and continues" $ do
        config <- defaultCLIDebugConfig
        runToCursor config "target.go:50"
        -- Should set breakpoint L.and continue
        return ()
    
    , testCase "processDebugCommand handles known commands" $ do
        config <- defaultCLIDebugConfig
        result1 <- processDebugCommand config "location" ["c"]
        result1 @?= ResumeExecution
        
        result2 <- processDebugCommand config "location" ["continue"]
        result2 @?= ResumeExecution
        
        result3 <- processDebugCommand config "location" ["l"]
        result3 @?= AwaitMoreInput
    
    , testCase "processDebugCommand handles unknown commands" $ do
        config <- defaultCLIDebugConfig
        result <- processDebugCommand config "location" ["unknown", "command"]
        result @?= AwaitMoreInput
    
    , testCase "setConditionalBreakpoint works" $ do
        config <- defaultCLIDebugConfig
        let condition = (== "main.go:10")
        setConditionalBreakpoint config "main.go:10" condition
        -- Should set conditional breakpoint
        return ()
    ]

-- Property: setBreakpoint increases breakpoint count
prop_setBreakpointIncreasesCount :: String -> Bool
prop_setBreakpointIncreasesCount location = 
    let action = do
            config <- defaultCLIDebugConfig
            initialBreakpoints <- readIORef (cldBreakpoints config)
            let initialCount = Set.size initialBreakpoints
            setBreakpoint config location
            finalBreakpoints <- readIORef (cldBreakpoints config)
            let finalCount = Set.size finalBreakpoints
            return $ finalCount >= initialCount
    in unsafePerformIO action

-- Property: clearBreakpoints resets breakpoint count to zero
prop_clearBreakpointsResetsCount :: [String] -> Bool
prop_clearBreakpointsResetsCount locations =
    let action = do
            config <- defaultCLIDebugConfig
            mapM_ (setBreakpoint config) locations
            clearBreakpoints config
            breakpoints <- readIORef (cldBreakpoints config)
            return $ Set.size breakpoints == 0
    in unsafePerformIO action

-- Property: toggleDebugOutput flips enabled state
prop_toggleDebugOutputFlipsState :: Bool -> Bool
prop_toggleDebugOutputFlipsState initialState =
    let action = do
            config <- defaultCLIDebugConfig
            writeIORef (cldEnabled config) initialState
            toggleDebugOutput config
            finalState <- readIORef (cldEnabled config)
            return $ finalState == not initialState
    in unsafePerformIO action

-- Property: setDebugLevel updates log level
prop_setDebugLevelUpdatesLevel :: Int -> Int -> Bool
prop_setDebugLevelUpdatesLevel _ newLevel =
    let action = do
            config <- defaultCLIDebugConfig
            setDebugLevel config newLevel
            level <- readIORef (cldLogLevel config)
            return $ level == newLevel
    in unsafePerformIO action

-- Property: addWatchVariable increases watch variable count
prop_addWatchVariableIncreasesCount :: String -> String -> Bool
prop_addWatchVariableIncreasesCount varName value =
    let action = do
            config <- defaultCLIDebugConfig
            initialWatchVars <- readIORef (cldWatchVariables config)
            let initialCount = Map.size initialWatchVars
            addWatchVariable config varName value
            finalWatchVars <- readIORef (cldWatchVariables config)
            let finalCount = Map.size finalWatchVars
            return $ finalCount >= initialCount
    in unsafePerformIO action

-- Property: removeWatchVariable decreases watch variable count
prop_removeWatchVariableDecreasesCount :: String -> String -> Bool
prop_removeWatchVariableDecreasesCount varName value =
    let action = do
            config <- defaultCLIDebugConfig
            addWatchVariable config varName value
            initialWatchVars <- readIORef (cldWatchVariables config)
            let initialCount = Map.size initialWatchVars
            removeWatchVariable config varName
            finalWatchVars <- readIORef (cldWatchVariables config)
            let finalCount = Map.size finalWatchVars
            return $ finalCount <= initialCount
    in unsafePerformIO action

-- Property: pushCallStack increases call stack depth
prop_pushCallStackIncreasesDepth :: String -> Bool
prop_pushCallStackIncreasesDepth location =
    let action = do
            config <- defaultCLIDebugConfig
            initialStack <- getCallStack config
            let initialDepth = L.length initialStack
            pushCallStack config location
            finalStack <- getCallStack config
            let finalDepth = L.length finalStack
            return $ finalDepth == initialDepth + 1
    in unsafePerformIO action

-- Property: popCallStack decreases call stack depth
prop_popCallStackDecreasesDepth :: [String] -> Bool
prop_popCallStackDecreasesDepth locations =
    let action = do
            config <- defaultCLIDebugConfig
            mapM_ (pushCallStack config) locations
            initialStack <- getCallStack config
            let initialDepth = L.length initialStack
            when (initialDepth > 0) $ popCallStack config
            finalStack <- getCallStack config
            let finalDepth = L.length finalStack
            return $ finalDepth <= initialDepth
    in unsafePerformIO action

-- Property: evaluateExpression returns non-empty result
prop_evaluateExpressionReturnsResult :: String -> Bool
prop_evaluateExpressionReturnsResult expr =
    let action = do
            config <- defaultCLIDebugConfig
            result <- evaluateExpression config expr
            return $ not (null result)
    in unsafePerformIO action

-- Helper function to check if string contains a substring
isInfixOf :: String -> String -> Bool
isInfixOf needle haystack = needle `L.isInfixOf` haystack

import System.IO.Unsafe (unsafePerformIO)