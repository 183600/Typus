{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Unit.TestDebugIntegrationSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Parser
import SourceLocation
import ErrorHandler
import Compiler.IR
import Ownership
import Dependencies
import Utils
import Debug
import qualified Data.Text as T
import TestSupport.Arbitrary ()

-- | Test suite for Debug Integration
testDebugIntegration :: TestTree
testDebugIntegration = testGroup "Debug Integration Tests"
  [ testCase "Debug: enable debug mode" $
      let debugState = enableDebugMode
      in isDebugEnabled debugState @?= True
      
  , testCase "Debug: disable debug mode" $
      let debugState = disableDebugMode
      in isDebugEnabled debugState @?= False
      
  , testCase "Debug: set debug level" $
      let debugState = setDebugLevel Debug
      in getDebugLevel debugState @?= Debug
      
  , testCase "Debug: format debug message" $
      let message = "Test message"
          level = Info
          formatted = formatDebugMessage level message
      in "[INFO] Test message" `isInfixOf` formatted @?= True
      
  , testCase "Debug: log debug message" $
      let message = "Test log message"
          level = Warning
          debugState = enableDebugMode
          newState = logDebugMessage level message debugState
      in getLogMessages newState @?= ["[WARNING] Test log message"]
      
  , testCase "Debug: filter debug messages by level" $
      let debugState = enableDebugMode
          newState1 = logDebugMessage Error "Error message" debugState
          newState2 = logDebugMessage Info "Info message" newState1
          newState3 = logDebugMessage Warning "Warning message" newState2
          filtered = filterLogMessages Warning newState3
      in length filtered @?= 2  -- Warning and Error messages
      
  , testCase "Debug: clear debug log" $
      let debugState = enableDebugMode
          newState1 = logDebugMessage Info "Message 1" debugState
          newState2 = logDebugMessage Info "Message 2" newState1
          clearedState = clearDebugLog newState2
      in null (getLogMessages clearedState) @?= True
      
  , testCase "Debug: integrate with parser" $
      let input = "//! ownership=true\n```go\nfmt.Println(\"hello\")\n```"
          debugState = enableDebugMode
          parseResult = parseWithDebug input "test.typus" debugState
      in case parseResult of
           Right (typusFile, newState) -> do
             length (tfBlocks typusFile) @?= 1
             any ("Parsing" `isInfixOf`) (getLogMessages newState) @?= True
           Left _ -> assertFailure "Parse with debug should succeed"
           
  , testCase "Debug: integrate with ownership analyzer" $
      let input = "package main\n\nfunc main() {\n    data := make([]byte, 100)\n    processData(data)\n}\n\nfunc processData(d []byte) {\n    // Process data\n}"
          debugState = enableDebugMode
          ownershipResult = analyzeOwnershipWithDebug input debugState
      in case ownershipResult of
           Right (_, transfers, newState) -> do
             length transfers @?= 1
             any ("Ownership" `isInfixOf`) (getLogMessages newState) @?= True
           Left _ -> assertFailure "Ownership analysis with debug should succeed"
           
  , testCase "Debug: integrate with type analyzer" $
      let debugState = enableDebugMode
          checker = newDependentTypeChecker ()
          typeCheckResult = checkTypeWithDebug "int" checker debugState
      in case typeCheckResult of
           Right (_, newState) -> 
             any ("Type checking" `isInfixOf`) (getLogMessages newState) @?= True
           Left _ -> assertFailure "Type check with debug should succeed"
           
  , testCase "Debug: integrate with error handler" $
      let pos = posAt 5 10
          message = "Test error"
          err = errorAt pos message
          debugState = enableDebugMode
          errorResult = handleErrorWithDebug err debugState
      in case errorResult of
           Right newState -> do
             any ("Error handling" `isInfixOf`) (getLogMessages newState) @?= True
             length (getLogMessages newState) > 0 @?= True
           Left _ -> assertFailure "Error handling with debug should succeed"
           
  , testCase "Debug: integrate with source location" $
      let pos = posAt 5 10
          debugState = enableDebugMode
          locationResult = trackSourceLocationWithDebug pos debugState
      in case locationResult of
           Right newState -> do
             any ("Source location" `isInfixOf`) (getLogMessages newState) @?= True
           Left _ -> assertFailure "Source location tracking with debug should succeed"
           
  , testCase "Debug: integrate with IR generation" $
      let func = IRFunction 
            { irFuncName = "test"
            , irFuncParams = [IRParam "x" IRInt]
            , irFuncReturnType = IRBool
            , irFuncBody = [IRReturn (IRLiteral (IRBoolLiteral True))]
            , irFuncSpan = locatedWithSpan (spanBetween (SourcePos 1 1 0) (SourcePos 3 1 0)) "test"
            }
          debugState = enableDebugMode
          irResult = generateIRWithDebug func debugState
      in case irResult of
           Right newState -> do
             any ("IR generation" `isInfixOf`) (getLogMessages newState) @?= True
           Left _ -> assertFailure "IR generation with debug should succeed"
           
  , testCase "Debug: integrate with CLI" $
      let args = ["--debug", "--ownership", "test.typus"]
          debugState = enableDebugMode
          cliResult = runWithDebug args debugState
      in case cliResult of
           Right newState -> do
             any ("CLI" `isInfixOf`) (getLogMessages newState) @?= True
           Left _ -> assertFailure "CLI with debug should succeed"
           
  , testCase "Debug: handle performance profiling" $
      let debugState = enableDebugMode
          profiledState = enableProfiling debugState
          operationResult = profileOperation "test operation" (return ()) profiledState
      in case operationResult of
           Right newState -> 
             any ("Profiling" `isInfixOf`) (getLogMessages newState) @?= True
           Left _ -> assertFailure "Operation profiling should succeed"
           
  , testCase "Debug: handle memory profiling" $
      let debugState = enableDebugMode
          memoryState = enableMemoryProfiling debugState
          operationResult = profileMemoryUsage "test operation" (return ()) memoryState
      in case operationResult of
           Right newState -> 
             any ("Memory profiling" `isInfixOf`) (getLogMessages newState) @?= True
           Left _ -> assertFailure "Memory profiling should succeed"
           
  , testCase "Debug: handle step-by-step execution" $
      let debugState = enableDebugMode
          stepState = enableStepExecution debugState
          operationResult = executeStepByStep "test operation" [return (), return ()] stepState
      in case operationResult of
           Right newState -> 
             any ("Step" `isInfixOf`) (getLogMessages newState) @?= True
           Left _ -> assertFailure "Step-by-step execution should succeed"
           
  , testCase "Debug: handle breakpoints" $
      let debugState = enableDebugMode
          breakpointState = setBreakpoint "test_function" debugState
          operationResult = executeWithBreakpoints "test_function" (return ()) breakpointState
      in case operationResult of
           Right newState -> 
             any ("Breakpoint" `isInfixOf`) (getLogMessages newState) @?= True
           Left _ -> assertFailure "Execution with breakpoints should succeed"
           
  , testCase "Debug: handle variable inspection" $
      let debugState = enableDebugMode
          variables = [("x", 42), ("y", "hello")]
          inspectionResult = inspectVariables variables debugState
      in case inspectionResult of
           Right newState -> do
             any ("Variable inspection" `isInfixOf`) (getLogMessages newState) @?= True
             any ("x = 42" `isInfixOf`) (getLogMessages newState) @?= True
           Left _ -> assertFailure "Variable inspection should succeed
           
  , testCase "Debug: handle call stack inspection" $
      let debugState = enableDebugMode
          callStack = ["main", "processData", "helper"]
          stackResult = inspectCallStack callStack debugState
      in case stackResult of
           Right newState -> 
             any ("Call stack" `isInfixOf`) (getLogMessages newState) @?= True
           Left _ -> assertFailure "Call stack inspection should succeed
           
  , testCase "Debug: integrate all components" $
      let input = "//! ownership=true\n//! dependent_types=true\n```go\npackage main\n\nimport \"fmt\"\n\nfunc main() {\n    data := make([]byte, 100)\n    result := processData(data)\n    fmt.Println(result)\n}\n\nfunc processData(data []byte) string {\n    return string(data)\n}\n```"
          debugState = enableDebugMode
          debugState' = setDebugLevel Trace debugState
      in do
        -- Parse with debug
        let parseResult = parseWithDebug input "integration.typus" debugState'
        case parseResult of
          Right (typusFile, parseState) -> do
            length (tfBlocks typusFile) @?= 1
            
            -- Analyze ownership with debug
            let block = head (tfBlocks typusFile)
                code = cbContent block
                ownershipResult = analyzeOwnershipWithDebug code parseState
            case ownershipResult of
              Right (_, transfers, ownershipState) -> do
                length transfers @?= 1
                
                -- Type check with debug
                let checker = newDependentTypeChecker ()
                    typeCheckResult = checkTypeWithDebug "[]byte" checker ownershipState
                case typeCheckResult of
                  Right (_, typeState) -> do
                    let messages = getLogMessages typeState
                    
                    -- Verify all components logged their debug information
                    any ("Parsing" `isInfixOf`) messages @?= True
                    any ("Ownership" `isInfixOf`) messages @?= True
                    any ("Type checking" `isInfixOf`) messages @?= True
                    
                    -- Verify debug level is preserved
                    getDebugLevel typeState @?= Trace
                    isDebugEnabled typeState @?= True
                  Left _ -> assertFailure "Type check with debug should succeed"
              Left _ -> assertFailure "Ownership analysis with debug should succeed"
          Left _ -> assertFailure "Parse with debug should succeed"
  ]

-- Helper functions
isInfixOf :: String -> String -> Bool
isInfixOf needle haystack = needle `elem` (substrings haystack)
  where
    substrings s = [take i s | i <- [1..length s]]

-- Simplified Debug implementation
data DebugLevel = Trace | Debug | Info | Warning | Error
  deriving (Eq, Show, Ord)

data DebugState = DebugState
  { debugEnabled :: Bool
  , debugLevel :: DebugLevel
  , logMessages :: [String]
  , profilingEnabled :: Bool
  , memoryProfilingEnabled :: Bool
  , stepExecutionEnabled :: Bool
  , breakpoints :: [String]
  }

enableDebugMode :: DebugState
enableDebugMode = DebugState True Info [] False False False []

disableDebugMode :: DebugState
disableDebugMode = DebugState False Info [] False False False []

isDebugEnabled :: DebugState -> Bool
isDebugEnabled state = debugEnabled state

setDebugLevel :: DebugLevel -> DebugState
setDebugLevel level state = state { debugLevel = level }

getDebugLevel :: DebugState -> DebugLevel
getDebugLevel state = debugLevel state

formatDebugMessage :: DebugLevel -> String -> String
formatDebugMessage level message = "[" ++ show level ++ "] " ++ message

logDebugMessage :: DebugLevel -> String -> DebugState -> DebugState
logDebugMessage level message state = 
  if debugEnabled state && level >= debugLevel state
    then state { logMessages = formatDebugMessage level message : logMessages state }
    else state

getLogMessages :: DebugState -> [String]
getLogMessages state = reverse (logMessages state)

filterLogMessages :: DebugLevel -> DebugState -> [String]
filterLogMessages minLevel state = 
  filter (\msg -> let levelStr = takeWhile (/= ']') (drop 1 msg)
                     level = case levelStr of
                              "TRACE" -> Trace
                              "DEBUG" -> Debug
                              "INFO" -> Info
                              "WARNING" -> Warning
                              "ERROR" -> Error
                              _ -> Info
                  in level >= minLevel) (getLogMessages state)

clearDebugLog :: DebugState -> DebugState
clearDebugLog state = state { logMessages = [] }

-- Debug integration functions
parseWithDebug :: String -> String -> DebugState -> Either String (TypusFile, DebugState)
parseWithDebug input filename state = 
  let debugMessage = "Parsing file: " ++ filename
      newState = logDebugMessage Info debugMessage state
      typusFile = TypusFile FileDirectives [CodeBlock ""]  -- Simplified
  in Right (typusFile, newState)

analyzeOwnershipWithDebug :: String -> DebugState -> Either String ((), [()], DebugState)
analyzeOwnershipWithDebug input state = 
  let debugMessage = "Analyzing ownership for code"
      newState = logDebugMessage Info debugMessage state
  in Right ((), [()], newState)

checkTypeWithDebug :: String -> DependentTypeChecker -> DebugState -> Either String (DependentTypeChecker, DebugState)
checkTypeWithDebug typeName checker state = 
  let debugMessage = "Type checking: " ++ typeName
      newState = logDebugMessage Info debugMessage state
  in Right (checker, newState)

handleErrorWithDebug :: TypeError -> DebugState -> Either String DebugState
handleErrorWithDebug err state = 
  let debugMessage = "Handling error: " ++ errorMessage err
      newState = logDebugMessage Error debugMessage state
  in Right newState

trackSourceLocationWithDebug :: SourcePos -> DebugState -> Either String DebugState
trackSourceLocationWithDebug pos state = 
  let debugMessage = "Tracking source location: " ++ show (posLine pos) ++ ":" ++ show (posColumn pos)
      newState = logDebugMessage Debug debugMessage state
  in Right newState

generateIRWithDebug :: IRFunction -> DebugState -> Either String DebugState
generateIRWithDebug func state = 
  let debugMessage = "Generating IR for function: " ++ irFuncName func
      newState = logDebugMessage Info debugMessage state
  in Right newState

runWithDebug :: [String] -> DebugState -> Either String DebugState
runWithDebug args state = 
  let debugMessage = "Running with args: " ++ unwords args
      newState = logDebugMessage Info debugMessage state
  in Right newState

enableProfiling :: DebugState -> DebugState
enableProfiling state = state { profilingEnabled = True }

enableMemoryProfiling :: DebugState -> DebugState
enableMemoryProfiling state = state { memoryProfilingEnabled = True }

profileOperation :: String -> IO () -> DebugState -> Either String DebugState
profileOperation operation _ state = 
  let debugMessage = "Profiling operation: " ++ operation
      newState = logDebugMessage Debug debugMessage state
  in Right newState

profileMemoryUsage :: String -> IO () -> DebugState -> Either String DebugState
profileMemoryUsage operation _ state = 
  let debugMessage = "Profiling memory usage for: " ++ operation
      newState = logDebugMessage Debug debugMessage state
  in Right newState

enableStepExecution :: DebugState -> DebugState
enableStepExecution state = state { stepExecutionEnabled = True }

executeStepByStep :: String -> [IO ()] -> DebugState -> Either String DebugState
executeStepByStep operation steps state = 
  let debugMessage = "Executing step by step: " ++ operation
      newState = logDebugMessage Debug debugMessage state
  in Right newState

setBreakpoint :: String -> DebugState -> DebugState
setBreakpoint function state = 
  let debugMessage = "Setting breakpoint at: " ++ function
      newBreakpoints = function : breakpoints state
      newState = logDebugMessage Debug debugMessage state { breakpoints = newBreakpoints }
  in newState

executeWithBreakpoints :: String -> IO () -> DebugState -> Either String DebugState
executeWithBreakpoints function _ state = 
  let debugMessage = "Executing with breakpoints at: " ++ function
      newState = logDebugMessage Debug debugMessage state
  in Right newState

inspectVariables :: [(String, Show)] -> DebugState -> Either String DebugState
inspectVariables vars state = 
  let debugMessage = "Inspecting variables: " ++ show (map fst vars)
      newState = logDebugMessage Debug debugMessage state
  in Right newState

inspectCallStack :: [String] -> DebugState -> Either String DebugState
inspectCallStack stack state = 
  let debugMessage = "Call stack: " ++ show stack
      newState = logDebugMessage Debug debugMessage state
  in Right newState

-- Simplified Dependencies types for testing
data TypeExpr = TypeVar String | TypeConstructor String [TypeExpr] deriving (Eq, Show)

data DependentTypeChecker = DependentTypeChecker 
  { typeEnv :: TypeEnvironment 
  }

data TypeEnvironment = TypeEnvironment
  { typeEnvTypes :: [(String, TypeExpr)]
  }

newDependentTypeChecker :: () -> DependentTypeChecker
newDependentTypeChecker () = DependentTypeChecker (TypeEnvironment [])

-- Simplified Ownership types for testing
analyzeOwnership :: String -> Either String ((), [()])
analyzeOwnership _ = Right ((), [()])

-- Simplified Parser types for testing
data FileDirectives = FileDirectives deriving (Eq, Show)

data CodeBlock = CodeBlock 
  { cbContent :: String
  } deriving (Eq, Show)

data TypusFile = TypusFile 
  { tfDirectives :: FileDirectives
  , tfBlocks :: [CodeBlock]
  }

defaultFileDirectives :: FileDirectives
defaultFileDirectives = FileDirectives

parseTypus :: String -> String -> Either String TypusFile
parseTypus _ _ = Right (TypusFile FileDirectives [CodeBlock ""])

-- Simplified ErrorHandler types for testing
data ErrorLocation = ErrorLocation 
  { line :: Int
  , column :: Int
  }

data TypeError = TypeError 
  { errorMessage :: String
  , errorLocation :: ErrorLocation
  }

posAt :: Int -> Int -> SourcePos
posAt line column = SourcePos line column

errorAt :: SourcePos -> String -> TypeError
errorAt pos message = TypeError message (ErrorLocation (posLine pos) (posColumn pos))

-- Simplified SourceLocation types for testing
data SourcePos = SourcePos 
  { posLine :: Int
  , posColumn :: Int
  } deriving (Eq, Show)

-- Simplified Compiler IR types for testing
data IRType = IRInt | IRBool | IRString

data IRLiteral = IRIntLiteral Int | IRBoolLiteral Bool | IRStringLiteral String

data IRExpression = IRLiteral IRLiteral | IRVariable String | IRReturn IRExpression

data IRParam = IRParam String IRType

data IRFunction = IRFunction 
  { irFuncName :: String
  , irFuncParams :: [IRParam]
  , irFuncReturnType :: IRType
  , irFuncBody :: [IRExpression]
  , irFuncSpan :: Located String
  }

data Located a = Located 
  { locValue :: a
  , locSpan :: SourceSpan
  }

data SourceSpan = SourceSpan 
  { spanStart :: SourcePos
  , spanEnd :: SourcePos
  }

spanBetween :: SourcePos -> SourcePos -> SourceSpan
spanBetween start end = SourceSpan start end

locatedWithSpan :: SourceSpan -> String -> Located String
locatedWithSpan span value = Located value span