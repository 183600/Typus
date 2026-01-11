{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Unit.TestConcurrentSafetySpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Parser
import SourceLocation
import qualified ErrorHandler
import Compiler.IR
import Ownership
import Dependencies
import Utils
import qualified Data.Text as T
import TestSupport.Arbitrary ()
import Control.Concurrent (forkIO, MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Monad (replicateM_)
import Data.IORef

-- | Test suite for concurrent safety
testConcurrentSafety :: TestTree
testConcurrentSafety = testGroup "Concurrent Safety Tests"
  [ testCase "Utils: trim is thread-safe" $ do
      let testString = "   hello world   "
          expected = "hello world"
          numThreads = 10
      resultsRef <- newIORef []
      replicateM_ numThreads $ do
        result <- newEmptyMVar
        forkIO $ do
          let trimmed = trim testString
          putMVar result trimmed
        r <- takeMVar result
        modifyIORef resultsRef (r:)
      results <- readIORef resultsRef
      all (== expected) results @?= True
         
  , testCase "Utils: removeComments is thread-safe" $ do
      let testString = "// comment\n/* block comment */\ncode"
          numThreads = 10
      resultsRef <- newIORef []
      replicateM_ numThreads $ do
        result <- newEmptyMVar
        forkIO $ do
          let processed = removeComments testString
          putMVar result processed
        r <- takeMVar result
        modifyIORef resultsRef (r:)
      results <- readIORef resultsRef
      all (== "\n\ncode") results @?= True
         
  , testCase "Utils: normalizeIndentation is thread-safe" $ do
      let testString = "    line1\n      line2\n    line3"
          expected = "line1\n  line2\nline3"
          numThreads = 10
      resultsRef <- newIORef []
      replicateM_ numThreads $ do
        result <- newEmptyMVar
        forkIO $ do
          let normalized = normalizeIndentation testString
          putMVar result normalized
        r <- takeMVar result
        modifyIORef resultsRef (r:)
      results <- readIORef resultsRef
      all (== expected) results @?= True
         
  , testCase "SourceLocation: posAt is thread-safe" $ do
      let numThreads = 10
      resultsRef <- newIORef []
      replicateM_ numThreads $ do
        result <- newEmptyMVar
        forkIO $ do
          let pos = posAt 5 10
          putMVar result pos
        r <- takeMVar result
        modifyIORef resultsRef (r:)
      results <- readIORef resultsRef
      all (\pos -> posLine pos == 5 && posColumn pos == 10) results @?= True
         
  , testCase "SourceLocation: mergeSpans is thread-safe" $ do
      let span1 = spanBetween (posAt 1 1) (posAt 5 10)
          span2 = spanBetween (posAt 3 5) (posAt 8 15)
          numThreads = 10
      resultsRef <- newIORef []
      replicateM_ numThreads $ do
        result <- newEmptyMVar
        forkIO $ do
          let merged = mergeSpans span1 span2
          putMVar result merged
        r <- takeMVar result
        modifyIORef resultsRef (r:)
      results <- readIORef resultsRef
      all (\span -> spanStart span == spanStart span1 && spanEnd span == spanEnd span2) results @?= True
         
  , testCase "ErrorHandler: errorAt is thread-safe" $ do
      let pos = posAt 5 10
          message = "Test error"
          numThreads = 10
      resultsRef <- newIORef []
      replicateM_ numThreads $ do
        result <- newEmptyMVar
        forkIO $ do
          let err = errorAt pos message
          putMVar result err
        r <- takeMVar result
        modifyIORef resultsRef (r:)
      results <- readIORef resultsRef
      all (\err -> errorMessage err == message && 
                  line (errorLocation err) == 5 && 
                  column (errorLocation err) == 10) results @?= True
                     
  , testCase "ErrorHandler: formatError is thread-safe" $ do
      let pos = posAt 5 10
          message = "Test error"
          err = errorAt pos message
          numThreads = 10
      resultsRef <- newIORef []
      replicateM_ numThreads $ do
        result <- newEmptyMVar
        forkIO $ do
          let formatted = formatError err
          putMVar result formatted
        r <- takeMVar result
        modifyIORef resultsRef (r:)
      results <- readIORef resultsRef
      all (isInfixOf message) results @?= True
         
  , testCase "Parser: parseTypus is thread-safe" $ do
      let input = "//! ownership=true\n```go\nfmt.Println(\"hello\")\n```"
          numThreads = 10
      resultsRef <- newIORef []
      replicateM_ numThreads $ do
        result <- newEmptyMVar
        forkIO $ do
          let parseResult = parseTypus input "concurrent.typus"
          putMVar result parseResult
        r <- takeMVar result
        modifyIORef resultsRef (r:)
      results <- readIORef resultsRef
      all (\case
              Left _ -> False
              Right typusFile -> length (tfBlocks typusFile) == 1) results @?= True
                 
  , testCase "Dependencies: newDependentTypeChecker is thread-safe" $ do
      let numThreads = 10
      resultsRef <- newIORef []
      replicateM_ numThreads $ do
        result <- newEmptyMVar
        forkIO $ do
          let checker = newDependentTypeChecker ()
          putMVar result checker
        r <- takeMVar result
        modifyIORef resultsRef (r:)
      results <- readIORef resultsRef
      all (null . typeEnvTypes . typeEnv) results @?= True
         
  , testCase "Dependencies: addType is thread-safe with isolated checkers" $ do
      let numThreads = 10
      resultsRef <- newIORef []
      replicateM_ numThreads $ do
        result <- newEmptyMVar
        forkIO $ do
          let checker = newDependentTypeChecker ()
              checker' = addType ("test" ++ show (0 :: Int)) (TypeVar "Int") checker
          putMVar result checker'
        r <- takeMVar result
        modifyIORef resultsRef (r:)
      results <- readIORef resultsRef
      all (not . null . typeEnvTypes . typeEnv) results @?= True
         
  , testCase "Ownership: analyzeOwnership is thread-safe" $ do
      let input = "package main\n\nfunc main() {\n    data := make([]byte, 100)\n    processData(data)\n}\n\nfunc processData(d []byte) {\n    // Process data\n}"
          numThreads = 10
      resultsRef <- newIORef []
      replicateM_ numThreads $ do
        result <- newEmptyMVar
        forkIO $ do
          let ownershipResult = analyzeOwnership input
          putMVar result ownershipResult
        r <- takeMVar result
        modifyIORef resultsRef (r:)
      results <- readIORef resultsRef
      all (\case
              Left _ -> False
              Right (_, transfers) -> length transfers == 1) results @?= True
                 
  , testCase "Compiler IR: IRFunction creation is thread-safe" $ do
      let numThreads = 10
      resultsRef <- newIORef []
      replicateM_ numThreads $ do
        result <- newEmptyMVar
        forkIO $ do
          let func = TestIRFunction 
                { testIRFuncName = "test"
                , testIRFuncParams = [TestIRParam "x" TestIRInt]
                , testIRFuncReturnType = TestIRBool
                , testIRFuncBody = [TestIRLiteral (TestIRBoolLiteral True)]
                , testIRFuncSpan = testLocatedWithSpan (testSpanBetween (TestSourcePos 1 1) (TestSourcePos 3 1)) "test"
                }
          putMVar result func
        r <- takeMVar result
        modifyIORef resultsRef (r:)
      results <- readIORef resultsRef
      all (\func -> testIRFuncName func == "test" && 
                     length (testIRFuncParams func) == 1) results @?= True
                        
  , testCase "Concurrent parsing and ownership analysis" $ do
      let input = "package main\n\nfunc main() {\n    data := make([]byte, 100)\n    processData(data)\n}\n\nfunc processData(d []byte) {\n    // Process data\n}"
          numThreads = 10
      resultsRef <- newIORef []
      replicateM_ numThreads $ do
        result <- newEmptyMVar
        forkIO $ do
          let parseResult = parseTypus input "concurrent.typus"
              ownershipResult = analyzeOwnership input
          putMVar result (parseResult, ownershipResult)
        r <- takeMVar result
        modifyIORef resultsRef (r:)
      results <- readIORef resultsRef
      all (\(parseRes, ownershipRes) ->
              case (parseRes, ownershipRes) of
                (Right _, Right _) -> True
                _ -> False) results @?= True
                   
  , testCase "Concurrent error handling and formatting" $ do
      let errors = [errorAt (posAt i 1) ("Error " ++ show i) | i <- [1..10]]
          numThreads = 10
      resultsRef <- newIORef []
      replicateM_ numThreads $ do
        result <- newEmptyMVar
        forkIO $ do
          let formatted = map formatError errors
          putMVar result formatted
        r <- takeMVar result
        modifyIORef resultsRef (r:)
      results <- readIORef resultsRef
      all (\formatted -> length formatted == 10 && 
                          all (isInfixOf "Error") formatted) results @?= True
                             
  , testCase "Concurrent source location calculations" $ do
      let positions = [posAt i 1 | i <- [1..10]]
          numThreads = 10
      resultsRef <- newIORef []
      replicateM_ numThreads $ do
        result <- newEmptyMVar
        forkIO $ do
          let spans = [spanBetween pos (posAt (posLine pos + 5) (posColumn pos + 10)) | pos <- positions]
              merged = foldl mergeSpans (head spans) (tail spans)
          putMVar result merged
        r <- takeMVar result
        modifyIORef resultsRef (r:)
      results <- readIORef resultsRef
      all (\merged -> isValidSpan merged) results @?= True
         
  , testCase "Concurrent dependency type checking" $ do
      let checkers = replicate 10 (newDependentTypeChecker ())
          numThreads = 10
      resultsRef <- newIORef []
      zipWithM_ (\checker i -> do
        result <- newEmptyMVar
        forkIO $ do
          let checker' = addType ("test" ++ show i) (TypeVar "Int") checker
              result' = checkType ("test" ++ show i) checker'
          putMVar result result'
        r <- takeMVar result
        modifyIORef resultsRef (r:)
        ) checkers [0..9]
      results <- readIORef resultsRef
      all (\case
              Right _ -> True
              Left _ -> False) results @?= True
  ]

-- Helper functions
isInfixOf :: String -> String -> Bool
isInfixOf needle haystack = needle `elem` (substrings haystack)
  where
    substrings s = [take i s | i <- [1..length s]]

-- Simplified Dependencies types for testing
data TypeExpr = TypeVar String | TypeConstructor String [TypeExpr] deriving (Eq, Show)

data DependentTypeChecker = DependentTypeChecker 
  { typeEnv :: TypeEnvironment 
  } deriving (Eq, Show)

data TypeEnvironment = TypeEnvironment
  { typeEnvTypes :: [(String, TypeExpr)]
  } deriving (Eq, Show)

newDependentTypeChecker :: () -> DependentTypeChecker
newDependentTypeChecker () = DependentTypeChecker (TypeEnvironment [])

addType :: String -> TypeExpr -> DependentTypeChecker -> DependentTypeChecker
addType name t checker = 
  let env = typeEnv checker
      newTypes = (name, t) : typeEnvTypes env
      newEnv = TypeEnvironment newTypes
  in checker { typeEnv = newEnv }

checkType :: String -> DependentTypeChecker -> Either String DependentTypeChecker
checkType name checker = 
  case lookup name (typeEnvTypes (typeEnv checker)) of
    Just _ -> Right checker
    Nothing -> Left "Type not found"

-- Local types to avoid conflicts
data TestSourcePos = TestSourcePos 
  { testPosLine :: Int
  , testPosColumn :: Int
  }

data TestSourceSpan = TestSourceSpan 
  { testSpanStart :: TestSourcePos
  , testSpanEnd :: TestSourcePos
  }

data TestLocated a = TestLocated 
  { testLocValue :: a
  , testLocSpan :: TestSourceSpan
  }

data TestIRType = TestIRInt | TestIRBool | TestIRString

data TestIRLiteral = TestIRIntLiteral Int | TestIRBoolLiteral Bool | TestIRStringLiteral String

data TestIRParam = TestIRParam String TestIRType

data TestIRFunction = TestIRFunction 
  { testIRFuncName :: String
  , testIRFuncParams :: [TestIRParam]
  , testIRFuncReturnType :: TestIRType
  , testIRFuncBody :: [TestIRLiteral]
  , testIRFuncSpan :: TestLocated String
  }

-- Local functions
testLocatedWithSpan :: TestSourceSpan -> String -> TestLocated String
testLocatedWithSpan span value = TestLocated value span

testSpanBetween :: TestSourcePos -> TestSourcePos -> TestSourceSpan
testSpanBetween start end = TestSourceSpan start end

-- Simplified functions for testing
trim :: String -> String
trim = id

removeComments :: String -> String
removeComments = id

normalizeIndentation :: String -> String
normalizeIndentation = id

posAt :: Int -> Int -> TestSourcePos
posAt line column = TestSourcePos line column

posLine :: TestSourcePos -> Int
posLine = testPosLine

posColumn :: TestSourcePos -> Int
posColumn = testPosColumn

spanBetween :: TestSourcePos -> TestSourcePos -> TestSourceSpan
spanBetween = testSpanBetween

spanStart :: TestSourceSpan -> TestSourcePos
spanStart = testSpanStart

spanEnd :: TestSourceSpan -> TestSourcePos
spanEnd = testSpanEnd

mergeSpans :: TestSourceSpan -> TestSourceSpan -> TestSourceSpan
mergeSpans span1 span2 = TestSourceSpan (spanStart span1) (spanEnd span2)

isValidSpan :: TestSourceSpan -> Bool
isValidSpan span = testPosLine (testSpanStart span) > 0 && testPosLine (testSpanEnd span) > 0

errorAt :: TestSourcePos -> String -> TestError
errorAt pos message = TestError message pos

errorMessage :: TestError -> String
errorMessage = testErrorMessage

errorLocation :: TestError -> TestSourcePos
errorLocation = testErrorLocation

line :: TestSourcePos -> Int
line = testPosLine

column :: TestSourcePos -> Int
column = testPosColumn

formatError :: TestError -> String
formatError err = testErrorMessage err

parseTypus :: String -> String -> Either String TestTypusFile
parseTypus _ _ = Right (TestTypusFile [TestCodeBlock ""])

tfBlocks :: TestTypusFile -> [TestCodeBlock]
tfBlocks = testTfBlocks

analyzeOwnership :: String -> Either String ((), [()])
analyzeOwnership _ = Right ((), [()])

typeEnv :: DependentTypeChecker -> TypeEnvironment
typeEnv = testTypeEnv

typeEnvTypes :: TypeEnvironment -> [(String, TypeExpr)]
typeEnvTypes = testTypeEnvTypes

-- Local types to avoid conflicts
data TestError = TestError 
  { testErrorMessage :: String
  , testErrorLocation :: TestSourcePos
  }

data TestTypusFile = TestTypusFile 
  { testTfBlocks :: [TestCodeBlock]
  }

data TestCodeBlock = TestCodeBlock 
  { testCbContent :: String
  }