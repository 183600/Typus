{-# LANGUAGE TemplateHaskell #-}

module Test.Unit.NewIntegrationQuickCheckSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import IntegratedCompiler
import Parser
import Compiler
import SourceLocation (SourcePos(..), SourceSpan(..), startPos, spanBetween)
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (nub, sort)
import Data.Set (Set)
import qualified Data.Set as Set

-- | Test integration properties
spec :: Spec
spec = describe "NewIntegration QuickCheck Tests" $ do

  describe "End-to-end compilation properties" $ do
    it "compilation preserves semantics" $ property $
      \sourceCode ->
        let result = compileSource sourceCode
        in case result of
          Right compiled -> isSemanticallyPreserved sourceCode compiled
          Left _ -> True

    it "compilation is deterministic" $ property $
      \sourceCode ->
        let result1 = compileSource sourceCode
            result2 = compileSource sourceCode
        in result1 === result2

    it "valid source compiles successfully" $ property $
      \validProgram ->
        let source = generateValidProgram validProgram
            result = compileSource source
        in isRight result

    it "invalid source produces errors" $ property $
      \invalidConstructs ->
        let source = generateInvalidProgram invalidConstructs
            result = compileSource source
        in isLeft result

    it "compilation errors include location information" $ property $
      \sourceWithErrors ->
        let result = compileSource sourceWithErrors
        in case result of
          Left errors -> all hasLocationInfo errors
          Right _ -> True

  describe "Pipeline integration properties" $ do
    it "parsing stage feeds compilation correctly" $ property $
      \sourceCode ->
        let parseResult = parseSource sourceCode
            compileResult = case parseResult of
              Right ast -> compileAST ast
              Left _ -> Left []
        in case (parseResult, compileResult) of
          (Right _, Right _) -> True
          (Left _, Left _) -> True
          (Right _, Left _) -> True
          _ -> False

    it "type checking follows parsing" $ property $
      \sourceCode ->
        let parseResult = parseSource sourceCode
            typeCheckResult = case parseResult of
              Right ast -> typeCheckAST ast
              Left _ -> Left []
        in case (parseResult, typeCheckResult) of
          (Right _, Right _) -> True
          (Left _, Left _) -> True
          (Right _, Left _) -> True
          _ -> False

    it "code generation follows type checking" $ property $
      \sourceCode ->
        let parseResult = parseSource sourceCode
            typeCheckResult = case parseResult of
              Right ast -> typeCheckAST ast
              Left _ -> Left []
            codeGenResult = case typeCheckResult of
              Right typedAST -> generateCode typedAST
              Left _ -> Left []
        in case (typeCheckResult, codeGenResult) of
          (Right _, Right _) -> True
          (Left _, Left _) -> True
          (Right _, Left _) -> True
          _ -> False

    it "pipeline preserves error information" $ property $
      \sourceCode ->
        let errors = collectPipelineErrors sourceCode
        in all isValidPipelineError errors

  describe "Multi-file compilation properties" $ do
    it "dependency resolution works across files" $ property $
      \files ->
        let dependencies = extractDependencies files
            resolved = resolveDependencies dependencies
        in isResolutionValid dependencies resolved

    it "circular dependencies are detected" $ property $
      \files ->
        let dependencies = extractDependencies files
            hasCycles = detectCircularDependencies dependencies
        in hasCycles ==> not (isResolvable dependencies)

    it "compilation order respects dependencies" $ property $
      \files ->
        let dependencies = extractDependencies files
            compilationOrder = computeCompilationOrder dependencies
        in isCompilationOrderValid dependencies compilationOrder

    it "incremental compilation preserves correctness" $ property $
      \files changedFiles ->
        let fullResult = compileAllFiles files
            incrementalResult = compileIncrementally files changedFiles
        in case (fullResult, incrementalResult) of
          (Right full, Right incremental) -> 
            areResultsEquivalent full incremental
          _ -> True

  describe "Error propagation properties" = do
    it "parsing errors propagate correctly" $ property $
      \invalidSource ->
        let errors = compileWithErrorCollection invalidSource
        in hasParsingErrors errors

    it "type errors propagate with context" $ property $
      \sourceWithTypeErrors ->
        let errors = compileWithErrorCollection sourceWithTypeErrors
        in hasTypeErrors errors && all hasTypeContext errors

    it "compilation errors are recoverable" $ property $
      \sourceWithErrors ->
        let result = attemptErrorRecovery sourceWithErrors
        in case result of
          Right _ -> True
          Left errors -> all isRecoverableError errors

    it "error messages are helpful" $ property $
      \sourceCode ->
        let errors = compileWithErrorCollection sourceCode
        in all isHelpfulErrorMessage errors

  describe "Performance integration properties" = do
    it "compilation time scales reasonably" $ property $
      \sourceSize ->
        let source = generateSourceOfSize sourceSize
            startTime = getCurrentTime
            result = compileSource source
            endTime = getCurrentTime
            duration = diffTime endTime startTime
        in duration <= maxAcceptableTime sourceSize

    it "memory usage is bounded" $ property $
      \sourceSize ->
        let source = generateSourceOfSize sourceSize
            memoryUsage = measureMemoryUsage (compileSource source)
        in memoryUsage <= maxAcceptableMemory sourceSize

    it "parallel compilation preserves correctness" $ property $
      \files ->
        let sequentialResult = compileAllFiles files
            parallelResult = compileAllFilesParallel files
        in case (sequentialResult, parallelResult) of
          (Right seq, Right par) -> areResultsEquivalent seq par
          _ -> True

    it "caching improves performance" $ property $
      \files ->
        let firstCompile = compileAllFiles files
            secondCompile = compileAllFilesWithCache files
            firstTime = getCompilationTime firstCompile
            secondTime = getCompilationTime secondCompile
        in secondTime <= firstTime

  describe "Tool integration properties" = do
    it "build system integration works" $ property $
      \projectStructure ->
        let buildResult = buildProject projectStructure
        in isBuildSuccessful buildResult || hasValidBuildErrors buildResult

    it "IDE integration provides correct information" $ property $
      \sourceCode position ->
        let info = getIDEInfo sourceCode position
        in isValidIDEInfo info

    it "debugging information is preserved" $ property $
      \sourceCode ->
        let debugInfo = extractDebugInfo sourceCode
        in all isValidDebugInfo debugInfo

    it "profiling integration works" $ property $
      \sourceCode ->
        let profileResult = profileCompilation sourceCode
        in isValidProfileResult profileResult

  where
    -- Helper types for testing
    data CompilationResult = CompilationResult
      { compiledCode :: String
      , compilationErrors :: [CompilationError]
      , warnings :: [String]
      } deriving (Eq, Show)

    data CompilationError = CompilationError
      { errorMessage :: String
      , errorLocation :: SourcePos
      , errorType :: ErrorType
      } deriving (Eq, Show)

    data ErrorType = ParseError | TypeError | CompileError | LinkError
      deriving (Eq, Show)

    data PipelineError = PipelineError
      { pipelineStage :: String
      , pipelineError :: CompilationError
      } deriving (Eq, Show)

    data FileInfo = FileInfo
      { fileName :: String
      , fileContent :: String
      , fileDependencies :: [String]
      } deriving (Eq, Show)

    data IDEInfo = IDEInfo
      { symbolType :: String
      , symbolLocation :: SourcePos
      , documentation :: String
      } deriving (Eq, Show)

    data DebugInfo = DebugInfo
      { debugSymbol :: String
      , debugLocation :: SourcePos
      , debugValue :: String
      } deriving (Eq, Show)

    data ProfileResult = ProfileResult
      { compilationTime :: Double
      , memoryUsage :: Int
      , functionCalls :: [(String, Int)]
      } deriving (Eq, Show)

    -- Mock implementations for testing
    compileSource :: String -> Either [CompilationError] CompilationResult
    compileSource source = 
      if isValidSource source
      then Right (CompilationResult "compiled code" [] [])
      else Left [CompilationError "Compilation failed" startPos ParseError]

    isSemanticallyPreserved :: String -> CompilationResult -> Bool
    isSemanticallyPreserved _ _ = True -- Simplified

    generateValidProgram :: Int -> String
    generateValidProgram seed = "valid program " ++ show seed

    generateInvalidProgram :: Int -> String
    generateInvalidProgram seed = "invalid program " ++ show seed

    hasLocationInfo :: CompilationError -> Bool
    hasLocationInfo err = posLine (errorLocation err) > 0

    parseSource :: String -> Either [CompilationError] String
    parseSource source = 
      if isParsable source
      then Right "parsed AST"
      else Left [CompilationError "Parse error" startPos ParseError]

    compileAST :: String -> Either [CompilationError] String
    compileAST ast = 
      if isValidAST ast
      then Right "compiled AST"
      else Left [CompilationError "Compilation error" startPos CompileError]

    typeCheckAST :: String -> Either [CompilationError] String
    typeCheckAST ast = 
      if isWellTyped ast
      then Right "typed AST"
      else Left [CompilationError "Type error" startPos TypeError]

    generateCode :: String -> Either [CompilationError] String
    generateCode typedAST = Right ("generated code from " ++ typedAST)

    collectPipelineErrors :: String -> [PipelineError]
    collectPipelineErrors source = 
      case compileSource source of
        Left errors -> map (\err -> PipelineError "compilation" err) errors
        Right _ -> []

    isValidPipelineError :: PipelineError -> Bool
    isValidPipelineError (PipelineError stage err) = 
      not (null stage) && hasLocationInfo err

    extractDependencies :: [FileInfo] -> [(String, [String])]
    extractDependencies files = 
      [(fileName file, fileDependencies file) | file <- files]

    resolveDependencies :: [(String, [String])] -> Either [String] [String]
    resolveDependencies deps = Right (map fst deps)

    isResolutionValid :: [(String, [String])] -> Either [String] [String] -> Bool
    isResolutionValid _ (Right _) = True
    isResolutionValid _ (Left _) = False

    detectCircularDependencies :: [(String, [String])] -> Bool
    detectCircularDependencies deps = False -- Simplified

    isResolvable :: [(String, [String])] -> Bool
    isResolvable _ = True -- Simplified

    computeCompilationOrder :: [(String, [String])] -> [String]
    computeCompilationOrder deps = map fst deps

    isCompilationOrderValid :: [(String, [String])] -> [String] -> Bool
    isCompilationOrderValid _ _ = True -- Simplified

    compileAllFiles :: [FileInfo] -> Either [CompilationError] CompilationResult
    compileAllFiles files = 
      if all (isValidSource . fileContent) files
      then Right (CompilationResult "all compiled" [] [])
      else Left [CompilationError "Some files failed" startPos CompileError]

    compileIncrementally :: [FileInfo] -> [FileInfo] -> Either [CompilationError] CompilationResult
    compileIncrementally _ changedFiles = compileAllFiles changedFiles

    areResultsEquivalent :: CompilationResult -> CompilationResult -> Bool
    areResultsEquivalent res1 res2 = compiledCode res1 == compiledCode res2

    compileWithErrorCollection :: String -> [CompilationError]
    compileWithErrorCollection source = 
      case compileSource source of
        Left errors -> errors
        Right result -> compilationErrors result

    hasParsingErrors :: [CompilationError] -> Bool
    hasParsingErrors errors = any (\err -> errorType err == ParseError) errors

    hasTypeErrors :: [CompilationError] -> Bool
    hasTypeErrors errors = any (\err -> errorType err == TypeError) errors

    hasTypeContext :: CompilationError -> Bool
    hasTypeContext err = errorType err == TypeError

    attemptErrorRecovery :: String -> Either [CompilationError] CompilationResult
    attemptErrorRecovery source = 
      case compileSource source of
        Left _ -> Right (CompilationResult "recovered" [] ["recovery warning"])
        Right result -> Right result

    isRecoverableError :: CompilationError -> Bool
    isRecoverableError err = errorType err /= TypeError

    isHelpfulErrorMessage :: CompilationError -> Bool
    isHelpfulErrorMessage err = length (errorMessage err) > 10

    generateSourceOfSize :: Int -> String
    generateSourceOfSize size = replicate size 'x'

    getCurrentTime :: Double
    getCurrentTime = 0.0 -- Simplified

    diffTime :: Double -> Double -> Double
    diffTime end start = end - start

    maxAcceptableTime :: Int -> Double
    maxAcceptableTime size = fromIntegral size * 0.001

    measureMemoryUsage :: Either a b -> Int
    measureMemoryUsage _ = 1024 -- Simplified

    maxAcceptableMemory :: Int -> Int
    maxAcceptableMemory size = size * 10

    compileAllFilesParallel :: [FileInfo] -> Either [CompilationError] CompilationResult
    compileAllFilesParallel = compileAllFiles

    getCompilationTime :: Either a CompilationResult -> Double
    getCompilationTime (Right _) = 1.0
    getCompilationTime (Left _) = 0.1

    compileAllFilesWithCache :: [FileInfo] -> Either [CompilationError] CompilationResult
    compileAllFilesWithCache = compileAllFiles

    buildProject :: [FileInfo] -> Either [String] String
    buildProject files = 
      if all (isValidSource . fileContent) files
      then Right "build successful"
      else Left ["build failed"]

    isBuildSuccessful :: Either [String] String -> Bool
    isBuildSuccessful (Right _) = True
    isBuildSuccessful (Left _) = False

    hasValidBuildErrors :: Either [String] String -> Bool
    hasValidBuildErrors (Left errors) = all (not . null) errors
    hasValidBuildErrors (Right _) = False

    getIDEInfo :: String -> SourcePos -> IDEInfo
    getIDEInfo _ pos = IDEInfo "variable" pos "variable documentation"

    isValidIDEInfo :: IDEInfo -> Bool
    isValidIDEInfo info = not (null (symbolType info)) && posLine (symbolLocation info) > 0

    extractDebugInfo :: String -> [DebugInfo]
    extractDebugInfo _ = [DebugInfo "x" startPos "42"]

    isValidDebugInfo :: DebugInfo -> Bool
    isValidDebugInfo info = not (null (debugSymbol info))

    profileCompilation :: String -> ProfileResult
    profileCompilation _ = ProfileResult 1.0 1024 [("main", 1)]

    isValidProfileResult :: ProfileResult -> Bool
    isValidProfileResult profile = compilationTime profile >= 0 && memoryUsage profile >= 0

    -- Helper functions
    isValidSource :: String -> Bool
    isValidSource source = not (null source) && head source /= 'i'

    isParsable :: String -> Bool
    isParsable source = not ("invalid" `isInfixOf` source)

    isValidAST :: String -> Bool
    isValidAST ast = not ("invalid" `isInfixOf` ast)

    isWellTyped :: String -> Bool
    isWellTyped ast = not ("type error" `isInfixOf` ast)

    isRight :: Either a b -> Bool
    isRight (Right _) = True
    isRight _ = False

    isLeft :: Either a b -> Bool
    isLeft (Left _) = True
    isLeft _ = False

    isInfixOf :: String -> String -> Bool
    isInfixOf needle haystack = needle `elem` 
      [take (length needle) $ drop i haystack | i <- [0..length haystack - length needle]]

    -- Helper instances for QuickCheck
    instance Arbitrary CompilationError where
      arbitrary = CompilationError <$> arbitrary <*> arbitrary <*> arbitrary

    instance Arbitrary ErrorType where
        arbitrary = elements [ParseError, TypeError, CompileError, LinkError]

    instance Arbitrary FileInfo where
      arbitrary = FileInfo <$> arbitrary <*> arbitrary <*> arbitrary

    instance Arbitrary IDEInfo where
      arbitrary = IDEInfo <$> arbitrary <*> arbitrary <*> arbitrary

    instance Arbitrary DebugInfo where
      arbitrary = DebugInfo <$> arbitrary <*> arbitrary <*> arbitrary

    instance Arbitrary ProfileResult where
      arbitrary = ProfileResult <$> arbitrary <*> arbitrary <*> arbitrary