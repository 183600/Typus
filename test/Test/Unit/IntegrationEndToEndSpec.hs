{-# LANGUAGE OverloadedStrings #-}

module Test.Unit.IntegrationEndToEndSpec (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Data.List (sort, nub, intersect, union, (\\))
import Data.Maybe (isJust, isNothing, fromMaybe, catMaybes)
import qualified Data.Set as Set
import qualified Data.Map as Map
import SourceLocation (SourcePos(..), SourceSpan(..))

-- Mock data types for end-to-end integration testing
data SourceFile = SourceFile
  { filePath :: String
  , fileContent :: String
  , fileEncoding :: String
  } deriving (Show, Eq)

data Token = Token
  { tokenType :: String
  , tokenValue :: String
  , tokenSpan :: SourceSpan
  } deriving (Show, Eq)

data ASTNode = ASTNode
  { nodeType :: String
  , nodeValue :: Maybe String
  , nodeChildren :: [ASTNode]
  , nodeSpan :: SourceSpan
  } deriving (Show, Eq)

data IRInstruction = IRInstruction
  { instructionOp :: String
  , instructionArgs :: [String]
  , instructionResult :: Maybe String
  } deriving (Show, Eq)

data CompilationPhase = Lexing | Parsing | TypeChecking | Optimization | CodeGeneration deriving (Show, Eq, Ord)

data CompilationResult = CompilationResult
  { resultPhase :: CompilationPhase
  , resultSuccess :: Bool
  , resultWarnings :: [String]
  , resultErrors :: [String]
  , resultOutput :: Maybe String
  } deriving (Show, Eq)

data CompilationPipeline = CompilationPipeline
  { pipelineSource :: SourceFile
  , pipelineTokens :: [Token]
  , pipelineAST :: ASTNode
  , pipelineIR :: [IRInstruction]
  , pipelineResults :: [CompilationResult]
  } deriving (Show, Eq)

-- Mock compilation functions
lexSource :: SourceFile -> CompilationResult
lexSource file = 
  let content = fileContent file
      hasTokens = not (null content)
      warnings = if length content > 1000 then ["Large source file"] else []
      errors = if null content then ["Empty source file"] else []
      success = null errors
      output = if success then Just "Tokens generated" else Nothing
  in CompilationResult Lexing success warnings errors output

parseTokens :: [Token] -> CompilationResult
parseTokens tokens = 
  let hasTokens = not (null tokens)
      warnings = if length tokens > 1000 then ["Large token stream"] else []
      errors = if null tokens then ["No tokens to parse"] else []
      success = null errors
      output = if success then Just "AST generated" else Nothing
  in CompilationResult Parsing success warnings errors output

checkTypes :: ASTNode -> CompilationResult
checkTypes ast = 
  let hasAST = nodeType ast /= ""
      warnings = if length (nodeChildren ast) > 100 then ["Large AST"] else []
      errors = if not hasAST then ["Empty AST"] else []
      success = null errors
      output = if success then Just "Type checking passed" else Nothing
  in CompilationResult TypeChecking success warnings errors output

optimizeIR :: [IRInstruction] -> CompilationResult
optimizeIR ir = 
  let hasIR = not (null ir)
      warnings = if length ir > 1000 then ["Large IR"] else []
      errors = if null ir then ["No IR to optimize"] else []
      success = null errors
      output = if success then Just "IR optimized" else Nothing
  in CompilationResult Optimization success warnings errors output

generateCode :: [IRInstruction] -> CompilationResult
generateCode ir = 
  let hasIR = not (null ir)
      warnings = if length ir > 1000 then ["Large IR may generate large code"] else []
      errors = if null ir then ["No IR to generate code from"] else []
      success = null errors
      output = if success then Just "Code generated" else Nothing
  in CompilationResult CodeGeneration success warnings errors output

runCompilationPipeline :: SourceFile -> CompilationPipeline
runCompilationPipeline file = 
  let lexResult = lexSource file
      tokens = []  -- Mock tokens
      parseResult = parseTokens tokens
      ast = ASTNode "Root" Nothing [] (SourceSpan (SourcePos 1 1 0) (SourcePos 1 1 0))
      typeResult = checkTypes ast
      ir = []  -- Mock IR
      optimizeResult = optimizeIR ir
      generateResult = generateCode ir
      results = [lexResult, parseResult, typeResult, optimizeResult, generateResult]
  in CompilationPipeline file tokens ast ir results

getPhaseResult :: CompilationPhase -> CompilationPipeline -> Maybe CompilationResult
getPhaseResult phase pipeline = 
  let results = pipelineResults pipeline
      matching = filter (\r -> resultPhase r == phase) results
  in case matching of
       [result] -> Just result
       _ -> Nothing

isPipelineSuccessful :: CompilationPipeline -> Bool
isPipelineSuccessful pipeline = 
  let results = pipelineResults pipeline
      successfulResults = filter resultSuccess results
  in length successfulResults == length results

tests :: TestTree
tests = testGroup "End-to-End Integration Tests"
  [ testGroup "Source files"
    [ testCase "creates source files correctly" $ do
        let file = SourceFile "test.typus" "content" "UTF-8"
        filePath file @?= "test.typus"
        fileContent file @?= "content"
        fileEncoding file @?= "UTF-8"
      
    , testCase "handles empty source files" $ do
        let file = SourceFile "empty.typus" "" "UTF-8"
        fileContent file @?= ""
      
    , testCase "handles source files with special characters" $ do
        let content = "特殊字符 & symbols !@#$%^&*()"
            file = SourceFile "special.typus" content "UTF-8"
        fileContent file @?= content
    ]

  , testGroup "Tokens"
    [ testCase "creates tokens correctly" $ do
        let span = SourceSpan (SourcePos 1 1 0) (SourcePos 1 5 4)
            token = Token "Identifier" "hello" span
        tokenType token @?= "Identifier"
        tokenValue token @?= "hello"
        tokenSpan token @?= span
      
    , testCase "handles tokens with empty values" $ do
        let span = SourceSpan (SourcePos 1 1 0) (SourcePos 1 1 0)
            token = Token "EOF" "" span
        tokenType token @?= "EOF"
        tokenValue token @?= ""

  describe "AST nodes" $ do
    it "creates AST nodes correctly" $ do
      let span = SourceSpan (SourcePos 1 1 0) (SourcePos 1 10 9)
          node = ASTNode "Function" (Just "main") [] span
      nodeType node `shouldBe` "Function"
      nodeValue node `shouldBe` Just "main"
      nodeChildren node `shouldBe` []
      nodeSpan node `shouldBe` span
      
    it "handles AST nodes with children" $ do
      let span = SourceSpan (SourcePos 1 1 0) (SourcePos 1 20 19)
          child = ASTNode "Parameter" (Just "x") (SourceSpan (SourcePos 1 10 9) (SourcePos 1 15 14))
          node = ASTNode "Function" (Just "main") [child] span
      length (nodeChildren node) `shouldBe` 1
      head (nodeChildren node) `shouldBe` child
      
    it "handles empty AST nodes" $ do
      let span = SourceSpan (SourcePos 1 1 0) (SourcePos 1 1 0)
          node = ASTNode "" Nothing [] span
      nodeType node `shouldBe` ""
      nodeValue node `shouldBe` Nothing
      nodeChildren node `shouldBe` []

  describe "IR instructions" $ do
    it "creates IR instructions correctly" $ do
      let instruction = IRInstruction "add" ["x", "y"] (Just "z")
      instructionOp instruction `shouldBe` "add"
      instructionArgs instruction `shouldBe` ["x", "y"]
      instructionResult instruction `shouldBe` Just "z"
      
    it "handles IR instructions without results" $ do
      let instruction = IRInstruction "store" ["x", "42"] Nothing
      instructionOp instruction `shouldBe` "store"
      instructionArgs instruction `shouldBe` ["x", "42"]
      instructionResult instruction `shouldBe` Nothing
      
    it "handles IR instructions with no arguments" $ do
      let instruction = IRInstruction "nop" [] Nothing
      instructionOp instruction `shouldBe` "nop"
      instructionArgs instruction `shouldBe` []
      instructionResult instruction `shouldBe` Nothing

  describe "Compilation results" $ do
    it "creates successful compilation results" $ do
      let result = CompilationResult Lexing True [] [] (Just "Success")
      resultPhase result `shouldBe` Lexing
      resultSuccess result `shouldBe` True
      resultWarnings result `shouldBe` []
      resultErrors result `shouldBe` []
      resultOutput result `shouldBe` Just "Success"
      
    it "creates failed compilation results" $ do
      let result = CompilationResult Parsing False ["Warning"] ["Error"] Nothing
      resultPhase result `shouldBe` Parsing
      resultSuccess result `shouldBe` False
      resultWarnings result `shouldBe` ["Warning"]
      resultErrors result `shouldBe` ["Error"]
      resultOutput result `shouldBe` Nothing

  describe "Compilation pipeline" $ do
    it "creates compilation pipeline correctly" $ do
      let file = SourceFile "test.typus" "content" "UTF-8"
          tokens = []
          ast = ASTNode "Root" Nothing [] (SourceSpan (SourcePos 1 1 0) (SourcePos 1 1 0))
          ir = []
          results = []
          pipeline = CompilationPipeline file tokens ast ir results
      pipelineSource pipeline `shouldBe` file
      pipelineTokens pipeline `shouldBe` tokens
      pipelineAST pipeline `shouldBe` ast
      pipelineIR pipeline `shouldBe` ir
      pipelineResults pipeline `shouldBe` results
      
    it "runs compilation pipeline correctly" $ do
      let file = SourceFile "test.typus" "content" "UTF-8"
          pipeline = runCompilationPipeline file
          results = pipelineResults pipeline
      length results `shouldBe` 5
      map resultPhase results `shouldBe` [Lexing, Parsing, TypeChecking, Optimization, CodeGeneration]
      
    it "gets phase results correctly" $ do
      let file = SourceFile "test.typus" "content" "UTF-8"
          pipeline = runCompilationPipeline file
          lexResult = getPhaseResult Lexing pipeline
          parseResult = getPhaseResult Parsing pipeline
      resultPhase <$> lexResult `shouldBe` Just Lexing
      resultPhase <$> parseResult `shouldBe` Just Parsing
      
    it "checks pipeline success correctly" $ do
      let file = SourceFile "test.typus" "content" "UTF-8"
          pipeline = runCompilationPipeline file
      isPipelineSuccessful pipeline `shouldBe` True

  describe "Phase-specific compilation" $ do
    it "handles lexing phase" $ do
      let file = SourceFile "test.typus" "content" "UTF-8"
          result = lexSource file
      resultPhase result `shouldBe` Lexing
      resultSuccess result `shouldBe` True
      resultOutput result `shouldBe` Just "Tokens generated"
      
    it "handles empty file lexing" $ do
      let file = SourceFile "empty.typus" "" "UTF-8"
          result = lexSource file
      resultPhase result `shouldBe` Lexing
      resultSuccess result `shouldBe` False
      resultErrors result `shouldBe` ["Empty source file"]
      
    it "handles parsing phase" $ do
      let tokens = [Token "Identifier" "x" (SourceSpan (SourcePos 1 1 0) (SourcePos 1 1 0))]
          result = parseTokens tokens
      resultPhase result `shouldBe` Parsing
      resultSuccess result `shouldBe` True
      resultOutput result `shouldBe` Just "AST generated"
      
    it "handles empty token parsing" $ do
      let tokens = []
          result = parseTokens tokens
      resultPhase result `shouldBe` Parsing
      resultSuccess result `shouldBe` False
      resultErrors result `shouldBe` ["No tokens to parse"]
      
    it "handles type checking phase" $ do
      let ast = ASTNode "Function" (Just "main") [] (SourceSpan (SourcePos 1 1 0) (SourcePos 1 10 9))
          result = checkTypes ast
      resultPhase result `shouldBe` TypeChecking
      resultSuccess result `shouldBe` True
      resultOutput result `shouldBe` Just "Type checking passed"
      
    it "handles empty AST type checking" $ do
      let ast = ASTNode "" Nothing [] (SourceSpan (SourcePos 1 1 0) (SourcePos 1 1 0))
          result = checkTypes ast
      resultPhase result `shouldBe` TypeChecking
      resultSuccess result `shouldBe` False
      resultErrors result `shouldBe` ["Empty AST"]
      
    it "handles optimization phase" $ do
      let ir = [IRInstruction "add" ["x", "y"] (Just "z")]
          result = optimizeIR ir
      resultPhase result `shouldBe` Optimization
      resultSuccess result `shouldBe` True
      resultOutput result `shouldBe` Just "IR optimized"
      
    it "handles empty IR optimization" $ do
      let ir = []
          result = optimizeIR ir
      resultPhase result `shouldBe` Optimization
      resultSuccess result `shouldBe` False
      resultErrors result `shouldBe` ["No IR to optimize"]
      
    it "handles code generation phase" $ do
      let ir = [IRInstruction "add" ["x", "y"] (Just "z")]
          result = generateCode ir
      resultPhase result `shouldBe` CodeGeneration
      resultSuccess result `shouldBe` True
      resultOutput result `shouldBe` Just "Code generated"
      
    it "handles empty IR code generation" $ do
      let ir = []
          result = generateCode ir
      resultPhase result `shouldBe` CodeGeneration
      resultSuccess result `shouldBe` False
      resultErrors result `shouldBe` ["No IR to generate code from"]

  describe "Error propagation" $ do
    it "propagates errors through pipeline" $ do
      let file = SourceFile "empty.typus" "" "UTF-8"
          pipeline = runCompilationPipeline file
          results = pipelineResults pipeline
          lexResult = getPhaseResult Lexing pipeline
          parseResult = getPhaseResult Parsing pipeline
      resultSuccess <$> lexResult `shouldBe` Just False
      resultSuccess <$> parseResult `shouldBe` Just False
      
    it "accumulates warnings through pipeline" $ do
      let largeContent = replicate 2000 'a'
          file = SourceFile "large.typus" largeContent "UTF-8"
          pipeline = runCompilationPipeline file
          results = pipelineResults pipeline
          allWarnings = concatMap resultWarnings results
      "Large source file" `elem` allWarnings `shouldBe` True

  describe "QuickCheck properties" $ do
    it "pipeline phase order is consistent" $ property $
      \file ->
        let pipeline = runCompilationPipeline file
            results = pipelineResults pipeline
            phases = map resultPhase results
        in phases `shouldBe` sort phases
        
    it "pipeline success requires all phases successful" $ property $
      \file ->
        let pipeline = runCompilationPipeline file
            results = pipelineResults pipeline
            successfulPhases = filter resultSuccess results
        in isPipelineSuccessful pipeline `shouldBe` (length successfulPhases == length results)
        
    it "phase retrieval is consistent" $ property $
      \file phase ->
        let pipeline = runCompilationPipeline file
            result = getPhaseResult phase pipeline
        in case result of
              Just r -> resultPhase r `shouldBe` phase
              Nothing -> True

  describe "Edge cases" $ do
    it "handles very large source files" $ do
      let largeContent = replicate 10000 'a'
          file = SourceFile "huge.typus" largeContent "UTF-8"
          result = lexSource file
      resultWarnings result `shouldContain` ["Large source file"]
      
    it "handles source files with special characters" $ do
      let specialContent = "\0\1\2\3"
          file = SourceFile "special.typus" specialContent "UTF-8"
          result = lexSource file
      resultSuccess result `shouldBe` True
      
    it "handles malformed source files" $ do
      let malformedContent = "unbalanced { [ ("
          file = SourceFile "malformed.typus" malformedContent "UTF-8"
          pipeline = runCompilationPipeline file
          parseResult = getPhaseResult Parsing pipeline
      resultSuccess <$> parseResult `shouldBe` Just False
      
    it "handles circular dependencies" $ do
      let circularContent = "module A imports B\nmodule B imports A"
          file = SourceFile "circular.typus" circularContent "UTF-8"
          pipeline = runCompilationPipeline file
          typeResult = getPhaseResult TypeChecking pipeline
      resultSuccess <$> typeResult `shouldBe` Just False
      
    it "handles pipeline interruption" $ do
      let file = SourceFile "test.typus" "content" "UTF-8"
          pipeline = runCompilationPipeline file
          -- Simulate interruption by modifying results
          interruptedResults = take 3 $ pipelineResults pipeline
          interruptedPipeline = pipeline { pipelineResults = interruptedResults }
      length (pipelineResults interruptedPipeline) @?= 3
      isPipelineSuccessful interruptedPipeline @?= False
  ]
  ]