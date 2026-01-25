{-# LANGUAGE OverloadedStrings #-}
module Test.Unit.IntegrationEndToEndSpec where


import Test.Tasty.HUnit
import Test.Tasty
import Test.Tasty.QuickCheck



import Test.Tasty

import Test.Tasty.QuickCheck
import Data.List (sort)
import SourceLocation (SourcePos(..), SourceSpan(..))
import Test.QuickCheck (Arbitrary(..), arbitrary, oneof, elements, listOf, sized, suchThat)

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

-- Arbitrary instances for QuickCheck testing
-- Arbitrary instance for SourcePos is now defined in SourceLocation module


-- Arbitrary instance for SourceSpan is now defined in SourceLocation module


instance Arbitrary SourceFile where
  arbitrary = do
    path <- arbitrary
    content <- arbitrary
    encoding <- elements ["UTF-8", "ASCII", "UTF-16"]
    return $ SourceFile path content encoding

instance Arbitrary Token where
  arbitrary = do
    tokenType <- elements ["Identifier", "Keyword", "Operator", "Literal", "EOF"]
    tokenValue <- arbitrary
    tokenSpan <- arbitrary
    return $ Token tokenType tokenValue tokenSpan

instance Arbitrary ASTNode where
  arbitrary = sized $ \n -> do
    nodeType <- elements ["Function", "Variable", "Literal", "Operator", "Root"]
    nodeValue <- oneof [return Nothing, arbitrary >>= return . Just]
    let childCount = min n 3
    nodeChildren <- listOf $ resize (n `div` 2) arbitrary
    nodeSpan <- arbitrary
    return $ ASTNode nodeType nodeValue nodeChildren nodeSpan

instance Arbitrary IRInstruction where
  arbitrary = do
    instructionOp <- elements ["add", "sub", "mul", "div", "load", "store", "nop"]
    instructionArgs <- listOf $ elements ["x", "y", "z", "temp1", "temp2"]
    instructionResult <- oneof [return Nothing, arbitrary >>= return . Just]
    return $ IRInstruction instructionOp instructionArgs instructionResult

instance Arbitrary CompilationPhase where
  arbitrary = elements [Lexing, Parsing, TypeChecking, Optimization, CodeGeneration]

instance Arbitrary CompilationResult where
  arbitrary = do
    resultPhase <- arbitrary
    resultSuccess <- arbitrary
    resultWarnings <- listOf arbitrary
    resultErrors <- listOf arbitrary
    resultOutput <- oneof [return Nothing, arbitrary >>= return . Just]
    return $ CompilationResult resultPhase resultSuccess resultWarnings resultErrors resultOutput

instance Arbitrary CompilationPipeline where
  arbitrary = do
    pipelineSource <- arbitrary
    pipelineTokens <- listOf arbitrary
    pipelineAST <- arbitrary
    pipelineIR <- listOf arbitrary
    pipelineResults <- listOf arbitrary
    return $ CompilationPipeline pipelineSource pipelineTokens pipelineAST pipelineIR pipelineResults

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
    ]

  , testGroup "AST nodes"
    [ testCase "creates AST nodes correctly" $ do
        let span = SourceSpan (SourcePos 1 1 0) (SourcePos 1 10 9)
            node = ASTNode "Function" (Just "main") [] span
        nodeType node @?= "Function"
        nodeValue node @?= Just "main"
        nodeChildren node @?= []
        nodeSpan node @?= span
      
    , testCase "handles AST nodes with children" $ do
        let span = SourceSpan (SourcePos 1 1 0) (SourcePos 1 20 19)
            child = ASTNode "Parameter" (Just "x") [] (SourceSpan (SourcePos 1 10 9) (SourcePos 1 15 14))
            node = ASTNode "Function" (Just "main") [child] span
        length (nodeChildren node) @?= 1
        case nodeChildren node of
          (c:_) -> c @?= child
          [] -> assertBool "Should have at least one child" False
      
    , testCase "handles empty AST nodes" $ do
        let span = SourceSpan (SourcePos 1 1 0) (SourcePos 1 1 0)
            node = ASTNode "" Nothing [] span
        nodeType node @?= ""
        nodeValue node @?= Nothing
        nodeChildren node @?= []
    ]

  , testGroup "IR instructions"
    [ testCase "creates IR instructions correctly" $ do
        let instruction = IRInstruction "add" ["x", "y"] (Just "z")
        instructionOp instruction @?= "add"
        instructionArgs instruction @?= ["x", "y"]
        instructionResult instruction @?= Just "z"
      
    , testCase "handles IR instructions without results" $ do
        let instruction = IRInstruction "store" ["x", "42"] Nothing
        instructionOp instruction @?= "store"
        instructionArgs instruction @?= ["x", "42"]
        instructionResult instruction @?= Nothing
      
    , testCase "handles IR instructions with no arguments" $ do
        let instruction = IRInstruction "nop" [] Nothing
        instructionOp instruction @?= "nop"
        instructionArgs instruction @?= []
        instructionResult instruction @?= Nothing
    ]

  , testGroup "Compilation results"
    [ testCase "creates successful compilation results" $ do
        let result = CompilationResult Lexing True [] [] (Just "Success")
        resultPhase result @?= Lexing
        resultSuccess result @?= True
        resultWarnings result @?= []
        resultErrors result @?= []
        resultOutput result @?= Just "Success"
      
    , testCase "creates failed compilation results" $ do
        let result = CompilationResult Parsing False ["Warning"] ["Error"] Nothing
        resultPhase result @?= Parsing
        resultSuccess result @?= False
        resultWarnings result @?= ["Warning"]
        resultErrors result @?= ["Error"]
        resultOutput result @?= Nothing
    ]

  , testGroup "Compilation pipeline"
    [ testCase "creates compilation pipeline correctly" $ do
        let file = SourceFile "test.typus" "content" "UTF-8"
            tokens = []
            ast = ASTNode "Root" Nothing [] (SourceSpan (SourcePos 1 1 0) (SourcePos 1 1 0))
            ir = []
            results = []
            pipeline = CompilationPipeline file tokens ast ir results
        pipelineSource pipeline @?= file
        pipelineTokens pipeline @?= tokens
        pipelineAST pipeline @?= ast
        pipelineIR pipeline @?= ir
        pipelineResults pipeline @?= results
      
    , testCase "runs compilation pipeline correctly" $ do
        let file = SourceFile "test.typus" "content" "UTF-8"
            pipeline = runCompilationPipeline file
            results = pipelineResults pipeline
        length results @?= 5
        map resultPhase results @?= [Lexing, Parsing, TypeChecking, Optimization, CodeGeneration]
      
    , testCase "gets phase results correctly" $ do
        let file = SourceFile "test.typus" "content" "UTF-8"
            pipeline = runCompilationPipeline file
            lexResult = getPhaseResult Lexing pipeline
            parseResult = getPhaseResult Parsing pipeline
        resultPhase <$> lexResult @?= Just Lexing
        resultPhase <$> parseResult @?= Just Parsing
      
    , testCase "checks pipeline success correctly" $ do
        let file = SourceFile "test.typus" "content" "UTF-8"
            pipeline = runCompilationPipeline file
        isPipelineSuccessful pipeline @?= True
    ]

  , testGroup "Phase-specific compilation"
    [ testCase "handles lexing phase" $ do
        let file = SourceFile "test.typus" "content" "UTF-8"
            result = lexSource file
        resultPhase result @?= Lexing
        resultSuccess result @?= True
        resultOutput result @?= Just "Tokens generated"
      
    , testCase "handles empty file lexing" $ do
        let file = SourceFile "empty.typus" "" "UTF-8"
            result = lexSource file
        resultPhase result @?= Lexing
        resultSuccess result @?= False
        resultErrors result @?= ["Empty source file"]
      
    , testCase "handles parsing phase" $ do
        let tokens = [Token "Identifier" "x" (SourceSpan (SourcePos 1 1 0) (SourcePos 1 1 0))]
            result = parseTokens tokens
        resultPhase result @?= Parsing
        resultSuccess result @?= True
        resultOutput result @?= Just "AST generated"
      
    , testCase "handles empty token parsing" $ do
        let tokens = []
            result = parseTokens tokens
        resultPhase result @?= Parsing
        resultSuccess result @?= False
        resultErrors result @?= ["No tokens to parse"]
      
    , testCase "handles type checking phase" $ do
        let ast = ASTNode "Function" (Just "main") [] (SourceSpan (SourcePos 1 1 0) (SourcePos 1 10 9))
            result = checkTypes ast
        resultPhase result @?= TypeChecking
        resultSuccess result @?= True
        resultOutput result @?= Just "Type checking passed"
      
    , testCase "handles empty AST type checking" $ do
        let ast = ASTNode "" Nothing [] (SourceSpan (SourcePos 1 1 0) (SourcePos 1 1 0))
            result = checkTypes ast
        resultPhase result @?= TypeChecking
        resultSuccess result @?= False
        resultErrors result @?= ["Empty AST"]
      
    , testCase "handles optimization phase" $ do
        let ir = [IRInstruction "add" ["x", "y"] (Just "z")]
            result = optimizeIR ir
        resultPhase result @?= Optimization
        resultSuccess result @?= True
        resultOutput result @?= Just "IR optimized"
      
    , testCase "handles empty IR optimization" $ do
        let ir = []
            result = optimizeIR ir
        resultPhase result @?= Optimization
        resultSuccess result @?= False
        resultErrors result @?= ["No IR to optimize"]
      
    , testCase "handles code generation phase" $ do
        let ir = [IRInstruction "add" ["x", "y"] (Just "z")]
            result = generateCode ir
        resultPhase result @?= CodeGeneration
        resultSuccess result @?= True
        resultOutput result @?= Just "Code generated"
      
    , testCase "handles empty IR code generation" $ do
        let ir = []
            result = generateCode ir
        resultPhase result @?= CodeGeneration
        resultSuccess result @?= False
        resultErrors result @?= ["No IR to generate code from"]
    ]

  , testGroup "Error propagation"
    [ testCase "propagates errors through pipeline" $ do
        let file = SourceFile "empty.typus" "" "UTF-8"
            pipeline = runCompilationPipeline file
            results = pipelineResults pipeline
            lexResult = getPhaseResult Lexing pipeline
            parseResult = getPhaseResult Parsing pipeline
        resultSuccess <$> lexResult @?= Just False
        resultSuccess <$> parseResult @?= Just False
      
    , testCase "accumulates warnings through pipeline" $ do
        let largeContent = replicate 2000 'a'
            file = SourceFile "large.typus" largeContent "UTF-8"
            pipeline = runCompilationPipeline file
            results = pipelineResults pipeline
            allWarnings = concatMap resultWarnings results
        "Large source file" `elem` allWarnings @?= True
    ]

  , testGroup "QuickCheck properties"
    [ testProperty "pipeline phase order is consistent" $ \file ->
        let pipeline = runCompilationPipeline file
            results = pipelineResults pipeline
            phases = map resultPhase results
        in phases == sort phases
        
    , testProperty "pipeline success requires all phases successful" $ \file ->
        let pipeline = runCompilationPipeline file
            results = pipelineResults pipeline
            successfulPhases = filter resultSuccess results
        in isPipelineSuccessful pipeline == (length successfulPhases == length results)
        
    , testProperty "phase retrieval is consistent" $ \file phase ->
        let pipeline = runCompilationPipeline file
            result = getPhaseResult phase pipeline
        in case result of
              Just r -> resultPhase r == phase
              Nothing -> True
    ]

  , testGroup "Edge cases"
    [ testCase "handles very large source files" $ do
        let largeContent = replicate 10000 'a'
            file = SourceFile "huge.typus" largeContent "UTF-8"
            result = lexSource file
        "Large source file" `elem` (resultWarnings result) @?= True
      
    , testCase "handles source files with special characters" $ do
        let specialContent = "\0\1\2\3"
            file = SourceFile "special.typus" specialContent "UTF-8"
            result = lexSource file
        resultSuccess result @?= True
      
    , testCase "handles malformed source files" $ do
        let malformedContent = "unbalanced { [ ("
            file = SourceFile "malformed.typus" malformedContent "UTF-8"
            pipeline = runCompilationPipeline file
            parseResult = getPhaseResult Parsing pipeline
        resultSuccess <$> parseResult @?= Just False
      
    , testCase "handles circular dependencies" $ do
        let circularContent = "module A imports B\nmodule B imports A"
            file = SourceFile "circular.typus" circularContent "UTF-8"
            pipeline = runCompilationPipeline file
            typeResult = getPhaseResult TypeChecking pipeline
        resultSuccess <$> typeResult @?= Just False
      
    , testCase "handles pipeline interruption" $ do
        let file = SourceFile "test.typus" "content" "UTF-8"
            pipeline = runCompilationPipeline file
            -- Simulate interruption by modifying results
            interruptedResults = take 3 $ pipelineResults pipeline
            interruptedPipeline = pipeline { pipelineResults = interruptedResults }
        length (pipelineResults interruptedPipeline) @?= 3
        isPipelineSuccessful interruptedPipeline @?= False
    ]
  ]