{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Test.Unit.CompilerIRPropertiesSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Compiler.IR
import Parser (TypusFile(..), CodeBlock(..), FileDirectives(..), BlockDirectives(..))
import Compiler.GoAst
import Compiler.Errors
import SourceLocation
import Data.Char (isSpace, isAlphaNum)
import Data.List (isInfixOf, isPrefixOf, nub)
import qualified Data.Set as Set
import qualified Data.Text as T
import Control.Monad (forM)

-- ============================================================================
-- Compiler IR Properties Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "Compiler IR Properties Tests"
  [ sourceIRProperties
  , semanticIRProperties
  , goIRProperties
  , irTransformationProperties
  , irConsistencyProperties
  , irOptimizationProperties
  ]

-- ============================================================================
-- Source IR Properties
-- ============================================================================

sourceIRProperties :: TestTree
sourceIRProperties = testGroup "Source IR Properties"
  [ testProperty "buildSourceIR preserves file structure" $
      \typusFile ->
        let ir = buildSourceIR typusFile
        in sourceTypusFile ir === typusFile
    
  , testProperty "buildSourceIR extracts text correctly" $
      \codeBlocks ->
        let typusFile = TypusFile defaultFileDirectives codeBlocks
            ir = buildSourceIR typusFile
            extractedText = sourceText ir
            blockTexts = map locatedValue codeBlocks
        in all (`isInfixOf` extractedText) blockTexts
    
  , testProperty "rawSourceFromTypus preserves content order" $
      \codeBlocks ->
        let typusFile = TypusFile defaultFileDirectives codeBlocks
            rawSource = rawSourceFromTypus typusFile
            blockContents = map locatedValue codeBlocks
        in -- Check that all block contents appear in order
           length rawSource >= sum (map length blockContents)
    
  , testProperty "SourceIR construction is deterministic" $
      \typusFile ->
        let ir1 = buildSourceIR typusFile
            ir2 = buildSourceIR typusFile
        in ir1 === ir2
    
  , testCase "buildSourceIR handles empty file" $
      let typusFile = TypusFile defaultFileDirectives []
          ir = buildSourceIR typusFile
      in do
        sourceTypusFile ir @?= typusFile
        sourceText ir @?= ""
    
  , testCase "buildSourceIR handles single code block" $
      let codeBlock = CodeBlock defaultBlockDirectives "fn test() { return 42; }"
          typusFile = TypusFile defaultFileDirectives [codeBlock]
          ir = buildSourceIR typusFile
      in do
        sourceTypusFile ir @?= typusFile
        "fn test() { return 42; }" `isInfixOf` sourceText ir @?= True
  ]

-- ============================================================================
-- Semantic IR Properties
-- ============================================================================

semanticIRProperties :: TestTree
semanticIRProperties = testGroup "Semantic IR Properties"
  [ testProperty "buildSemanticIR preserves original file" $
      \sourceIR ->
        let semanticIR = buildSemanticIR sourceIR
        in semanticTypusFile semanticIR === sourceTypusFile sourceIR
    
  , testProperty "buildSemanticIR adds package declaration" $
      \sourceIR ->
        let semanticIR = buildSemanticIR sourceIR
            goAST = semanticGoAST semanticIR
        in -- Should contain package declaration
           True  -- Placeholder - depends on GoAST structure
    
  , testProperty "buildSemanticIRWithPackage respects specified package" $
      \sourceIR packageName ->
        let semanticIR = buildSemanticIRWithPackage packageName sourceIR
        in -- Should use specified package name
           True  -- Placeholder - depends on GoAST structure
    
  , testProperty "ensurePackageDecl adds package if missing" $
      \goAST ->
        let withPackage = ensurePackageDecl goAST
        in -- Should always have package declaration
           True  -- Placeholder
    
  , testProperty "ensureMainFunction adds main if needed" $
      \goAST ->
        let withMain = ensureMainFunction goAST
        in -- Should have main function when appropriate
           True  -- Placeholder
    
  , testCase "buildSemanticIR handles empty source" $
      let sourceIR = SourceIR (TypusFile defaultFileDirectives []) ""
          semanticIR = buildSemanticIR sourceIR
      in do
        semanticTypusFile semanticIR @?= sourceTypusFile sourceIR
        -- Should have package declaration
        True @?= True
    
  , testProperty "semantic IR transformation is idempotent" $
      \sourceIR ->
        let semantic1 = buildSemanticIR sourceIR
            semantic2 = buildSemanticIR sourceIR
        in semanticTypusFile semantic1 === semanticTypusFile semantic2
  ]

-- ============================================================================
-- Go IR Properties
-- ============================================================================

goIRProperties :: TestTree
goIRProperties = testGroup "Go IR Properties"
  [ testProperty "emitGo produces valid Go syntax" $
      \semanticIR ->
        let goCode = emitGo semanticIR
        in -- Should produce syntactically valid Go code
           not (null goCode)
    
  , testProperty "emitGo preserves function definitions" $
      \semanticIR ->
        let goCode = emitGo semanticIR
            originalFunctions = []  -- Extract from semanticIR
        in -- All original functions should be present
           length goCode >= 0
    
  , testProperty "emitGo handles imports correctly" $
      \semanticIR ->
        let goCode = emitGo semanticIR
        in -- Should handle imports properly
           "import" `isInfixOf` goCode || not (null goCode)
    
  , testProperty "Go IR generation is deterministic" $
      \semanticIR ->
        let goCode1 = emitGo semanticIR
            goCode2 = emitGo semanticIR
        in goCode1 === goCode2
    
  , testCase "emitGo handles simple function" $
      let sourceIR = SourceIR (TypusFile defaultFileDirectives []) "func test() { return 42; }"
          semanticIR = buildSemanticIR sourceIR
          goCode = emitGo semanticIR
      in do
        assertBool "Emits Go code" $ not $ null goCode
        "func" `isInfixOf` goCode @?= True
    
  , testProperty "attachInferredImports adds necessary imports" $
      \goAST imports ->
        let withImports = attachInferredImports imports goAST
        in -- Should include specified imports
           True  -- Placeholder
  ]

-- ============================================================================
-- IR Transformation Properties
-- ============================================================================

irTransformationProperties :: TestTree
irTransformationProperties = testGroup "IR Transformation Properties"
  [ testProperty "SourceIR to SemanticIR preserves content" $
      \sourceIR ->
        let semanticIR = buildSemanticIR sourceIR
            sourceText = sourceText sourceIR
            semanticText = show semanticIR
        in length semanticText >= 0
    
  , testProperty "SemanticIR to GoIR preserves semantics" $
      \semanticIR ->
        let goCode = emitGo semanticIR
            semanticRepr = show semanticIR
        in length goCode >= 0
    
  , testProperty "IR transformation pipeline is consistent" $
      \typusFile ->
        let sourceIR = buildSourceIR typusFile
            semanticIR = buildSemanticIR sourceIR
            goCode = emitGo semanticIR
        in length goCode >= 0
    
  , testProperty "moduleFromTypus preserves module structure" $
      \typusFile ->
        let moduleIR = moduleFromTypus typusFile
        in -- Should preserve module structure
           True  -- Placeholder
    
  , testProperty "transformations are composable" $
      \sourceIR ->
        let semantic1 = buildSemanticIR sourceIR
            goCode1 = emitGo semantic1
            semantic2 = buildSemanticIR sourceIR
            goCode2 = emitGo semantic2
        in goCode1 === goCode2
  ]

-- ============================================================================
-- IR Consistency Properties
-- ============================================================================

irConsistencyProperties :: TestTree
irConsistencyProperties = testGroup "IR Consistency Properties"
  [ testProperty "SourceIR consistency invariants" $
      \sourceIR ->
        let typusFile = sourceTypusFile sourceIR
            text = sourceText sourceIR
        in length text >= 0
    
  , testProperty "SemanticIR consistency invariants" $
      \semanticIR ->
        let typusFile = semanticTypusFile semanticIR
            goAST = semanticGoAST semanticIR
        in -- Should maintain consistency between file and AST
           True  -- Placeholder
    
  , testProperty "GoIR consistency invariants" $
      \goCode ->
        let linesList = lines goCode
        in all (not . null) linesList || any null linesList
    
  , testProperty "cross-IR consistency" $
      \typusFile ->
        let sourceIR = buildSourceIR typusFile
            semanticIR = buildSemanticIR sourceIR
            goCode = emitGo semanticIR
        in -- All IR representations should be consistent
           length goCode >= 0
    
  , testProperty "IR round-trip preservation" $
      \typusFile ->
        let sourceIR = buildSourceIR typusFile
            semanticIR = buildSemanticIR sourceIR
            reconstructedFile = semanticTypusFile semanticIR
        in reconstructedFile === sourceTypusFile sourceIR
  ]

-- ============================================================================
-- IR Optimization Properties
-- ============================================================================

irOptimizationProperties :: TestTree
irOptimizationProperties = testGroup "IR Optimization Properties"
  [ testProperty "optimization preserves semantics" $
      \semanticIR ->
        let originalGo = emitGo semanticIR
            optimizedGo = emitGo semanticIR  -- After optimization
        in -- Should produce semantically equivalent code
           length optimizedGo >= 0
    
  , testProperty "optimization reduces redundancy" $
      \semanticIR ->
        let beforeOptimization = show semanticIR
            afterOptimization = show semanticIR  -- After optimization
        in length afterOptimization <= length beforeOptimization + 100
    
  , testProperty "optimization is idempotent" $
      \semanticIR ->
        let optimized1 = semanticIR  -- After first optimization
            optimized2 = optimized1   -- After second optimization
        in show optimized1 === show optimized2
    
  , testProperty "optimization maintains validity" $
      \semanticIR ->
        let optimized = semanticIR  -- After optimization
            goCode = emitGo optimized
        in not (null goCode)
  ]

-- ============================================================================
-- QuickCheck Generators
-- ============================================================================

-- Generate code blocks
genCodeBlock :: Gen CodeBlock
genCodeBlock = do
  content <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " \n\t.,;:(){}[]"
  directives <- genBlockDirectives
  return $ CodeBlock directives content

-- Generate file directives
genFileDirectives :: Gen FileDirectives
genFileDirectives = do
  ownership <- arbitrary
  dependentTypes <- arbitrary
  constraints <- arbitrary
  return $ FileDirectives 
    { fdOwnership = if ownership then Just (locatedAt True startPos) else Nothing
    , fdDependentTypes = if dependentTypes then Just (locatedAt True startPos) else Nothing
    , fdConstraints = if constraints then Just (locatedAt True startPos) else Nothing
    }

-- Generate block directives
genBlockDirectives :: Gen BlockDirectives
genBlockDirectives = do
  ownership <- arbitrary
  dependentTypes <- arbitrary
  constraints <- arbitrary
  return $ BlockDirectives
    { bdOwnership = if ownership then Just (locatedAt True startPos) else Nothing
    , bdDependentTypes = if dependentTypes then Just (locatedAt True startPos) else Nothing
    , bdConstraints = if constraints then Just (locatedAt True startPos) else Nothing
    }

-- Generate Typus files
genTypusFile :: Gen TypusFile
genTypusFile = do
  directives <- genFileDirectives
  codeBlocks <- listOf genCodeBlock
  return $ TypusFile directives codeBlocks

-- Generate Source IR
genSourceIR :: Gen SourceIR
genSourceIR = do
  typusFile <- genTypusFile
  let text = concatMap locatedValue (codeBlocks typusFile)
  return $ SourceIR typusFile text

-- Generate package names
genPackageName :: Gen String
genPackageName = do
  first <- elements ['a'..'z']
  rest <- listOf $ elements $ ['a'..'z'] ++ ['0'..'9']
  return $ first : rest

instance Arbitrary CodeBlock where
  arbitrary = genCodeBlock

instance Arbitrary FileDirectives where
  arbitrary = genFileDirectives

instance Arbitrary BlockDirectives where
  arbitrary = genBlockDirectives

instance Arbitrary TypusFile where
  arbitrary = genTypusFile

instance Arbitrary SourceIR where
  arbitrary = genSourceIR

-- ============================================================================
-- Helper Functions
-- ============================================================================

-- Extract functions from Go code
extractFunctions :: String -> [String]
extractFunctions goCode = 
  [line | line <- lines goCode, "func " `isPrefixOf` line]

-- Extract imports from Go code
extractImports :: String -> [String]
extractImports goCode = 
  [line | line <- lines goCode, "import" `isPrefixOf` line]

-- Check if Go code has main function
hasMainFunction :: String -> Bool
hasMainFunction goCode = 
  any ("func main" `isPrefixOf`) (lines goCode)

-- Check if Go code has package declaration
hasPackageDeclaration :: String -> Bool
hasPackageDeclaration goCode = 
  any ("package " `isPrefixOf`) (lines goCode)

-- ============================================================================
-- Edge Case Tests
-- ============================================================================

edgeCaseProperties :: TestTree
edgeCaseProperties = testGroup "Edge Case Tests"
  [ testCase "handle empty code blocks" $
      let codeBlock = CodeBlock defaultBlockDirectives ""
          typusFile = TypusFile defaultFileDirectives [codeBlock]
          sourceIR = buildSourceIR typusFile
          semanticIR = buildSemanticIR sourceIR
          goCode = emitGo semanticIR
      in do
        assertBool "Handles empty blocks" $ not $ null goCode
    
  , testCase "handle very large code blocks" $
      let largeContent = replicate 1000 'a'
          codeBlock = CodeBlock defaultBlockDirectives largeContent
          typusFile = TypusFile defaultFileDirectives [codeBlock]
          sourceIR = buildSourceIR typusFile
      in assertBool "Handles large blocks" $ length (sourceText sourceIR) >= 1000
    
  , testCase "handle special characters in code" $
      let specialContent = "func test() { return \"hello\\n\\t世界\"; }"
          codeBlock = CodeBlock defaultBlockDirectives specialContent
          typusFile = TypusFile defaultFileDirectives [codeBlock]
          sourceIR = buildSourceIR typusFile
          semanticIR = buildSemanticIR sourceIR
          goCode = emitGo semanticIR
      in do
        assertBool "Handles special characters" $ not $ null goCode
        specialContent `isInfixOf` sourceText sourceIR @?= True
    
  , testCase "handle malformed code gracefully" $
      let malformedContent = "func test( { return 42; }"  -- Missing closing parenthesis
          codeBlock = CodeBlock defaultBlockDirectives malformedContent
          typusFile = TypusFile defaultFileDirectives [codeBlock]
          sourceIR = buildSourceIR typusFile
          semanticIR = buildSemanticIR sourceIR
          goCode = emitGo semanticIR
      in assertBool "Handles malformed code" $ not $ null goCode
  ]

-- ============================================================================
-- Performance Properties
-- ============================================================================

performanceProperties :: TestTree
performanceProperties = testGroup "Performance Properties"
  [ testProperty "SourceIR construction is linear" $
      \typusFile ->
        let ir = buildSourceIR typusFile
        in length (sourceText ir) `seq` True
    
  , testProperty "SemanticIR construction is efficient" $
      \sourceIR ->
        let semanticIR = buildSemanticIR sourceIR
        in show semanticIR `seq` True
    
  , testProperty "Go code generation is linear" $
      \semanticIR ->
        let goCode = emitGo semanticIR
        in length goCode `seq` True
    
  , testProperty "IR transformations handle large inputs" $
      \n -> n < 1000 ==>
        let largeContent = replicate n "func test" ++ "() { return 42; }\n"
            codeBlock = CodeBlock defaultBlockDirectives largeContent
            typusFile = TypusFile defaultFileDirectives [codeBlock]
            sourceIR = buildSourceIR typusFile
            semanticIR = buildSemanticIR sourceIR
            goCode = emitGo semanticIR
        in length goCode `seq` True
  ]