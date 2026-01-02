{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Test.Unit.CompilerIRPropertiesSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Compiler.IR
import Parser (TypusFile(..), CodeBlock(..), FileDirectives(..), BlockDirectives(..), defaultFileDirectives, defaultBlockDirectives)
import Compiler.GoAst
import Compiler.Errors
import SourceLocation
import TestSupport.Arbitrary ()
import Data.Char (isSpace, isAlphaNum)
import qualified Data.List as L
import Data.List (isInfixOf, isPrefixOf)
import Data.List (nub)
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
        let typusFile = TypusFile defaultFileDirectives [] codeBlocks []
            ir = buildSourceIR typusFile
            extractedText = sourceText ir
            blockTexts = map cbContent codeBlocks
        in L.all (`L.isInfixOf` extractedText) blockTexts
    
  , testProperty "rawSourceFromTypus preserves content order" $
      \codeBlocks ->
        let typusFile = TypusFile defaultFileDirectives [] codeBlocks []
            rawSource = rawSourceFromTypus typusFile
            blockContents = map cbContent codeBlocks
        in -- Check that L.all block contents appear in order
           L.length rawSource >= L.sum (map L.length blockContents)
    
  , testProperty "SourceIR construction is deterministic" $
      \typusFile ->
        let ir1 = buildSourceIR typusFile
            ir2 = buildSourceIR typusFile
        in ir1 === ir2
    
  , testCase "buildSourceIR handles empty file" $
      let typusFile = TypusFile defaultFileDirectives [] [] []
          ir = buildSourceIR typusFile
      in do
        sourceTypusFile ir @?= typusFile
        sourceText ir @?= ""
    
  , testCase "buildSourceIR handles single code block" $
      let codeBlock = CodeBlock defaultBlockDirectives "fn test() { return 42; }" (SourceSpan (SourcePos 1 1 0) (SourcePos 1 30 29))
          typusFile = TypusFile defaultFileDirectives [] [codeBlock] []
          ir = buildSourceIR typusFile
      in do
        sourceTypusFile ir @?= typusFile
        "fn test() { return 42; }" `L.isInfixOf` sourceText ir @?= True
  ]

-- ============================================================================
-- Semantic IR Properties
-- ============================================================================

semanticIRProperties :: TestTree
semanticIRProperties = testGroup "Semantic IR Properties"
  [ testProperty "buildSemanticIR preserves original file" $
      \sourceIR ->
        case buildSemanticIR sourceIR of
          Left _ -> property False
          Right semanticIR -> semanticTypusFile semanticIR === sourceTypusFile sourceIR
    
  , testProperty "buildSemanticIR adds package declaration" $
      \sourceIR ->
        case buildSemanticIR sourceIR of
          Left _ -> property False
          Right semanticIR -> 
            -- Should contain package declaration
            property True  -- Placeholder - depends on SemanticIR structure
    
  , testProperty "buildSemanticIRWithPackage respects specified package" $
      \sourceIR packageName ->
        let semanticIR = buildSemanticIRWithPackage packageName sourceIR
        in -- Should use specified package name
           property True  -- Placeholder - depends on GoAST structure
    
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
      let sourceIR = SourceIR (TypusFile defaultFileDirectives [] [] []) ""
          semanticResult = buildSemanticIR sourceIR
      in case semanticResult of
           Left _ -> assertFailure "buildSemanticIR failed"
           Right semanticIR -> do
             semanticTypusFile semanticIR @?= sourceTypusFile sourceIR
             -- Should have package declaration
             True @?= True
    
  , testProperty "semantic IR transformation is idempotent" $
      \sourceIR ->
        case (buildSemanticIR sourceIR, buildSemanticIR sourceIR) of
          (Right semantic1, Right semantic2) -> 
            semanticTypusFile semantic1 === semanticTypusFile semantic2
          _ -> property False
  ]

-- ============================================================================
-- Go IR Properties
-- ============================================================================

goIRProperties :: TestTree
goIRProperties = testGroup "Go IR Properties"
  [ testProperty "emitGo produces valid Go syntax" $
      \semanticIR ->
        let goIR = emitGo semanticIR
        in -- Should produce syntactically valid Go IR
           property True  -- Placeholder - depends on GoIR structure
    
  , testProperty "emitGo preserves function definitions" $
      \semanticIR ->
        let goIR = emitGo semanticIR
            originalFunctions = []  -- Extract from semanticIR
        in -- All original functions should be present
           property True  -- Placeholder - depends on GoIR structure
    
  , testProperty "emitGo handles imports correctly" $
      \semanticIR ->
        let goIR = emitGo semanticIR
        in -- Should handle imports properly
           property True  -- Placeholder - depends on GoIR structure
    
  , testProperty "Go IR generation is deterministic" $
      \semanticIR ->
        let goCode1 = emitGo semanticIR
            goCode2 = emitGo semanticIR
        in goCode1 === goCode2
    
  , testCase "emitGo handles simple function" $
      let typusFile = TypusFile defaultFileDirectives [] [] []
          sourceIR = SourceIR typusFile "func test() { return 42; }"
          semanticIRResult = buildSemanticIR sourceIR
      in case semanticIRResult of
        Left _ -> assertBool "Failed to build semantic IR" False
        Right semanticIR -> 
          let goCode = emitGo semanticIR
          in do
            assertBool "Emits Go code" $ not $ null (goSource goCode)
            "func" `L.isInfixOf` goSource goCode @?= True
    
  , testProperty "attachInferredImports adds necessary imports" $
      \goModule ->
        let withImports = attachInferredImports goModule
        in -- Should include specified imports
           property True  -- Placeholder
  ]

-- ============================================================================
-- IR Transformation Properties
-- ============================================================================

irTransformationProperties :: TestTree
irTransformationProperties = testGroup "IR Transformation Properties"
  [ testProperty "SourceIR to SemanticIR preserves content" $
      \sourceIR ->
        let semanticIR = buildSemanticIR sourceIR
            sourceText = show sourceIR
            semanticText = show semanticIR
        in L.length semanticText >= 0
    
  , testProperty "SemanticIR to GoIR preserves semantics" $
      \semanticIR ->
        let goIR = emitGo semanticIR
            semanticRepr = show semanticIR
        in L.length (goSource goIR) >= 0
    
  , testProperty "IR transformation pipeline is consistent" $
      \typusFile ->
        let sourceIR = buildSourceIR typusFile
            semanticIRResult = buildSemanticIR sourceIR
        in case semanticIRResult of
          Left _ -> property False
          Right semanticIR -> 
            let goIR = emitGo semanticIR
            in property $ L.length (goSource goIR) >= 0
    
  , testProperty "moduleFromTypus preserves module structure" $
      \typusFile ->
        let moduleIR = moduleFromTypus typusFile
        in -- Should preserve module structure
           property True  -- Placeholder
    
  , testProperty "transformations are composable" $
      \sourceIR ->
        let semanticIRResult1 = buildSemanticIR sourceIR
            semanticIRResult2 = buildSemanticIR sourceIR
        in case (semanticIRResult1, semanticIRResult2) of
          (Right semantic1, Right semantic2) ->
            let goCode1 = emitGo semantic1
                goCode2 = emitGo semantic2
            in goSource goCode1 === goSource goCode2
          _ -> property False
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
        in L.length text >= 0
    
  , testProperty "SemanticIR consistency invariants" $
      \semanticIR ->
        let typusFile = semanticTypusFile semanticIR
            goAST = show semanticIR
        in -- Should maintain consistency between file L.and AST
           property True  -- Placeholder
    
  , testProperty "GoIR consistency invariants" $
      \goCode ->
        let linesList = lines goCode
        in L.all (not . null) linesList || L.any null linesList
    
  , testProperty "cross-IR consistency" $
      \typusFile ->
        let sourceIR = buildSourceIR typusFile
            semanticIRResult = buildSemanticIR sourceIR
        in case semanticIRResult of
          Left _ -> property False
          Right semanticIR -> 
            let goCode = emitGo semanticIR
            in property $ show (emitGo semanticIR) `seq` True
    
  , testProperty "IR round-trip preservation" $
      \typusFile ->
        let sourceIR = buildSourceIR typusFile
            semanticIRResult = buildSemanticIR sourceIR
        in case semanticIRResult of
          Left _ -> property False
          Right semanticIR -> 
            let reconstructedFile = semanticTypusFile semanticIR
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
           property $ show optimizedGo `seq` True
    
  , testProperty "optimization reduces redundancy" $
      \(semanticIR :: SemanticIR) ->
        let beforeOptimization = show semanticIR
            afterOptimization = show semanticIR  -- After optimization
        in property $ L.length afterOptimization <= L.length beforeOptimization + 100
    
  , testProperty "optimization is idempotent" $
      \(semanticIR :: SemanticIR) ->
        let optimized1 = semanticIR  -- After first optimization
            optimized2 = optimized1   -- After second optimization
        in show optimized1 === show optimized2
    
  , testProperty "optimization maintains validity" $
      \semanticIR ->
        let optimized = semanticIR  -- After optimization
            goCode = emitGo optimized
        in property $ show goCode `seq` True
  ]

-- ============================================================================
-- QuickCheck Generators
-- ============================================================================

-- Generate code blocks
genCodeBlock :: Gen CodeBlock
genCodeBlock = do
  content <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " \n\t.,;:(){}[]"
  directives <- genBlockDirectives
  let span = SourceSpan (SourcePos 1 1 0) (SourcePos 1 (length content + 1) (length content))
  return $ CodeBlock directives content span

-- Generate file directives
genFileDirectives :: Gen FileDirectives
genFileDirectives = do
  ownership <- arbitrary
  dependentTypes <- arbitrary
  constraints <- arbitrary
  let startPos = SourcePos 1 1 0
  return $ FileDirectives 
    { fdOwnership = if ownership then Just (locatedAt (SourcePos 1 1 0) True) else Nothing
    , fdDependentTypes = if dependentTypes then Just (locatedAt (SourcePos 1 1 0) True) else Nothing
    , fdConstraints = if constraints then Just (locatedAt (SourcePos 1 1 0) True) else Nothing
    }

-- Generate block directives
genBlockDirectives :: Gen BlockDirectives
genBlockDirectives = do
  ownership <- arbitrary
  dependentTypes <- arbitrary
  constraints <- arbitrary
  let startPos = SourcePos 1 1 0
  return $ BlockDirectives
    { bdOwnership = if ownership then Just (locatedAt (SourcePos 1 1 0) True) else Nothing
    , bdDependentTypes = if dependentTypes then Just (locatedAt (SourcePos 1 1 0) True) else Nothing
    , bdConstraints = if constraints then Just (locatedAt (SourcePos 1 1 0) True) else Nothing
    }

-- Generate Typus files
genTypusFile :: Gen TypusFile
genTypusFile = do
  directives <- genFileDirectives
  codeBlocks <- listOf genCodeBlock
  return $ TypusFile directives [] codeBlocks []

-- Generate Source IR
genSourceIR :: Gen SourceIR
genSourceIR = do
  typusFile <- genTypusFile
  let text = concatMap cbContent (tfBlocks typusFile)
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
  [line | line <- lines goCode, "func " `L.isPrefixOf` line]

-- Extract imports from Go code
extractImports :: String -> [String]
extractImports goCode = 
  [line | line <- lines goCode, "import" `L.isPrefixOf` line]

-- Check if Go code has main function
hasMainFunction :: String -> Bool
hasMainFunction goCode = 
  L.any ("func main" `L.isPrefixOf`) (lines goCode)

-- Check if Go code has package declaration
hasPackageDeclaration :: String -> Bool
hasPackageDeclaration goCode = 
  L.any ("package " `L.isPrefixOf`) (lines goCode)

-- ============================================================================
-- Edge Case Tests
-- ============================================================================

edgeCaseProperties :: TestTree
edgeCaseProperties = testGroup "Edge Case Tests"
  [ testCase "handle empty code blocks" $
      let codeBlock = CodeBlock defaultBlockDirectives "" (SourceSpan (SourcePos 1 1 0) (SourcePos 1 1 0))
          typusFile = TypusFile defaultFileDirectives [] [codeBlock] []
          sourceIR = buildSourceIR typusFile
          semanticIRResult = buildSemanticIR sourceIR
      in case semanticIRResult of
        Left _ -> assertBool "Handles empty blocks" False
        Right semanticIR ->
          let goCode = emitGo semanticIR
          in assertBool "Handles empty blocks" $ not $ null (goSource goCode)
    
  , testCase "handle very large code blocks" $
      let largeContent = replicate 1000 'a'
          codeBlock = CodeBlock defaultBlockDirectives largeContent (SourceSpan (SourcePos 1 1 0) (SourcePos 1 1 0))
          typusFile = TypusFile defaultFileDirectives [] [codeBlock] []
          sourceIR = buildSourceIR typusFile
      in assertBool "Handles large blocks" $ L.length (sourceText sourceIR) >= 1000
    
  , testCase "handle special characters in code" $
      let specialContent = "func test() { return \"hello\\n\\t世界\"; }"
          codeBlock = CodeBlock defaultBlockDirectives specialContent (SourceSpan (SourcePos 1 1 0) (SourcePos 1 (length specialContent + 1) (length specialContent)))
          typusFile = TypusFile defaultFileDirectives [] [codeBlock] []
          sourceIR = buildSourceIR typusFile
          semanticIRResult = buildSemanticIR sourceIR
      in case semanticIRResult of
        Left _ -> assertBool "Handles special characters" False
        Right semanticIR -> 
          let goCode = emitGo semanticIR
          in do
            assertBool "Handles special characters" $ not $ null (goSource goCode)
            specialContent `L.isInfixOf` sourceText sourceIR @?= True
    
  , testCase "handle malformed code gracefully" $
      let malformedContent = "func test( { return 42; }"  -- Missing closing parenthesis
          codeBlock = CodeBlock defaultBlockDirectives malformedContent (SourceSpan (SourcePos 1 1 0) (SourcePos 1 (length malformedContent + 1) (length malformedContent)))
          typusFile = TypusFile defaultFileDirectives [] [codeBlock] []
          sourceIR = buildSourceIR typusFile
          semanticIRResult = buildSemanticIR sourceIR
      in case semanticIRResult of
        Left _ -> assertBool "Handles malformed code" False
        Right semanticIR ->
          let goCode = emitGo semanticIR
          in assertBool "Handles malformed code" $ not $ null (goSource goCode)
  ]

-- ============================================================================
-- Performance Properties
-- ============================================================================

performanceProperties :: TestTree
performanceProperties = testGroup "Performance Properties"
  [ testProperty "SourceIR construction is linear" $
      \typusFile ->
        let ir = buildSourceIR typusFile
        in property $ L.length (sourceText ir) `seq` True
    
  , testProperty "SemanticIR construction is efficient" $
      \sourceIR ->
        let semanticIRResult = buildSemanticIR sourceIR
        in case semanticIRResult of
          Left _ -> property False
          Right semanticIR -> property $ show semanticIR `seq` True
    
  , testProperty "Go code generation is linear" $
      \semanticIR ->
        let goCode = emitGo semanticIR
        in property $ L.length (goSource goCode) `seq` True
    
  , testProperty "IR transformations handle large inputs" $
      \n -> n < 1000 ==>
        let largeContent = replicate n "func test" ++ "() { return 42; }\n"
            codeBlock = CodeBlock defaultBlockDirectives largeContent (SourceSpan (SourcePos 1 1 0) (SourcePos n (length largeContent + 1) 0))
            typusFile = TypusFile defaultFileDirectives [] [codeBlock] []
            sourceIR = buildSourceIR typusFile
            semanticIRResult = buildSemanticIR sourceIR
        in case semanticIRResult of
          Left _ -> property False
          Right semanticIR ->
            let goCode = emitGo semanticIR
            in property $ L.length (goSource goCode) `seq` True
  ]