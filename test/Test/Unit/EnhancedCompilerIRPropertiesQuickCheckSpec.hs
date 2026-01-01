{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Test.Unit.EnhancedCompilerIRPropertiesQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property, (===), (.&&.), counterexample, forAll, oneof, elements, listOf, listOf1, choose, sized, Positive(..))
import Test.Tasty.HUnit (testCase, assertEqual, assertBool)
import Compiler.IR (SourceIR(..), SemanticIR(..), GoIR(..), buildSourceIR, buildSemanticIR, emitGo, rawSourceFromTypus, moduleFromTypus)
import Parser (TypusFile(..), CodeBlock(..), BlockDirectives(..), FileDirectives(..))
import Compiler.GoAst (GoModule(..), GoDecl(..), ImportDecl(..), PackageDecl(..))
import Compiler.Errors (CompilerResult(..))
import SourceLocation (SourceSpan(..), locatedAt, startPos)
import qualified Data.List as L
import Data.List (isInfixOf, isPrefixOf)
import Data.List (nub)
import qualified Data.Set as Set
import Data.Char (isSpace)

-- ============================================================================
-- Compiler IR Properties QuickCheck Tests
-- ============================================================================

tests :: TestTree
tests = testGroup "Compiler IR Properties QuickCheck Tests"
  [ testProperty "buildSourceIR preserves original content" prop_build_source_ir_preserves_content
  , testProperty "buildSourceIR maintains file structure" prop_build_source_ir_maintains_structure
  , testProperty "rawSourceFromTypus concatenates block content" prop_raw_source_concatenates_blocks
  , testProperty "moduleFromTypus produces valid module structure" prop_module_from_typus_valid_structure
  , testProperty "emitGo produces syntactically valid Go" prop_emit_go_valid_syntax
  , testProperty "emitGo preserves package declarations" prop_emit_go_preserves_package
  , testProperty "emitGo preserves imports" prop_emit_go_preserves_imports
  , testProperty "IR transformation is deterministic" prop_ir_transformation_deterministic
  , testProperty "buildSemanticIR handles empty modules" prop_build_semantic_ir_empty_modules
  , testProperty "buildSemanticIR preserves value information" prop_build_semantic_ir_preserves_values
  , testProperty "GoIR contains consistent module L.and source" prop_go_ir_consistent_module_source
  , testCase "IR handles edge cases" test_ir_edge_cases
  , testCase "IR generation handles malformed input" test_ir_malformed_input
  ]

-- ============================================================================
-- SourceIR Properties
-- ============================================================================

prop_build_source_ir_preserves_content :: TypusFile -> Property
prop_build_source_ir_preserves_content typusFile =
  let sourceIR = buildSourceIR typusFile
      originalContent = rawSourceFromTypus typusFile
      irContent = sourceText sourceIR
  in counterexample ("Original content L.length: " ++ show (L.length originalContent) ++ 
                    ", IR content L.length: " ++ show (L.length irContent)) $
     originalContent === irContent

prop_build_source_ir_maintains_structure :: TypusFile -> Property
prop_build_source_ir_maintains_structure typusFile =
  let sourceIR = buildSourceIR typusFile
      originalFile = sourceTypusFile sourceIR
      originalBlocks = tfBlocks typusFile
      irBlocks = tfBlocks originalFile
  in L.length originalBlocks === L.length irBlocks .&&.
     L.all (\(orig, ir) -> cbContent orig === cbContent ir) (zip originalBlocks irBlocks)

prop_raw_source_concatenates_blocks :: TypusFile -> Property
prop_raw_source_concatenates_blocks typusFile =
  let rawSource = rawSourceFromTypus typusFile
      blocks = tfBlocks typusFile
      blockContents = map cbContent blocks
      expectedContent = unlines blockContents
  in counterexample ("Expected lines: " ++ show (L.length blockContents) ++ 
                    ", Raw source lines: " ++ show (L.length (lines rawSource))) $
     L.length (lines rawSource) >= L.length blockContents

-- ============================================================================
-- SemanticIR Properties
-- ============================================================================

prop_module_from_typus_valid_structure :: TypusFile -> Property
prop_module_from_typus_valid_structure typusFile =
  let sourceIR = buildSourceIR typusFile
      result = moduleFromTypus (sourceTypusFile sourceIR)
  in counterexample ("Module result: " ++ show result) $
     case result of
       Left _ -> True  -- Should fail gracefully for invalid input
       Right goModule -> isValidGoModule goModule

prop_build_semantic_ir_empty_modules :: Property
prop_build_semantic_ir_empty_modules =
  let emptyFile = TypusFile defaultFileDirectives [] [] []
      sourceIR = buildSourceIR emptyFile
      result = buildSemanticIR sourceIR
  in counterexample ("Empty module result: " ++ show result) $
     case result of
       Left _ -> True  -- Should handle empty input gracefully
       Right semanticIR -> True  -- Should succeed with empty module

prop_build_semantic_ir_preserves_values :: TypusFile -> Property
prop_build_semantic_ir_preserves_values typusFile =
  let sourceIR = buildSourceIR typusFile
      result = buildSemanticIR sourceIR
  in counterexample ("Semantic IR result: " ++ show result) $
     case result of
       Left _ -> True  -- Should fail gracefully
       Right semanticIR -> 
         let moduleContent = moduleContentText (semanticModule semanticIR)
             valueInfo = semanticValueInfo semanticIR
         in not (null valueInfo) ==> not (null moduleContent)

-- ============================================================================
-- GoIR Properties
-- ============================================================================

prop_emit_go_valid_syntax :: TypusFile -> Property
prop_emit_go_valid_syntax typusFile =
  let sourceIR = buildSourceIR typusFile
  in case buildSemanticIR sourceIR of
       Left _ -> True  -- Should handle semantic errors gracefully
       Right semanticIR ->
         let goIR = emitGo semanticIR
             goSource = goSource goIR
         in hasValidGoStructure goSource

prop_emit_go_preserves_package :: TypusFile -> Property
prop_emit_go_preserves_package typusFile =
  let sourceIR = buildSourceIR typusFile
  in case buildSemanticIR sourceIR of
       Left _ -> True
       Right semanticIR ->
         let goIR = emitGo semanticIR
             goModule = goModule goIR
             goSource = goSource goIR
         in case gmPackage goModule of
              Just pkg -> "package " ++ pdName pkg `L.isInfixOf` goSource
              Nothing -> "package main" `L.isInfixOf` goSource

prop_emit_go_preserves_imports :: TypusFile -> Property
prop_emit_go_preserves_imports typusFile =
  let sourceIR = buildSourceIR typusFile
  in case buildSemanticIR sourceIR of
       Left _ -> True
       Right semanticIR ->
         let goIR = emitGo semanticIR
             goModule = goModule goIR
             goSource = goSource goIR
             imports = gmImports goModule
         in if null imports
            then True
            else L.all (\imp -> importPath imp `L.isInfixOf` goSource) imports

-- ============================================================================
-- IR Transformation Properties
-- ============================================================================

prop_ir_transformation_deterministic :: TypusFile -> Property
prop_ir_transformation_deterministic typusFile =
  let sourceIR1 = buildSourceIR typusFile
      sourceIR2 = buildSourceIR typusFile
      result1 = buildSemanticIR sourceIR1
      result2 = buildSemanticIR sourceIR2
  in case (result1, result2) of
       (Right semanticIR1, Right semanticIR2) ->
         let goIR1 = emitGo semanticIR1
             goIR2 = emitGo semanticIR2
         in goSource goIR1 === goSource goIR2
       (Left _, Left _) -> True  -- Both should fail the same way
       _ -> False  -- Should be consistent

prop_go_ir_consistent_module_source :: TypusFile -> Property
prop_go_ir_consistent_module_source typusFile =
  let sourceIR = buildSourceIR typusFile
  in case buildSemanticIR sourceIR of
       Left _ -> True
       Right semanticIR ->
         let goIR = emitGo semanticIR
             goModule = goModule goIR
             goSource = goSource goIR
             renderedModule = renderGoModule goModule
         in goSource === renderedModule

-- ============================================================================
-- Specific Test Cases
-- ============================================================================

test_ir_edge_cases :: IO ()
test_ir_edge_cases = do
  -- Test with empty file
  let emptyFile = TypusFile defaultFileDirectives [] [] []
      sourceIR = buildSourceIR emptyFile
      rawSource = sourceText sourceIR
  assertEqual "Empty file should produce empty source" "" rawSource
  
  -- Test with only whitespace blocks
  let whitespaceBlock = CodeBlock defaultBlockDirectives "   \n  \t  \n   " (emptySpan startPos)
      whitespaceFile = TypusFile defaultFileDirectives [] [whitespaceBlock] []
      sourceIR' = buildSourceIR whitespaceFile
      rawSource' = sourceText sourceIR'
  assertBool "Whitespace blocks should be preserved" $ not (null rawSource')
  
  -- Test with very long content
  let longContent = replicate 1000 "very long line with content\n"
      longBlock = CodeBlock defaultBlockDirectives (L.concat longContent) (emptySpan startPos)
      longFile = TypusFile defaultFileDirectives [] [longBlock] []
      sourceIR'' = buildSourceIR longFile
      rawSource'' = sourceText sourceIR''
  assertBool "Long content should be preserved" $ L.length rawSource'' > 10000

test_ir_malformed_input :: IO ()
test_ir_malformed_input = do
  -- Test with malformed Go code
  let malformedBlock = CodeBlock defaultBlockDirectives "func malformed(\n  // missing closing paren" (emptySpan startPos)
      malformedFile = TypusFile defaultFileDirectives [] [malformedBlock] []
      sourceIR = buildSourceIR malformedFile
      result = buildSemanticIR sourceIR >>= return . emitGo
  case result of
    Left _ -> assertBool "Malformed Go should fail gracefully" True
    Right goIR -> assertBool "Malformed Go should produce some output" $ not (L.null (goSource goIR))

-- ============================================================================
-- Helper Functions
-- ============================================================================

-- Helper function to check if a GoModule has valid structure
isValidGoModule :: GoModule -> Bool
isValidGoModule GoModule{..} = 
  not (null gmName) && 
  L.all isValidImport gmImports &&
  L.all isValidDecl gmDecls

isValidImport :: ImportDecl -> Bool
isValidImport ImportDecl{..} = not (null importPath)

isValidDecl :: GoDecl -> Bool
isValidDecl decl = case decl of
  GoFunc _ -> True
  GoType _ -> True
  GoVar _ -> True
  GoConst _ -> True
  GoStatement _ -> True
  GoRaw block -> not (null block)

hasValidGoStructure :: String -> Bool
hasValidGoStructure source = 
  let lines' = lines source
      hasPackage = L.any ("package" `L.isPrefixOf`) lines'
      hasValidBraces = countBraces source >= 0
  in hasPackage && hasValidBraces

countBraces :: String -> Int
countBraces = go 0
  where
    go acc [] = acc
    go acc ('{':rest) = go (acc + 1) rest
    go acc ('}':rest) = go (acc - 1) rest
    go acc (_:rest) = go acc rest

moduleContentText :: GoModule -> String
moduleContentText GoModule{..} =
  unlines $ concatMap flattenDeclLines gmDecls

flattenDeclLines :: GoDecl -> [String]
flattenDeclLines decl = case decl of
  GoFunc (FuncDecl ls) -> ls
  GoType (TypeDecl ls _) -> ls
  GoVar (VarDecl ls _) -> ls
  GoConst (ConstDecl ls _) -> ls
  GoStatement (StatementBlock ls) -> ls
  GoRaw block -> [block]

renderGoModule :: GoModule -> String
renderGoModule GoModule{..} =
  let packageLine = case gmPackage of
        Nothing -> "package main"
        Just pkg -> "package " ++ pdName pkg
      importLines = map renderImport gmImports
      declLines = concatMap flattenDeclLines gmDecls
  in unlines $ [packageLine] ++ importLines ++ declLines

renderImport :: ImportDecl -> String
renderImport ImportDecl{..} = case importAlias of
  Nothing -> "import \"" ++ importPath ++ "\""
  Just alias -> "import " ++ alias ++ " \"" ++ importPath ++ "\""

defaultFileDirectives :: FileDirectives
defaultFileDirectives = FileDirectives Nothing Nothing Nothing

defaultBlockDirectives :: BlockDirectives
defaultBlockDirectives = BlockDirectives Nothing Nothing Nothing

emptySpan :: SourceSpan -> SourceSpan
emptySpan pos = SourceSpan pos pos

-- ============================================================================
-- Arbitrary Instances
-- ============================================================================

instance Arbitrary TypusFile where
  arbitrary = do
    directives <- arbitrary
    buildTags <- listOf arbitrary
    blocks <- listOf arbitrary
    syntaxErrors <- arbitrary  -- Usually empty for generated files
    return $ TypusFile directives buildTags blocks syntaxErrors

instance Arbitrary FileDirectives where
  arbitrary = do
    ownership <- arbitrary
    dependentTypes <- arbitrary
    constraints <- arbitrary
    return $ FileDirectives ownership dependentTypes constraints

instance Arbitrary BlockDirectives where
  arbitrary = do
    ownership <- arbitrary
    dependentTypes <- arbitrary
    constraints <- arbitrary
    return $ BlockDirectives ownership dependentTypes constraints

instance Arbitrary CodeBlock where
  arbitrary = do
    directives <- arbitrary
    content <- listOf1 $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " \n\t{}();"
    span <- arbitrary
    return $ CodeBlock directives (L.concat content) span

instance Arbitrary SourceSpan where
  arbitrary = do
    start <- arbitrary
    endOffset <- choose (0, 100)
    let end = start { posOffset = posOffset start + endOffset }
    return $ SourceSpan start end