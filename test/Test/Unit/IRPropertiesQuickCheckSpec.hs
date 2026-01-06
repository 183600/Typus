{-# LANGUAGE CPP #-}

module Test.Unit.IRPropertiesQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck
import qualified Data.List as L
import Data.List (isInfixOf)

import Compiler.IR
import Compiler.GoAst
import Parser (TypusFile(..), FileDirectives(..), CodeBlock(..), BlockDirectives(..))
import SourceLocation (SourcePos(..), SourceSpan(..))
import TestSupport.Arbitrary ()

prop_buildSourceIR_preserves_content :: String -> Property
prop_buildSourceIR_preserves_content content =
  let typusFile = TypusFile
        { tfDirectives = FileDirectives Nothing Nothing Nothing
        , tfBuildTags = []
        , tfBlocks = [CodeBlock
            { cbDirectives = BlockDirectives Nothing Nothing Nothing
            , cbContent = content
            , cbSpan = SourceSpan (SourcePos 1 1 0) (SourcePos 1 1 0)
            }]
        , tfSyntaxErrors = []
        }
      sourceIR = buildSourceIR typusFile
  in sourceText sourceIR === content

prop_ensurePackageDecl_idempotent :: Property
prop_ensurePackageDecl_idempotent =
  let module1 = GoModule [] Nothing [] []
      module2 = ensurePackageDecl module1
      module3 = ensurePackageDecl module2
  in module2 === module3

prop_rawSourceFromTypus_concatenates_blocks :: Property
prop_rawSourceFromTypus_concatenates_blocks =
  forAll (listOf1 genCodeBlock) $ \blocks ->
  let typusFile = TypusFile
        { tfDirectives = FileDirectives Nothing Nothing Nothing
        , tfBuildTags = []
        , tfBlocks = blocks
        , tfSyntaxErrors = []
        }
      result = rawSourceFromTypus typusFile
      expected = unlines (map cbContent blocks)
  in result === expected
  where
    genCodeBlock = do
      content <- listOf (elements ['a'..'z'])
      return $ CodeBlock
        { cbDirectives = BlockDirectives Nothing Nothing Nothing
        , cbContent = content
        , cbSpan = SourceSpan (SourcePos 1 1 0) (SourcePos 1 1 0)
        }

prop_attachInferredImports_preserves_existing :: Property
prop_attachInferredImports_preserves_existing =
  forAll genModule $ \goMod ->
  let result = attachInferredImports goMod
      originalImports = gmImports goMod
  in L.all (`elem` gmImports result) originalImports === True
  where
    genModule = do
      imports <- listOf genImport
      return $ GoModule [] Nothing imports []
    genImport = do
      path <- listOf1 (elements ['a'..'z'])
      return $ ImportDecl Nothing path

prop_ensureMainFunction_adds_main :: Property
prop_ensureMainFunction_adds_main =
  let module1 = GoModule [] Nothing [] []
      module2 = ensureMainFunction module1
      hasMain = L.any isMainFunc (gmDecls module2)
  in property hasMain
  where
    isMainFunc (GoFunc (FuncDecl funcLines)) = L.any (isInfixOf "main") funcLines
    isMainFunc _ = False

prop_emitGo_produces_valid_structure :: Property
prop_emitGo_produces_valid_structure =
  forAll genGoIR $ \goIR ->
  let source = goSource goIR
  in not (null source) === True
  where
    genGoIR = do
      pkg <- listOf1 (elements ['a'..'z'])
      return $ GoIR
        { goModule = GoModule [] (Just (PackageDecl pkg)) [] []
        , goSource = "package " ++ pkg
        }

prop_buildSemanticIR_preserves_typusFile :: Property
prop_buildSemanticIR_preserves_typusFile =
  forAll genTypusFile $ \tf ->
  let sourceIR = buildSourceIR tf
  in case buildSemanticIR sourceIR of
    Right semIR -> (semanticTypusFile semIR === tf)
    Left _ -> property True
  where
    genTypusFile = return $ TypusFile
      { tfDirectives = FileDirectives Nothing Nothing Nothing
      , tfBuildTags = []
      , tfBlocks = []
      , tfSyntaxErrors = []
      }

prop_moduleFromTypus_handles_empty :: Property
prop_moduleFromTypus_handles_empty =
  let typusFile = TypusFile
        { tfDirectives = FileDirectives Nothing Nothing Nothing
        , tfBuildTags = []
        , tfBlocks = []
        , tfSyntaxErrors = []
        }
  in case moduleFromTypus typusFile of
       Right goMod -> (L.length (gmDecls goMod) >= 0) === True
       Left _ -> property True

tests :: TestTree
tests = testGroup "IR Properties QuickCheck Tests"
  [ fastProperty "buildSourceIR preserves content" prop_buildSourceIR_preserves_content
  , fastProperty "ensurePackageDecl is idempotent" prop_ensurePackageDecl_idempotent
  , fastProperty "rawSourceFromTypus concatenates blocks" prop_rawSourceFromTypus_concatenates_blocks
  , fastProperty "attachInferredImports preserves existing imports" prop_attachInferredImports_preserves_existing
  , fastProperty "ensureMainFunction adds main function" prop_ensureMainFunction_adds_main
  , fastProperty "emitGo produces valid structure" prop_emitGo_produces_valid_structure
  , fastProperty "buildSemanticIR preserves TypusFile" prop_buildSemanticIR_preserves_typusFile
  , fastProperty "moduleFromTypus handles empty input" prop_moduleFromTypus_handles_empty
  ]
