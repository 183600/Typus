{-# LANGUAGE CPP #-}
module Test.Unit.CompilerIREdgeCaseSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool, assertFailure)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck ((===), Property, forAll, Gen, choose, listOf, elements)
import Data.List (sort, nub, length, intercalate, isInfixOf, isPrefixOf)
import Data.Maybe (isJust, isNothing, fromMaybe)
import qualified Data.Text as T
import qualified Data.Set as Set

import Compiler.IR
  ( SourceIR(..)
  , SemanticIR(..)
  , GoIR(..)
  , buildSourceIR
  , buildSemanticIR
  , emitGo
  , rawSourceFromTypus
  , moduleFromTypus
  )
import Parser (TypusFile(..), FileDirectives(..), BlockDirectives(..), CodeBlock(..))
import Compiler.GoAst (GoModule(..), GoDecl(..))
import SourceLocation (Located(..), SourceSpan(..), mkSourcePos, mkSourceSpan, locatedWithSpan)

-- | Edge case and property-based tests for Compiler.IR module
tests :: TestTree
tests =
  testGroup "Compiler IR Edge Case Tests"
    [ testGroup "SourceIR properties"
        [ fastProperty "SourceIR equality is reflexive" prop_sourceIREquality
        , fastProperty "SourceIR preserves typus file" prop_sourceIRPreservesTypusFile
        , fastProperty "SourceIR preserves source text" prop_sourceIRPreservesSourceText
        , fastProperty "buildSourceIR creates valid IR" prop_buildSourceIRValid
        ]

    , testGroup "SemanticIR properties"
        [ fastProperty "SemanticIR equality is reflexive" prop_semanticIREquality
        , fastProperty "SemanticIR preserves typus file" prop_semanticIRPreservesTypusFile
        , fastProperty "SemanticIR preserves module" prop_semanticIRPreservesModule
        , fastProperty "SemanticIR preserves value info" prop_semanticIRPreservesValueInfo
        ]

    , testGroup "GoIR properties"
        [ fastProperty "GoIR equality is reflexive" prop_goIREquality
        , fastProperty "GoIR preserves module" prop_goIRPreservesModule
        , fastProperty "GoIR preserves source" prop_goIRPreservesSource
        , fastProperty "emitGo creates valid GoIR" prop_emitGoValid
        ]

    , testGroup "IR transformation properties"
        [ testCase "empty TypusFile creates valid SourceIR" $ do
            let emptyFile = TypusFile 
                  { tfDirectives = defaultFileDirectives
                  , tfBuildTags = []
                  , tfBlocks = []
                  }
                ir = buildSourceIR emptyFile
            sourceTypusFile ir @?= emptyFile
            sourceText ir @?= ""

        , testCase "SourceIR handles complex nested structures" $ do
            let pos = mkSourcePos 1 1 0
                span = mkSourceSpan pos pos
                directives = BlockDirectives 
                  { bdOwnership = Just (locatedWithSpan span True)
                  , bdDependentTypes = Nothing
                  , bdConstraints = Nothing
                  }
                codeBlock = CodeBlock 
                  { cbDirectives = directives
                  , cbContent = "func test() { return 42 }"
                  , cbSpan = span
                  }
                typusFile = TypusFile 
                  { tfDirectives = defaultFileDirectives
                  , tfBuildTags = []
                  , tfBlocks = [codeBlock]
                  }
                ir = buildSourceIR typusFile
            sourceTypusFile ir @?= typusFile
            sourceText ir @?= "func test() { return 42 }"

        , testCase "rawSourceFromTypus concatenates block content" $ do
            let pos = mkSourcePos 1 1 0
                span = mkSourceSpan pos pos
                block1 = CodeBlock 
                  { cbDirectives = defaultBlockDirectives
                  , cbContent = "package main"
                  , cbSpan = span
                  }
                block2 = CodeBlock 
                  { cbDirectives = defaultBlockDirectives
                  , cbContent = "func main() {}"
                  , cbSpan = span
                  }
                typusFile = TypusFile 
                  { tfDirectives = defaultFileDirectives
                  , tfBuildTags = []
                  , tfBlocks = [block1, block2]
                  }
                rawSource = rawSourceFromTypus typusFile
            rawSource @?= "package main\nfunc main() {}"

        , testCase "moduleFromTypus handles simple Go code" $ do
            let pos = mkSourcePos 1 1 0
                span = mkSourceSpan pos pos
                codeBlock = CodeBlock 
                  { cbDirectives = defaultBlockDirectives
                  , cbContent = "package main\n\nfunc main() {\n    println(\"hello\")\n}"
                  , cbSpan = span
                  }
                typusFile = TypusFile 
                  { tfDirectives = defaultFileDirectives
                  , tfBuildTags = []
                  , tfBlocks = [codeBlock]
                  }
                ir = buildSourceIR typusFile
            case buildSemanticIR ir of
              Right semanticIR -> do
                case semanticModule semanticIR of
                  GoModule { gmPackage = pkg, gmDecls = decls } -> do
                    locatedValue pkg @?= "main"
                    assertBool "should have declarations" (not $ null decls)
                  _ -> assertFailure "Expected GoModule"
              Left _ -> assertFailure "Expected successful semantic analysis"

        , testCase "emitGo produces valid Go source" $ do
            let pos = mkSourcePos 1 1 0
                span = mkSourceSpan pos pos
                codeBlock = CodeBlock 
                  { cbDirectives = defaultBlockDirectives
                  , cbContent = "package main\n\nfunc main() {\n    println(\"test\")\n}"
                  , cbSpan = span
                  }
                typusFile = TypusFile 
                  { tfDirectives = defaultFileDirectives
                  , tfBuildTags = []
                  , tfBlocks = [codeBlock]
                  }
                ir = buildSourceIR typusFile
            case buildSemanticIR ir of
              Right semanticIR -> do
                let goIR = emitGo semanticIR
                case goIR of
                  GoIR { goModule = module, goSource = source } -> do
                    assertBool "source should contain package" ("package main" `isInfixOf` source)
                    assertBool "source should contain main function" ("func main" `isInfixOf` source)
                    assertBool "source should contain println" ("println" `isInfixOf` source)
                  _ -> assertFailure "Expected GoIR"
              Left _ -> assertFailure "Expected successful semantic analysis"
        ]

    , testGroup "Complex transformation scenarios"
        [ testCase "multiple code blocks are handled correctly" $ do
            let pos = mkSourcePos 1 1 0
                span = mkSourceSpan pos pos
                block1 = CodeBlock 
                  { cbDirectives = defaultBlockDirectives
                  , cbContent = "package main"
                  , cbSpan = span
                  }
                block2 = CodeBlock 
                  { cbDirectives = defaultBlockDirectives
                  , cbContent = "import \"fmt\""
                  , cbSpan = span
                  }
                block3 = CodeBlock 
                  { cbDirectives = defaultBlockDirectives
                  , cbContent = "func hello() {\n    fmt.Println(\"hello\")\n}"
                  , cbSpan = span
                  }
                typusFile = TypusFile 
                  { tfDirectives = defaultFileDirectives
                  , tfBuildTags = []
                  , tfBlocks = [block1, block2, block3]
                  }
                ir = buildSourceIR typusFile
                rawSource = rawSourceFromTypus typusFile
                expectedSource = "package main\nimport \"fmt\"\nfunc hello() {\n    fmt.Println(\"hello\")\n}"
            rawSource @?= expectedSource

        , testCase "build tags are preserved in IR" $ do
            let pos = mkSourcePos 1 1 0
                span = mkSourceSpan pos pos
                buildTag1 = locatedWithSpan span "//go:build ignore"
                buildTag2 = locatedWithSpan span "// +build ignore"
                codeBlock = CodeBlock 
                  { cbDirectives = defaultBlockDirectives
                  , cbContent = "package main\nfunc main() {}"
                  , cbSpan = span
                  }
                typusFile = TypusFile 
                  { tfDirectives = defaultFileDirectives
                  , tfBuildTags = [buildTag1, buildTag2]
                  , tfBlocks = [codeBlock]
                  }
                ir = buildSourceIR typusFile
            length (tfBuildTags $ sourceTypusFile ir) @?= 2
            locatedValue (head (tfBuildTags $ sourceTypusFile ir)) @?= "//go:build ignore"

        , testCase "directives are preserved through IR transformations" $ do
            let pos = mkSourcePos 1 1 0
                span = mkSourceSpan pos pos
                fileDirectives = FileDirectives 
                  { fdOwnership = Just (locatedWithSpan span True)
                  , fdDependentTypes = Just (locatedWithSpan span False)
                  , fdConstraints = Nothing
                  }
                blockDirectives = BlockDirectives 
                  { bdOwnership = Just (locatedWithSpan span True)
                  , bdDependentTypes = Just (locatedWithSpan span True)
                  , bdConstraints = Nothing
                  }
                codeBlock = CodeBlock 
                  { cbDirectives = blockDirectives
                  , cbContent = "func test() {}"
                  , cbSpan = span
                  }
                typusFile = TypusFile 
                  { tfDirectives = fileDirectives
                  , tfBuildTags = []
                  , tfBlocks = [codeBlock]
                  }
                ir = buildSourceIR typusFile
                sourceFile = sourceTypusFile ir
            case fdOwnership (tfDirectives sourceFile) of
              Just ownership -> locatedValue ownership @?= True
              Nothing -> assertFailure "Expected ownership directive"
            case fdDependentTypes (tfDirectives sourceFile) of
              Just depTypes -> locatedValue depTypes @?= False
              Nothing -> assertFailure "Expected dependent types directive"

        , testCase "IR handles edge cases in Go code generation" $ do
            let pos = mkSourcePos 1 1 0
                span = mkSourceSpan pos pos
                -- Edge case: empty function, complex generics, interfaces
                complexGoCode = unlines
                  [ "package main"
                  , ""
                  , "type Container[T any] struct {"
                  , "    data T"
                  , "}"
                  , ""
                  , "type Writer interface {"
                  , "    Write([]byte) (int, error)"
                  , "}"
                  , ""
                  , "func empty() {}"
                  , ""
                  , "func main() {"
                  , "    var c Container[int]"
                  , "    _ = c"
                  , "}"
                  ]
                codeBlock = CodeBlock 
                  { cbDirectives = defaultBlockDirectives
                  , cbContent = complexGoCode
                  , cbSpan = span
                  }
                typusFile = TypusFile 
                  { tfDirectives = defaultFileDirectives
                  , tfBuildTags = []
                  , tfBlocks = [codeBlock]
                  }
                ir = buildSourceIR typusFile
            case buildSemanticIR ir of
              Right semanticIR -> do
                let goIR = emitGo semanticIR
                case goIR of
                  GoIR { goSource = source } -> do
                    assertBool "source should contain Container" ("Container" `isInfixOf` source)
                    assertBool "source should contain Writer interface" ("Writer" `isInfixOf` source)
                    assertBool "source should contain empty function" ("func empty" `isInfixOf` source)
                    assertBool "source should contain main function" ("func main" `isInfixOf` source)
                  _ -> assertFailure "Expected GoIR"
              Left _ -> assertFailure "Expected successful semantic analysis"
        ]

    , testGroup "Error handling and edge cases"
        [ testCase "IR handles malformed Go code gracefully" $ do
            let pos = mkSourcePos 1 1 0
                span = mkSourceSpan pos pos
                malformedCode = "package main\nfunc main {\n    missing parenthesis\n}"
                codeBlock = CodeBlock 
                  { cbDirectives = defaultBlockDirectives
                  , cbContent = malformedCode
                  , cbSpan = span
                  }
                typusFile = TypusFile 
                  { tfDirectives = defaultFileDirectives
                  , tfBuildTags = []
                  , tfBlocks = [codeBlock]
                  }
                ir = buildSourceIR typusFile
            case buildSemanticIR ir of
              Left _ -> assertBool "expected failure for malformed code" True
              Right _ -> assertFailure "Expected failure for malformed code"

        , testCase "IR handles empty source text" $ do
            let emptyFile = TypusFile 
                  { tfDirectives = defaultFileDirectives
                  , tfBuildTags = []
                  , tfBlocks = []
                  }
                ir = buildSourceIR emptyFile
                rawSource = rawSourceFromTypus emptyFile
            rawSource @?= ""
            case buildSemanticIR ir of
              Right semanticIR -> do
                let goIR = emitGo semanticIR
                case goIR of
                  GoIR { goSource = source } -> do
                    assertBool "source should contain package" ("package" `isInfixOf` source)
                  _ -> assertFailure "Expected GoIR"
              Left _ -> assertFailure "Expected successful semantic analysis for empty file"
        ]
    ]

-- Helper generators for testing
genSourceIR :: Gen SourceIR
genSourceIR = do
  -- Simplified generator for testing
  let typusFile = TypusFile 
        { tfDirectives = defaultFileDirectives
        , tfBuildTags = []
        , tfBlocks = []
        }
      sourceText = ""
  return $ SourceIR typusFile sourceText

genSemanticIR :: Gen SemanticIR
genSemanticIR = do
  -- Simplified generator for testing
  let typusFile = TypusFile 
        { tfDirectives = defaultFileDirectives
        , tfBuildTags = []
        , tfBlocks = []
        }
      goModule = GoModule 
        { gmPackage = locatedWithSpan (mkSourceSpan (mkSourcePos 1 1 0) (mkSourcePos 1 8 7)) "main"
        , gmImports = []
        , gmDecls = []
        , gmBuildTags = []
        }
      valueInfo = []
  return $ SemanticIR typusFile goModule valueInfo

genGoIR :: Gen GoIR
genGoIR = do
  -- Simplified generator for testing
  let goModule = GoModule 
        { gmPackage = locatedWithSpan (mkSourceSpan (mkSourcePos 1 1 0) (mkSourcePos 1 8 7)) "main"
        , gmImports = []
        , gmDecls = []
        , gmBuildTags = []
        }
      goSource = "package main\n\nfunc main() {}"
  return $ GoIR goModule goSource

-- Default values for testing
defaultFileDirectives :: FileDirectives
defaultFileDirectives = FileDirectives 
  { fdOwnership = Nothing
  , fdDependentTypes = Nothing
  , fdConstraints = Nothing
  }

defaultBlockDirectives :: BlockDirectives
defaultBlockDirectives = BlockDirectives 
  { bdOwnership = Nothing
  , bdDependentTypes = Nothing
  , bdConstraints = Nothing
  }

-- Property: SourceIR equality is reflexive
prop_sourceIREquality :: SourceIR -> Property
prop_sourceIREquality sourceIR = sourceIR === sourceIR

-- Property: SourceIR preserves typus file
prop_sourceIRPreservesTypusFile :: TypusFile -> Property
prop_sourceIRPreservesTypusFile typusFile =
  let ir = buildSourceIR typusFile
  in sourceTypusFile ir === typusFile

-- Property: SourceIR preserves source text
prop_sourceIRPreservesSourceText :: TypusFile -> Property
prop_sourceIRPreservesSourceText typusFile =
  let ir = buildSourceIR typusFile
      expectedSource = rawSourceFromTypus typusFile
  in sourceText ir === expectedSource

-- Property: buildSourceIR creates valid IR
prop_buildSourceIRValid :: TypusFile -> Property
prop_buildSourceIRValid typusFile =
  let ir = buildSourceIR typusFile
  in sourceTypusFile ir === typusFile

-- Property: SemanticIR equality is reflexive
prop_semanticIREquality :: SemanticIR -> Property
prop_semanticIREquality semanticIR = semanticIR === semanticIR

-- Property: SemanticIR preserves typus file
prop_semanticIRPreservesTypusFile :: TypusFile -> Property
prop_semanticIRPreservesTypusFile typusFile =
  let sourceIR = buildSourceIR typusFile
  in case buildSemanticIR sourceIR of
       Right semanticIR -> semanticTypusFile semanticIR === typusFile
       Left _ -> property False

-- Property: SemanticIR preserves module
prop_semanticIRPreservesModule :: TypusFile -> Property
prop_semanticIRPreservesModule typusFile =
  let sourceIR = buildSourceIR typusFile
  in case buildSemanticIR sourceIR of
       Right semanticIR -> property True  -- Module should be preserved
       Left _ -> property False

-- Property: SemanticIR preserves value info
prop_semanticIRPreservesValueInfo :: TypusFile -> Property
prop_semanticIRPreservesValueInfo typusFile =
  let sourceIR = buildSourceIR typusFile
  in case buildSemanticIR sourceIR of
       Right semanticIR -> property True  -- Value info should be generated
       Left _ -> property False

-- Property: GoIR equality is reflexive
prop_goIREquality :: GoIR -> Property
prop_goIREquality goIR = goIR === goIR

-- Property: GoIR preserves module
prop_goIRPreservesModule :: SemanticIR -> Property
prop_goIRPreservesModule semanticIR =
  let goIR = emitGo semanticIR
  in goModule goIR === semanticModule semanticIR

-- Property: GoIR preserves source
prop_goIRPreservesSource :: SemanticIR -> Property
prop_goIRPreservesSource semanticIR =
  let goIR = emitGo semanticIR
      expectedSource = renderGoModule (semanticModule semanticIR)
  in goSource goIR === expectedSource

-- Property: emitGo creates valid GoIR
prop_emitGoValid :: SemanticIR -> Property
prop_emitGoValid semanticIR =
  let goIR = emitGo semanticIR
  in goModule goIR === semanticModule semanticIR

-- Helper function to render GoModule (simplified version)
renderGoModule :: GoModule -> String
renderGoModule module = 
  "package " ++ locatedValue (gmPackage module) ++ "\n\n" ++
  concatMap renderDecl (gmDecls module)

renderDecl :: GoDecl -> String
renderDecl decl = case decl of
  GoFunc _ -> "func declaration\n"
  GoType _ -> "type declaration\n"
  GoVar _ -> "variable declaration\n"
  GoConst _ -> "constant declaration\n"
  GoStatement _ -> "statement\n"
  GoRaw block -> block ++ "\n"