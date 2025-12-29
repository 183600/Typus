module Test.Unit.NewCabalCompilerQuickCheckTestSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property, (===), counterexample, forAll, oneof, elements, listOf, suchThat)
import Data.Maybe (isJust, isNothing)
import Data.List (isPrefixOf, isInfixOf)

import Compiler.IR (SourceIR(..), SemanticIR(..), GoIR(..))
import Parser (TypusFile(..), defaultFileDirectives)
import Compiler.GoAst (GoModule(..), GoDecl(..), GoImport(..))
import Compiler.ValueAnalysis (ValueInfo)
import TestSupport.QuickCheck (fastProperty)

-- | QuickCheck tests for Compiler IR module intermediate representation functions
tests :: TestTree
tests =
  testGroup "New Cabal Compiler QuickCheck Tests"
    [ testProperty "SourceIR construction preserves fields" prop_sourceIRConstruction
    , testProperty "SemanticIR construction preserves fields" prop_semanticIRConstruction
    , testProperty "GoIR construction preserves fields" prop_goIRConstruction
    , testProperty "SourceIR equality works correctly" prop_sourceIREquality
    , testProperty "SemanticIR equality works correctly" prop_semanticIREquality
    , testProperty "GoIR equality works correctly" prop_goIREquality
    , testProperty "SourceIR Show instance contains source info" prop_sourceIRShowContainsSource
    , testProperty "SemanticIR Show instance contains semantic info" prop_semanticIRShowContainsSemantic
    , testProperty "GoIR Show instance contains Go info" prop_goIRShowContainsGo
    , testGroup "Edge cases"
        [ testCase "SourceIR with empty file" $ do
            let typusFile = TypusFile defaultFileDirectives [] [] []
                sourceIR = SourceIR typusFile ""
            sourceTypusFile sourceIR @?= typusFile
            sourceText sourceIR @?= ""
        , testCase "SemanticIR with empty module" $ do
            let typusFile = TypusFile defaultFileDirectives [] [] []
                goModule = GoModule [] [] []
                semanticIR = SemanticIR typusFile goModule []
            semanticTypusFile semanticIR @?= typusFile
            semanticModule semanticIR @?= goModule
            semanticValueInfo semanticIR @?= []
        , testCase "GoIR with empty module and source" $ do
            let goModule = GoModule [] [] []
                goSource = ""
                goIR = GoIR goModule goSource
            goModule goIR @?= goModule
            goSource goIR @?= goSource
        ]
    ]

-- | Property: SourceIR construction preserves fields
prop_sourceIRConstruction :: String -> Property
prop_sourceIRConstruction sourceText = 
  let typusFile = TypusFile defaultFileDirectives [] [] []
      sourceIR = SourceIR typusFile sourceText
  in sourceTypusFile sourceIR === typusFile .&&.
     sourceText sourceIR === sourceText

-- | Property: SemanticIR construction preserves fields
prop_semanticIRConstruction :: String -> Property
prop_semanticIRConstruction packageName = 
  let typusFile = TypusFile defaultFileDirectives [] [] []
      goModule = GoModule packageName [] []
      valueInfo = [] :: [ValueInfo]
      semanticIR = SemanticIR typusFile goModule valueInfo
  in semanticTypusFile semanticIR === typusFile .&&.
     semanticModule semanticIR === goModule .&&.
     semanticValueInfo semanticIR === valueInfo

-- | Property: GoIR construction preserves fields
prop_goIRConstruction :: String -> String -> Property
prop_goIRConstruction packageName goSource = 
  let goModule = GoModule packageName [] []
      goIR = GoIR goModule goSource
  in goModule goIR === goModule .&&.
     goSource goIR === goSource

-- | Property: SourceIR equality works correctly
prop_sourceIREquality :: String -> String -> Property
prop_sourceIREquality sourceText1 sourceText2 = 
  let typusFile1 = TypusFile defaultFileDirectives [] [] []
      typusFile2 = TypusFile defaultFileDirectives [] [] []
      sourceIR1 = SourceIR typusFile1 sourceText1
      sourceIR2 = SourceIR typusFile2 sourceText2
  in (sourceIR1 == sourceIR2) === (sourceText1 == sourceText2)

-- | Property: SemanticIR equality works correctly
prop_semanticIREquality :: String -> String -> Property
prop_semanticIREquality packageName1 packageName2 = 
  let typusFile1 = TypusFile defaultFileDirectives [] [] []
      typusFile2 = TypusFile defaultFileDirectives [] [] []
      goModule1 = GoModule packageName1 [] []
      goModule2 = GoModule packageName2 [] []
      valueInfo = [] :: [ValueInfo]
      semanticIR1 = SemanticIR typusFile1 goModule1 valueInfo
      semanticIR2 = SemanticIR typusFile2 goModule2 valueInfo
  in (semanticIR1 == semanticIR2) === (packageName1 == packageName2)

-- | Property: GoIR equality works correctly
prop_goIREquality :: String -> String -> String -> String -> Property
prop_goIREquality packageName1 goSource1 packageName2 goSource2 = 
  let goModule1 = GoModule packageName1 [] []
      goModule2 = GoModule packageName2 [] []
      goIR1 = GoIR goModule1 goSource1
      goIR2 = GoIR goModule2 goSource2
  in (goIR1 == goIR2) === (packageName1 == packageName2 && goSource1 == goSource2)

-- | Property: SourceIR Show instance contains source info
prop_sourceIRShowContainsSource :: String -> Property
prop_sourceIRShowContainsSource sourceText = 
  let typusFile = TypusFile defaultFileDirectives [] [] []
      sourceIR = SourceIR typusFile sourceText
      showOutput = show sourceIR
  in "SourceIR" `isInfixOf` showOutput

-- | Property: SemanticIR Show instance contains semantic info
prop_semanticIRShowContainsSemantic :: String -> Property
prop_semanticIRShowContainsSemantic packageName = 
  let typusFile = TypusFile defaultFileDirectives [] [] []
      goModule = GoModule packageName [] []
      valueInfo = [] :: [ValueInfo]
      semanticIR = SemanticIR typusFile goModule valueInfo
      showOutput = show semanticIR
  in "SemanticIR" `isInfixOf` showOutput

-- | Property: GoIR Show instance contains Go info
prop_goIRShowContainsGo :: String -> String -> Property
prop_goIRShowContainsGo packageName goSource = 
  let goModule = GoModule packageName [] []
      goIR = GoIR goModule goSource
      showOutput = show goIR
  in "GoIR" `isInfixOf` showOutput

-- Helper operator for composing properties
(.&&.) :: Property -> Property -> Property
(.&&.) = (&&)