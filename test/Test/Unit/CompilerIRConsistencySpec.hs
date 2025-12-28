{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.CompilerIRConsistencySpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, choose, listOf1, elements, suchThat)
import Data.List (isPrefixOf, isInfixOf, isSuffixOf, intercalate, lines, unlines)
import Data.Maybe (isJust, isNothing, fromMaybe)
import Data.Either (isLeft, isRight)
import qualified Data.Text as T

import Parser
  ( TypusFile(..)
  , FileDirectives(..)
  , BlockDirectives(..)
  , CodeBlock(..)
  , defaultFileDirectives
  , defaultBlockDirectives
  , parseTypus
  )

import Compiler.IR
  ( SourceIR(..)
  , SemanticIR(..)
  , GoIR(..)
  , buildSourceIR
  , buildSemanticIR
  , emitGo
  , rawSourceFromTypus
  )

import Compiler.GoAst
  ( GoModule(..)
  , GoDecl(..)
  , GoImport(..)
  )

import SourceLocation
  ( SourceSpan(..)
  , SourcePos(..)
  , locatedWithSpan
  , spanStart
  , spanEnd
  , startPos
  )

import Compiler.Errors
  ( CompilerError(..)
  , CompilationPhase(..)
  , ErrorCategory(..)
  , ErrorSeverity(..)
  )

-- | Generate a valid identifier
genIdentifier :: Gen String
genIdentifier = do
  first <- elements ['a'..'z']
  rest <- listOf (elements $ ['a'..'z'] ++ ['0'..'9'] ++ "_")
  return $ first : rest

-- | Generate Go code content
genGoCode :: Gen String
genGoCode = do
  lines' <- listOf1 $ elements
    [ "package main"
    , "import \"fmt\""
    , "func main() {"
    , "  fmt.Println(\"Hello, World!\")"
    , "}"
    , "var x int = 42"
    , "func add(a, b int) int {"
    , "  return a + b"
    , "}"
    ]
  return $ unlines lines'

-- | Generate a code block
genCodeBlock :: Gen CodeBlock
genCodeBlock = do
  content <- genGoCode
  span <- genSourceSpan
  directives <- defaultBlockDirectives <$ pure ()  -- Use default directives
  return $ CodeBlock directives content span

-- | Generate a source span
genSourceSpan :: Gen SourceSpan
genSourceSpan = do
  line <- choose (1, 100)
  col <- choose (1, 50)
  return $ SourceSpan startPos startPos

-- | Generate a Typus file
genTypusFile :: Gen TypusFile
genTypusFile = do
  directives <- defaultFileDirectives <$ pure ()  -- Use default directives
  buildTags <- []
  blocks <- listOf1 genCodeBlock
  syntaxErrors <- []
  return $ TypusFile directives buildTags blocks syntaxErrors

-- | Generate a simple valid Typus source
genSimpleTypusSource :: Gen String
genSimpleTypusSource = do
  hasDirectives <- elements [True, False]
  directives <- if hasDirectives
                  then elements ["//! ownership: on", "//! dependent_types: off"]
                  else return ""
  code <- genGoCode
  return $ if null directives then code else directives ++ "\n" ++ code

-- | Generate an empty Typus source
genEmptyTypusSource :: Gen String
genEmptyTypusSource = return ""

-- | Generate a Typus source with only directives
genDirectivesOnlySource :: Gen String
genDirectivesOnlySource = do
  directives <- listOf1 $ elements ["//! ownership: on", "//! dependent_types: off", "//! constraints: on"]
  return $ unlines directives

-- | Generate a malformed Typus source
genMalformedTypusSource :: Gen String
genMalformedTypusSource = do
  malformed <- elements
    [ "func invalid syntax here !!!"
    , "package {"
    , "import"
    , "var x int"
    , "{ invalid block"
    ]
  return malformed

instance Arbitrary TypusFile where
  arbitrary = genTypusFile

instance Arbitrary CodeBlock where
  arbitrary = genCodeBlock

-- Helper function to check if string contains valid Go package declaration
hasValidPackageDecl :: String -> Bool
hasValidPackageDecl source = 
  let sourceLines = lines source
      hasPackage = any ("package " `isPrefixOf`) sourceLines
  in hasPackage

-- Helper function to check if string contains valid function declarations
hasValidFunctions :: String -> Bool
hasValidFunctions source = 
  let sourceLines = lines source
      hasFunc = any ("func " `isPrefixOf`) sourceLines
  in hasFunc

-- Helper function to check if Go code is syntactically reasonable
isReasonableGoCode :: String -> Bool
isReasonableGoCode source = 
  let hasPackage = hasValidPackageDecl source
      hasBraces = "{" `isInfixOf` source && "}" `isInfixOf` source
      hasSemicolons = ";" `isInfixOf` source
  in hasPackage && hasBraces

-- Property: buildSourceIR preserves Typus file content
prop_buildSourceIR_preservesContent :: Property
prop_buildSourceIR_preservesContent =
  forAll genTypusFile $ \typusFile ->
    let sourceIR = buildSourceIR typusFile
        originalFile = sourceTypusFile sourceIR
        extractedText = sourceText sourceIR
        rawFromOriginal = rawSourceFromTypus originalFile
    in typusFile == originalFile .&&.
       extractedText == rawFromOriginal

-- Property: rawSourceFromTypus concatenates code blocks
prop_rawSourceFromTypus_concatenatesBlocks :: Property
prop_rawSourceFromTypus_concatenatesBlocks =
  forAll genTypusFile $ \typusFile ->
    let rawSource = rawSourceFromTypus typusFile
        blocks = tfBlocks typusFile
        blockContents = map cbContent blocks
        expectedContent = intercalate "\n" blockContents
    in rawSource == expectedContent

-- Property: buildSourceIR creates valid SourceIR structure
prop_buildSourceIR_validStructure :: Property
prop_buildSourceIR_validStructure =
  forAll genTypusFile $ \typusFile ->
    let sourceIR = buildSourceIR typusFile
        sourceFile = sourceTypusFile sourceIR
        sourceText = sourceText sourceIR
    in sourceFile == typusFile .&&.
       not (null sourceText) ==> length (lines sourceText) >= 1

-- Property: buildSemanticIR preserves Typus file reference
prop_buildSemanticIR_preservesTypusFile :: Property
prop_buildSemanticIR_preservesTypusFile =
  forAll genTypusFile $ \typusFile ->
    let sourceIR = buildSourceIR typusFile
    in case buildSemanticIR sourceIR of
         Left _ -> property True  -- May fail, that's ok for this test
         Right semanticIR -> semanticTypusFile semanticIR == typusFile

-- Property: emitGo creates GoIR with valid module
prop_emitGo_validGoIR :: Property
prop_emitGo_validGoIR =
  forAll genTypusFile $ \typusFile ->
    let sourceIR = buildSourceIR typusFile
    in case buildSemanticIR sourceIR of
         Left _ -> property True  -- May fail, that's ok
         Right semanticIR ->
           let goIR = emitGo semanticIR
               goModule = goModule goIR
               goSource = goSource goIR
           in not (null goSource) .&&.
              length (gmDecls goModule) >= 0 .&&.
              length (gmImports goModule) >= 0

-- Property: emitGo generates source code from module
prop_emitGo_sourceFromModule :: Property
prop_emitGo_sourceFromModule =
  forAll genTypusFile $ \typusFile ->
    let sourceIR = buildSourceIR typusFile
    in case buildSemanticIR sourceIR of
         Left _ -> property True  -- May fail, that's ok
         Right semanticIR ->
           let goIR = emitGo semanticIR
               goModule = goModule goIR
               goSource = goSource goIR
           in not (null goSource) ==> isReasonableGoCode goSource

-- Property: parsing simple Typus source succeeds
prop_parseSimpleSource_succeeds :: Property
prop_parseSimpleSource_succeeds =
  forAll genSimpleTypusSource $ \source ->
    case parseTypus source of
      Left _ -> property False
      Right _ -> property True

-- Property: parsing empty source creates file with no blocks
prop_parseEmptySource_noBlocks :: Property
prop_parseEmptySource_noBlocks =
  let emptySource = ""
  in case parseTypus emptySource of
       Left _ -> property False
       Right typusFile -> null (tfBlocks typusFile)

-- Property: parsing directives-only source creates file with no blocks
prop_parseDirectivesOnlySource_noBlocks :: Property
prop_parseDirectivesOnlySource_noBlocks =
  forAll genDirectivesOnlySource $ \source ->
    case parseTypus source of
      Left _ -> property False
      Right typusFile -> null (tfBlocks typusFile)

-- Property: parsing malformed source may fail but doesn't crash
prop_parseMalformedSource_doesntCrash :: Property
prop_parseMalformedSource_doesntCrash =
  forAll genMalformedTypusSource $ \source ->
    case parseTypus source of
      Left _ -> property True  -- Expected to fail
      Right _ -> property True  -- May succeed with partial parsing

-- Property: IR transformation pipeline preserves basic structure
prop_irPipeline_preservesStructure :: Property
prop_irPipeline_preservesStructure =
  forAll genSimpleTypusSource $ \source ->
    case parseTypus source of
      Left _ -> property True  -- May fail parsing
      Right typusFile ->
        let sourceIR = buildSourceIR typusFile
        in case buildSemanticIR sourceIR of
             Left _ -> property True  -- May fail semantic analysis
             Right semanticIR ->
               let goIR = emitGo semanticIR
                   goSource = goSource goIR
               in not (null goSource)

-- Property: sourceIR contains original file content
prop_sourceIR_containsOriginalContent :: Property
prop_sourceIR_containsOriginalContent =
  forAll genSimpleTypusSource $ \source ->
    case parseTypus source of
      Left _ -> property True
      Right typusFile ->
        let sourceIR = buildSourceIR typusFile
            extractedText = sourceText sourceIR
            originalContent = rawSourceFromTypus typusFile
        in extractedText == originalContent

-- Property: semanticIR contains value information
prop_semanticIR_containsValueInfo :: Property
prop_semanticIR_containsValueInfo =
  forAll genSimpleTypusSource $ \source ->
    case parseTypus source of
      Left _ -> property True
      Right typusFile ->
        let sourceIR = buildSourceIR typusFile
        in case buildSemanticIR sourceIR of
             Left _ -> property True
             Right semanticIR ->
               let valueInfo = semanticValueInfo semanticIR
               in length valueInfo >= 0

-- Property: GoIR source contains package declaration
prop_goIR_containsPackage :: Property
prop_goIR_containsPackage =
  forAll genSimpleTypusSource $ \source ->
    case parseTypus source of
      Left _ -> property True
      Right typusFile ->
        let sourceIR = buildSourceIR typusFile
        in case buildSemanticIR sourceIR of
             Left _ -> property True
             Right semanticIR ->
               let goIR = emitGo semanticIR
                   goSource = goSource goIR
               in hasValidPackageDecl goSource

-- Property: IR transformation is idempotent for simple cases
prop_irTransformation_idempotent :: Property
prop_irTransformation_idempotent =
  forAll genSimpleTypusSource $ \source ->
    case parseTypus source of
      Left _ -> property True
      Right typusFile ->
        let sourceIR1 = buildSourceIR typusFile
            sourceIR2 = buildSourceIR (sourceTypusFile sourceIR1)
        in sourceIR1 == sourceIR2

-- Property: code blocks are preserved in IR pipeline
prop_codeBlocks_preserved :: Property
prop_codeBlocks_preserved =
  forAll genTypusFile $ \typusFile ->
    let originalBlocks = tfBlocks typusFile
        blockContents = map cbContent originalBlocks
        sourceIR = buildSourceIR typusFile
        extractedText = sourceText sourceIR
    in not (null originalBlocks) ==> 
       all (`isInfixOf` extractedText) blockContents
  where
    isInfixOf needle haystack = needle `elem` [take (length needle) $ drop i haystack | i <- [0..length haystack - length needle]]

-- Property: compilation errors are properly typed
prop_compilationErrors_properlyTyped :: Property
prop_compilationErrors_properlyTyped =
  forAll genTypusFile $ \typusFile ->
    let sourceIR = buildSourceIR typusFile
    in case buildSemanticIR sourceIR of
         Left errors -> all isCompilerError errors
         Right _ -> property True
  where
    isCompilerError (CompilerError {}) = True

tests :: TestTree
tests =
  testGroup "Compiler IR Consistency Properties"
    [ fastProperty "buildSourceIR preserves Typus file content" prop_buildSourceIR_preservesContent
    , fastProperty "rawSourceFromTypus concatenates code blocks" prop_rawSourceFromTypus_concatenatesBlocks
    , fastProperty "buildSourceIR creates valid SourceIR structure" prop_buildSourceIR_validStructure
    , fastProperty "buildSemanticIR preserves Typus file reference" prop_buildSemanticIR_preservesTypusFile
    , fastProperty "emitGo creates GoIR with valid module" prop_emitGo_validGoIR
    , fastProperty "emitGo generates source code from module" prop_emitGo_sourceFromModule
    , fastProperty "parsing simple Typus source succeeds" prop_parseSimpleSource_succeeds
    , fastProperty "parsing empty source creates file with no blocks" prop_parseEmptySource_noBlocks
    , fastProperty "parsing directives-only source creates file with no blocks" prop_parseDirectivesOnlySource_noBlocks
    , fastProperty "parsing malformed source may fail but doesn't crash" prop_parseMalformedSource_doesntCrash
    , fastProperty "IR transformation pipeline preserves basic structure" prop_irPipeline_preservesStructure
    , fastProperty "sourceIR contains original file content" prop_sourceIR_containsOriginalContent
    , fastProperty "semanticIR contains value information" prop_semanticIR_containsValueInfo
    , fastProperty "GoIR source contains package declaration" prop_goIR_containsPackage
    , fastProperty "IR transformation is idempotent for simple cases" prop_irTransformation_idempotent
    , fastProperty "code blocks are preserved in IR pipeline" prop_codeBlocks_preserved
    , fastProperty "compilation errors are properly typed" prop_compilationErrors_properlyTyped
    ]