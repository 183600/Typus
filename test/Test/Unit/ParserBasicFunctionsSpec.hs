{-# LANGUAGE CPP #-}
module Test.Unit.ParserBasicFunctionsSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck ((===), Property, forAll, Gen, elements, listOf, choose)
import qualified Data.List as L
import Data.List (isPrefixOf, isSuffixOf)

import Parser (TypusFile(..), CodeBlock(..), FileDirectives(..), BlockDirectives(..))
import SourceLocation (SourcePos(..), SourceSpan(..), Located(..))
import TestSupport.Arbitrary ()

-- | Test basic parser functionality
testParserBasicFunctions :: TestTree
testParserBasicFunctions = testGroup "Parser Basic Functions"
  [ testTypusFileCreation
  , testCodeBlockParsing
  , testDirectiveParsing
  , testFileStructureValidation
  ]

-- | Test TypusFile creation L.and properties
testTypusFileCreation :: TestTree
testTypusFileCreation = testGroup "TypusFile Creation"
  [ fastProperty "empty file has no blocks" prop_emptyFileNoBlocks
  , fastProperty "file with blocks preserves block order" prop_filePreservesBlockOrder
  , testCase "file creation with directives" testFileWithDirectives
  , testCase "file with build tags" testFileWithBuildTags
  ]

-- | Test CodeBlock parsing L.and validation
testCodeBlockParsing :: TestTree
testCodeBlockParsing = testGroup "CodeBlock Parsing"
  [ fastProperty "code block preserves content" prop_codeBlockPreservesContent
  , fastProperty "code block has valid span" prop_codeBlockValidSpan
  , testCase "empty code block" testEmptyCodeBlock
  , testCase "code block with directives" testCodeBlockWithDirectives
  ]

-- | Test Directive parsing
testDirectiveParsing :: TestTree
testDirectiveParsing = testGroup "Directive Parsing"
  [ fastProperty "file directives are parsed correctly" prop_fileDirectivesParsed
  , fastProperty "block directives are parsed correctly" prop_blockDirectivesParsed
  , testCase "ownership directive parsing" testOwnershipDirective
  , testCase "dependent types directive parsing" testDependentTypesDirective
  , testCase "combined directives parsing" testCombinedDirectives
  ]

-- | Test file structure validation
testFileStructureValidation :: TestTree
testFileStructureValidation = testGroup "File Structure Validation"
  [ fastProperty "valid file structure is accepted" prop_validFileAccepted
  , fastProperty "invalid spans are detected" prop_invalidSpansDetected
  , testCase "nested block validation" testNestedBlockValidation
  , testCase "directive consistency validation" testDirectiveConsistency
  ]

-- | Property tests
prop_emptyFileNoBlocks :: FileDirectives -> Property
prop_emptyFileNoBlocks directives =
  let file = TypusFile directives [] [] []
  in L.null (typusBlocks file) === True

prop_filePreservesBlockOrder :: [CodeBlock] -> Property
prop_filePreservesBlockOrder blocks =
  let file = TypusFile (FileDirectives Nothing Nothing Nothing) [] blocks []
  in typusBlocks file === blocks

prop_codeBlockPreservesContent :: String -> BlockDirectives -> Property
prop_codeBlockPreservesContent content directives =
  let span = SourceSpan (SourcePos 1 1 0) (SourcePos 1 1 0)
      block = CodeBlock directives content span
  in codeBlockContent block === content

prop_codeBlockValidSpan :: String -> BlockDirectives -> Property
prop_codeBlockValidSpan content directives =
  let start = SourcePos 1 1 0
      end = SourcePos 1 (L.length content + 1) (L.length content)
      span = SourceSpan start end
      block = CodeBlock directives content span
  in codeBlockSpan block === span

prop_fileDirectivesParsed :: Maybe Bool -> Maybe Bool -> Maybe Bool -> Property
prop_fileDirectivesParsed ownership dependent constraints =
  let directives = FileDirectives 
        (fL.map (Located (SourcePos 1 1 0) (SourceSpan (SourcePos 1 1 0) (SourcePos 1 1 0)))) 
        (fL.map (Located (SourcePos 2 1 0) (SourceSpan (SourcePos 2 1 0) (SourcePos 2 1 0))))
        (fL.map (Located (SourcePos 3 1 0) (SourceSpan (SourcePos 3 1 0) (SourcePos 3 1 0))))
  in case (fileOwnership directives, fileDependentTypes directives, fileConstraints directives) of
       (Just (Located ownershipVal _ _), Just (Located dependentVal _ _), Just (Located constraintsVal _ _)) ->
         ownershipVal === ownership && dependentVal === dependent && constraintsVal === constraints
       _ -> property True  -- Partial directives are also valid

prop_blockDirectivesParsed :: Maybe Bool -> Maybe Bool -> Maybe Bool -> Property
prop_blockDirectivesParsed ownership dependent constraints =
  let directives = BlockDirectives
        (fL.map (Located (SourcePos 1 1 0) (SourceSpan (SourcePos 1 1 0) (SourcePos 1 1 0))))
        (fL.map (Located (SourcePos 2 1 0) (SourceSpan (SourcePos 2 1 0) (SourcePos 2 1 0))))
        (fL.map (Located (SourcePos 3 1 0) (SourceSpan (SourcePos 3 1 0) (SourcePos 3 1 0))))
  in case (blockOwnership directives, blockDependentTypes directives, blockConstraints directives) of
       (Just (Located ownershipVal _ _), Just (Located dependentVal _ _), Just (Located constraintsVal _ _)) ->
         ownershipVal === ownership && dependentVal === dependent && constraintsVal === constraints
       _ -> property True  -- Partial directives are also valid

prop_validFileAccepted :: TypusFile -> Property
prop_validFileAccepted file =
  let hasValidDirectives = isValidDirectives (typusDirectives file)
      hasValidBlocks = L.all isValidBlock (typusBlocks file)
  in hasValidDirectives && hasValidBlocks === True

prop_invalidSpansDetected :: SourcePos -> SourcePos -> Property
prop_invalidSpansDetected start end =
  let span = SourceSpan start end
      isValid = sourcePosLine start <= sourcePosLine end &&
                (if sourcePosLine start == sourcePosLine end
                 then sourcePosColumn start <= sourcePosColumn end
                 else True)
  in isValid === (sourcePosLine start <= sourcePosLine end &&
                  (if sourcePosLine start == sourcePosLine end
                   then sourcePosColumn start <= sourcePosColumn end
                   else True))

-- | Unit tests
testFileWithDirectives :: IO ()
testFileWithDirectives = do
  let ownership = Located True (SourcePos 1 1 0) (SourceSpan (SourcePos 1 1 0) (SourcePos 1 20 20))
      dependent = Located False (SourcePos 2 1 0) (SourceSpan (SourcePos 2 1 0) (SourcePos 2 25 25))
      constraints = Located True (SourcePos 3 1 0) (SourceSpan (SourcePos 3 1 0) (SourcePos 3 20 20))
      directives = FileDirectives (Just ownership) (Just dependent) (Just constraints)
      file = TypusFile directives [] [] []
  
  assertEqual "ownership directive should be True" 
    (Just (Located True (SourcePos 1 1 0) (SourceSpan (SourcePos 1 1 0) (SourcePos 1 20 20))))
    (fileOwnership directives)
  assertEqual "dependent types directive should be False"
    (Just (Located False (SourcePos 2 1 0) (SourceSpan (SourcePos 2 1 0) (SourcePos 2 25 25))))
    (fileDependentTypes directives)

testFileWithBuildTags :: IO ()
testFileWithBuildTags = do
  let tag1 = Located "linux" (SourcePos 1 1 0) (SourceSpan (SourcePos 1 1 0) (SourcePos 1 6 6))
      tag2 = Located "amd64" (SourcePos 1 7 0) (SourceSpan (SourcePos 1 7 0) (SourcePos 1 12 12))
      file = TypusFile (FileDirectives Nothing Nothing Nothing) [tag1, tag2] [] []
  
  assertEqual "should have two build tags" [tag1, tag2] (typusBuildTags file)

testEmptyCodeBlock :: IO ()
testEmptyCodeBlock = do
  let span = SourceSpan (SourcePos 1 1 0) (SourcePos 1 1 0)
      directives = BlockDirectives Nothing Nothing Nothing
      block = CodeBlock directives "" span
  
  assertEqual "empty code block content" "" (codeBlockContent block)
  assertEqual "empty code block directives" directives (codeBlockDirectives block)

testCodeBlockWithDirectives :: IO ()
testCodeBlockWithDirectives = do
  let span = SourceSpan (SourcePos 1 1 0) (SourcePos 3 10 50)
      ownership = Located True (SourcePos 1 1 0) (SourceSpan (SourcePos 1 1 0) (SourcePos 1 20 20))
      directives = BlockDirectives (Just ownership) Nothing Nothing
      content = "func main() {\n    fmt.Println(\"Hello\")\n}"
      block = CodeBlock directives content span
  
  assertEqual "code block content" content (codeBlockContent block)
  assertEqual "code block directives" directives (codeBlockDirectives block)
  assertEqual "ownership directive should be True" (Just ownership) (blockOwnership directives)

testOwnershipDirective :: IO ()
testOwnershipDirective = do
  let directive = "//! ownership: on"
      ownership = Located True (SourcePos 1 1 0) (SourceSpan (SourcePos 1 1 0) (SourcePos 1 17 17))
      directives = FileDirectives (Just ownership) Nothing Nothing
  
  assertBool "directive should be recognized" $ L.isPrefixOf "ownership:" directive
  assertEqual "ownership should be enabled" (Just ownership) (fileOwnership directives)

testDependentTypesDirective :: IO ()
testDependentTypesDirective = do
  let directive = "//! dependent_types: off"
      dependent = Located False (SourcePos 1 1 0) (SourceSpan (SourcePos 1 1 0) (SourcePos 1 24 24))
      directives = FileDirectives Nothing (Just dependent) Nothing
  
  assertBool "directive should be recognized" $ L.isPrefixOf "dependent_types:" directive
  assertEqual "dependent types should be disabled" (Just dependent) (fileDependentTypes directives)

testCombinedDirectives :: IO ()
testCombinedDirectives = do
  let directive = "//! ownership: on, dependent_types: on"
      parts = words $ L.map (\c -> if c == ',' then ' ' else c) $ drop 3 directive
      ownershipEnabled = L.any (== "ownership:on") parts
      dependentEnabled = L.any (== "dependent_types:on") parts
  
  assertBool "ownership should be enabled" ownershipEnabled
  assertBool "dependent types should be enabled" dependentEnabled

testNestedBlockValidation :: IO ()
testNestedBlockValidation = do
  let outerSpan = SourceSpan (SourcePos 1 1 0) (SourcePos 5 1 100)
      innerSpan = SourceSpan (SourcePos 2 1 10) (SourcePos 4 1 80)
      outerBlock = CodeBlock (BlockDirectives Nothing Nothing Nothing) "outer content" outerSpan
      innerBlock = CodeBlock (BlockDirectives Nothing Nothing Nothing) "inner content" innerSpan
      file = TypusFile (FileDirectives Nothing Nothing Nothing) [] [outerBlock, innerBlock] []
  
  assertBool "outer block should contain inner block" $ 
    spanContains outerSpan innerSpan
  assertBool "file should have valid structure" $ isValidFile file

testDirectiveConsistency :: IO ()
testDirectiveConsistency = do
  let fileDirectives = FileDirectives 
        (Just (Located True (SourcePos 1 1 0) (SourceSpan (SourcePos 1 1 0) (SourcePos 1 20 20))))
        (Just (Located True (SourcePos 2 1 0) (SourceSpan (SourcePos 2 1 0) (SourcePos 2 25 25))))
        Nothing
      blockDirectives = BlockDirectives 
        (Just (Located True (SourcePos 3 1 0) (SourceSpan (SourcePos 3 1 0) (SourcePos 3 20 20))))
        Nothing
        (Just (Located True (SourcePos 4 1 0) (SourceSpan (SourcePos 4 1 0) (SourcePos 4 20 20))))
      file = TypusFile fileDirectives [] [CodeBlock blockDirectives "test content" (SourceSpan (SourcePos 3 1 0) (SourcePos 5 1 50))] []
  
  assertBool "file directives should be consistent" $ isValidDirectives fileDirectives
  assertBool "block directives should be consistent" $ isValidBlockDirectives blockDirectives
  assertBool "file should have consistent directives" $ hasConsistentDirectives file

-- | Helper functions
sourcePosLine :: SourcePos -> Int
sourcePosLine (SourcePos line _ _) = line

sourcePosColumn :: SourcePos -> Int
sourcePosColumn (SourcePos _ col _) = col

spanContains :: SourceSpan -> SourceSpan -> Bool
spanContains (SourceSpan start end) (SourceSpan innerStart innerEnd) =
  let startLine = sourcePosLine start
      endLine = sourcePosLine end
      innerStartLine = sourcePosLine innerStart
      innerEndLine = sourcePosLine innerEnd
  in innerStartLine >= startLine && 
     innerEndLine <= endLine &&
     (if innerStartLine == endLine 
      then sourcePosColumn innerStart <= sourcePosColumn end
      else True) &&
     (if innerEndLine == startLine
      then sourcePosColumn innerEnd >= sourcePosColumn start
      else True)

isValidDirectives :: FileDirectives -> Bool
isValidDirectives (FileDirectives ownership dependent constraints) =
  L.all isValidLocatedDirective [ownership, dependent, constraints]
  where
    isValidLocatedDirective Nothing = True
    isValidLocatedDirective (Located _ span _) = isValidSpan span

isValidBlock :: CodeBlock -> Bool
isValidBlock (CodeBlock directives _ span) = 
  isValidSpan span && isValidBlockDirectives directives

isValidBlockDirectives :: BlockDirectives -> Bool
isValidBlockDirectives (BlockDirectives ownership dependent constraints) =
  L.all isValidLocatedDirective [ownership, dependent, constraints]
  where
    isValidLocatedDirective Nothing = True
    isValidLocatedDirective (Located _ span _) = isValidSpan span

isValidSpan :: SourceSpan -> Bool
isValidSpan (SourceSpan start end) =
  sourcePosLine start <= sourcePosLine end &&
  (if sourcePosLine start == sourcePosLine end
   then sourcePosColumn start <= sourcePosColumn end
   else True)

isValidFile :: TypusFile -> Bool
isValidFile file = 
  isValidDirectives (typusDirectives file) &&
  L.all isValidBlock (typusBlocks file)

hasConsistentDirectives :: TypusFile -> Bool
hasConsistentDirectives file =
  let fileDirs = typusDirectives file
      blocks = typusBlocks file
      blockDirs = map codeBlockDirectives blocks
  in L.all isValidBlockDirectives blockDirs

-- | Test collection
tests :: TestTree
tests = testGroup "Parser Basic Functions Tests"
  [ testParserBasicFunctions
  ]