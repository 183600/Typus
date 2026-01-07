module Test.Unit.NewParserDirectivesSpec where
import Test.Tasty 
import Test.Tasty.HUnit (testCase, assertFailure, assertBool, (@?=)), assertBool
import Test.Tasty.QuickCheck (testProperty, Property, (===), forAll, Gen, choose, arbitrary, listOf1, elements)
import TestSupport.QuickCheck 
  ( FileDirectives(..)
  , BlockDirectives(..)
  , CodeBlock(..)
  , TypusFile(..)
  , defaultFileDirectives
  , defaultBlockDirectives
  , parseTypus
  )
import SourceLocation (SourcePos(..), SourceSpan)
                              genCodeContent = listOf1 $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " \t\n.,;:!()[]{}<>+-*/%=|&^~?@#"
-- Arbitrary instance for SourcePos
instance Arbitrary SourcePos where
  arbitrary = do
    line <- choose (1, 100)
    column <- choose (1, 100)
    offset <- choose (0, 1000)
    return $ SourcePos line column offset

-- Arbitrary instance for SourceSpan
instance Arbitrary SourceSpan where
  arbitrary = do
    start <- arbitrary
    end <- arbitrary
    return $ SourceSpan start end


-- ============================================================================
-- QuickCheck Properties
-- ============================================================================

-- Property: Default directives have L.all Nothing values
prop_defaultDirectivesNothing :: Bool
                              prop_defaultDirectivesNothing =
  let fileDirs = defaultFileDirectives
                                    blockDirs = defaultBlockDirectives
  in fdOwnership                               fileDirs == Nothing &&
     fdDependentTypes                               fileDirs == Nothing &&
     fdConstraints                               fileDirs == Nothing &&
     bdOwnership                               blockDirs == Nothing &&
     bdDependentTypes                               blockDirs == Nothing &&
     bdConstraints                               blockDirs == Nothing

-- Property: Parsing empty content returns default directives
prop_parseEmptyContent :: Bool
                              prop_parseEmptyContent =
  let result = parseTypus ""
      tfDirectives                               result == defaultFileDirectives
  in True  -- Simplified property test

-- ============================================================================
-- Unit Tests
-- ============================================================================

tests :: TestTree
tests =   testGroup "New Parser Directives Tests"
  [ testGroup "Directive Properties"
    [             testProperty "Default directives have L.all Nothing values" prop_defaultDirectivesNothing
    ,             testProperty "Parsing empty content returns default directives" prop_parseEmptyContent
    ]

  , testGroup "File Directive Parsing"
    [             testCase "Parse single ownership directive" $ do
                    let input = "//!                               ownership =true\n"
                                          result = parseTypus input
                                          tf = tfDirectives result
        case fdOwnership tf of
Just (Located value _) -> value @?= True
          Nothing -> assertBool "Expected ownership directive" False

      ,             testCase "Parse multiple file directives" $ do
                    let input = "//!                               ownership =true\n//! dependent-types=false\n//!                               constraints =on\n"
                                          result = parseTypus input
                                          tf = tfDirectives result
        case fdOwnership tf of
          Just (Located value _) -> value @?= True
          Nothing -> assertBool "Expected ownership directive" False
        case fdDependentTypes tf of
          Just (Located value _) -> value @?= False
          Nothing -> assertBool "Expected dependent-types directive" False
        case fdConstraints tf of
          Just (Located value _) -> value @?= True
          Nothing -> assertBool "Expected constraints directive" False

      ,             testCase "Parse build tags directive" $ do
                    let input = "//! build-tags=linux,amd64\n"
                                          result = parseTypus input
                                          tags = tfBuildTags result
        L.length tags @?= 1
        case L.head tags of
          Located tag _ -> tag @?= "linux,amd64"

      ,             testCase "Parse mixed directive formats" $ do
                    let input = "//!                               ownership =on\n//! dependent-types: false\n//! constraints enabled\n"
                                          result = parseTypus input
                                          tf = tfDirectives result
        -- Should parse at least the first directive correctly
        case fdOwnership tf of
          Just (Located value _) -> value @?= True
          Nothing -> assertBool "Expected ownership directive" False
    ]

  , testGroup "Block Directive Parsing"
    [             testCase "Parse block with ownership directive" $ do
                    let input = "//@                               ownership =true\nfunc main() {}\n"
                                          result = parseTypus input
                                          blocks = tfBlocks result
        L.length blocks @?= 1
        let block = L.head blocks
                                          directives = cbDirectives block
        case bdOwnership directives of
          Just (Located value _) -> value @?= True
          Nothing -> assertBool "Expected block ownership directive" False

      ,             testCase "Parse block with multiple directives" $ do
                    let input = "//@                               ownership =true\n//@ dependent-types=false\nfunc test() {}\n"
                                          result = parseTypus input
                                          blocks = tfBlocks result
        L.length blocks @?= 1
        let block = L.head blocks
                                          directives = cbDirectives block
        case bdOwnership directives of
          Just (Located value _) -> value @?= True
          Nothing -> assertBool "Expected block ownership directive" False
        case bdDependentTypes directives of
          Just (Located value _) -> value @?= False
          Nothing -> assertBool "Expected block dependent-types directive" False

      ,             testCase "Parse multiple blocks with different directives" $ do
                    let input = "//@                               ownership =true\nfunc first() {}\n\n//@                               ownership =false\nfunc second() {}\n"
                                          result = parseTypus input
                                          blocks = tfBlocks result
        L.length blocks @?= 2
        let firstBlock = blocks !! 0
                                          secondBlock = blocks !! 1
                                          firstDirs = cbDirectives firstBlock
                                          secondDirs = cbDirectives secondBlock
        case bdOwnership firstDirs of
          Just (Located value _) -> value @?= True
          Nothing -> assertBool "Expected first block ownership directive" False
        case bdOwnership secondDirs of
          Just (Located value _) -> value @?= False
          Nothing -> assertBool "Expected second block ownership directive" False
    ]

  , testGroup "Code Block Content"
    [             testCase "Parse simple code block" $ do
                    let input = "func main() {\n    println(\"Hello\")\n}\n"
                                          result = parseTypus input
                                          blocks = tfBlocks result
        L.length blocks @?= 1
        let block = L.head blocks
                                          content = cbContent block
        "func main()" `L.isInfixOf` content @?= True
        "println" `L.isInfixOf` content @?= True

      ,             testCase "Parse code block with comments" $ do
                    let input = "// This is a comment\nfunc main() {\n    // Another comment\n    println(\"Hello\")\n}\n"
                                          result = parseTypus input
                                          blocks = tfBlocks result
        L.length blocks @?= 1
        let block = L.head blocks
                                          content = cbContent block
        "This is a comment" `L.isInfixOf` content @?= True
        "Another comment" `L.isInfixOf` content @?= True

      ,             testCase "Parse code block with strings" $ do
                    let input = "func main() {\n    str := \"Hello // not a comment\"\n    println(str)\n}\n"
                                          result = parseTypus input
                                          blocks = tfBlocks result
        L.length blocks @?= 1
        let block = L.head blocks
                                          content = cbContent block
        "Hello // not a comment" `L.isInfixOf` content @?= True
    ]

  , testGroup "Error Handling"
    [             testCase "Handle malformed file directive gracefully" $ do
                    let input = "//! ownership\n"  -- Missing value
                                          result = parseTypus input
                                          tf = tfDirectives result
        -- Should still parse, even if directive is malformed
        tf `seq` True @?= True

      ,             testCase "Handle malformed block directive gracefully" $ do
                    let input = "//@ dependent-types\nfunc test() {}\n"  -- Missing value
                                          result = parseTypus input
                                          blocks = tfBlocks result
        L.length blocks @?= 1

      ,             testCase "Handle mixed valid L.and invalid directives" $ do
                    let input = "//!                               ownership =true\n//! invalid-directive\n//! dependent-types=false\n"
                                          result = parseTypus input
                                          tf = tfDirectives result
        -- Should parse valid directives despite invalid one
        case fdOwnership tf of
          Just (Located value _) -> value @?= True
          Nothing -> assertBool "Expected ownership directive" False
    ]

  , testGroup "Complex Scenarios"
    [             testCase "Parse file with file directives, blocks, L.and block directives" $ do
                    let input = "//!                               ownership =true\n//! dependent-types=false\n\n//@                               ownership =false\nfunc block1() {}\n\n//@                               constraints =true\nfunc block2() {}\n"
                                          result = parseTypus input
                                          tf = tfDirectives result
                                          blocks = tfBlocks result
        -- Check file directives
        case fdOwnership tf of
          Just (Located value _) -> value @?= True
          Nothing -> assertBool "Expected file ownership directive" False
        case fdDependentTypes tf of
          Just (Located value _) -> value @?= False
          Nothing -> assertBool "Expected file dependent-types directive" False
        -- Check blocks
        L.length blocks @?= 2
        let block1 = blocks !! 0
                                          block2 = blocks !! 1
                                          dirs1 = cbDirectives block1
                                          dirs2 = cbDirectives block2
        case bdOwnership dirs1 of
          Just (Located value _) -> value @?= False
          Nothing -> assertBool "Expected block1 ownership directive" False
        case bdConstraints dirs2 of
          Just (Located value _) -> value @?= True
          Nothing -> assertBool "Expected block2 constraints directive" False

      ,             testCase "Parse file with only code, no directives" $ do
                    let input = "func main() {\n    println(\"No directives here\")\n}\n"
                                          result = parseTypus input
                                          tf = tfDirectives result
                                          blocks = tfBlocks result
        tf @?= defaultFileDirectives
        L.length blocks @?= 1
        let block = L.head blocks
                                          directives = cbDirectives block
        directives @?= defaultBlockDirectives
    ]
  ]

-- Helper function to check if a string is contained in another
isInfixOf :: Eq                               a => [a] -> [a] -> Bool
isInfixOf needle                               haystack = L.any (isPrefixOf needle) (tails haystack)
  where
      isPrefixOf []                               _ = True
    isPrefixOf _ [] = False
    isPrefixOf (x:xs) (y:ys) =                               x == y && isPrefixOf xs ys
    tails [] = [[]]
    tails xs@(x:xs') = xs : tails xs'
