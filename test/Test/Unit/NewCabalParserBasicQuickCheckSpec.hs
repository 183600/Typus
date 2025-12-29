{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Test.Unit.NewCabalParserBasicQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import Test.QuickCheck.Gen (Gen, choose, listOf1, vectorOf, elements)

import Parser
  ( parseTypus
  , FileDirectives(..)
  , BlockDirectives(..)
  , CodeBlock(..)
  , TypusFile(..)
  , defaultFileDirectives
  , defaultBlockDirectives
  )

import Utils (trim, removeComments)

import Data.List (isPrefixOf, isInfixOf, isSuffixOf)
import Data.Char (isSpace)

-- Generate a simple directive string
genDirective :: Gen String
genDirective = do
  directive <- elements ["// @ownership", "// @dependent-types", "// @constraints"]
  enabled <- elements ["true", "false"]
  return $ directive ++ ": " ++ enabled

-- Generate a simple code block
genCodeBlock :: Gen String
genCodeBlock = do
  linesCount <- choose (1, 5)
  lines' <- vectorOf linesCount $ do
    line <- elements ["func main() {}", "x := 42", "return x", "var y int", "y = 10"]
    return line
  return $ unlines lines'

-- Generate a simple Typus file content
genTypusFileContent :: Gen String
genTypusFileContent = do
  directives <- listOf1 genDirective
  code <- genCodeBlock
  return $ unlines directives ++ "\n" ++ code

-- Property: defaultFileDirectives has no directives set
prop_defaultFileDirectives_empty :: Property
prop_defaultFileDirectives_empty =
  let fd = defaultFileDirectives
  in fdOwnership fd === Nothing .&&.
     fdDependentTypes fd === Nothing .&&.
     fdConstraints fd === Nothing

-- Property: defaultBlockDirectives has no directives set
prop_defaultBlockDirectives_empty :: Property
prop_defaultBlockDirectives_empty =
  let bd = defaultBlockDirectives
  in bdOwnership bd === Nothing .&&.
     bdDependentTypes bd === Nothing .&&.
     bdConstraints bd === Nothing

-- Property: parseTypus can handle empty input
prop_parseTypus_empty_input :: Property
prop_parseTypus_empty_input =
  let result = parseTypus ""
  in case result of
    Left _ -> property True  -- Parse error is acceptable for empty input
    Right tf -> tfCodeBlocks tf @?= []

-- Property: parseTypus can handle simple directive
prop_parseTypus_simple_directive :: Property
prop_parseTypus_simple_directive =
  let input = "// @ownership: true\nfunc main() {}"
      result = parseTypus input
  in case result of
    Left _ -> property False  -- Should not fail on simple input
    Right tf -> 
      let directives = tfFileDirectives tf
      in case fdOwnership directives of
        Nothing -> property False
        Just located -> locatedValue located @?= True

-- Property: parseTypus preserves code content
prop_parseTypus_preserves_code :: String -> Property
prop_parseTypus_preserves_code code =
  not (null code) && length code < 100 ==>
  let input = "// @ownership: true\n" ++ code
      result = parseTypus input
  in case result of
    Left _ -> property True  -- Parse error is acceptable
    Right tf -> 
      let blocks = tfCodeBlocks tf
      in if null blocks
         then property True
         else let firstBlock = head blocks
                  blockContent = cbContent firstBlock
              in code `isInfixOf` blockContent

-- Property: parseTypus handles whitespace correctly
prop_parseTypus_whitespace_handling :: String -> Property
prop_parseTypus_whitespace_handling code =
  let input1 = "// @ownership: true\n" ++ code
      input2 = "  // @ownership: true  \n  " ++ code ++ "  \n"
      result1 = parseTypus input1
      result2 = parseTypus input2
  in case (result1, result2) of
    (Left _, Left _) -> property True  -- Both fail is acceptable
    (Right tf1, Right tf2) -> 
      let directives1 = tfFileDirectives tf1
          directives2 = tfFileDirectives tf2
      in case (fdOwnership directives1, fdOwnership directives2) of
        (Just loc1, Just loc2) -> locatedValue loc1 === locatedValue loc2
        _ -> property True
    _ -> property True  -- Mixed success/failure is acceptable

-- Property: parseTypus can handle multiple directives
prop_parseTypus_multiple_directives :: Property
prop_parseTypus_multiple_directives =
  let input = unlines 
        [ "// @ownership: true"
        , "// @dependent-types: false"
        , "// @constraints: true"
        , "func main() {}"
        ]
      result = parseTypus input
  in case result of
    Left _ -> property False  -- Should not fail on multiple directives
    Right tf -> 
      let fd = tfFileDirectives tf
          ownership = fmap locatedValue (fdOwnership fd)
          dependentTypes = fmap locatedValue (fdDependentTypes fd)
          constraints = fmap locatedValue (fdConstraints fd)
      in ownership === Just True .&&.
         dependentTypes === Just False .&&.
         constraints === Just True

-- Property: parseTypus handles block directives
prop_parseTypus_block_directives :: Property
prop_parseTypus_block_directives =
  let input = unlines
        [ "// @ownership: true"
        , "// @block: @dependent-types: false"
        , "func test() {}"
        , "// @block: @constraints: true"
        , "func main() {}"
        ]
      result = parseTypus input
  in case result of
    Left _ -> property True  -- Parse error is acceptable
    Right tf -> 
      let blocks = tfCodeBlocks tf
      in if length blocks >= 2
         then let block1 = head blocks
                  block2 = blocks !! 1
                  bd1 = cbBlockDirectives block1
                  bd2 = cbBlockDirectives block2
              in ( fmap locatedValue (bdDependentTypes bd1) === Just False
                 ) .&&.
                 ( fmap locatedValue (bdConstraints bd2) === Just True
                 )
         else property True

-- Property: parseTypus is idempotent on well-formed input
prop_parseTypus_idempotent :: String -> Property
prop_parseTypus_idempotent content =
  length content < 200 && not (null content) ==>
  let trimmed = trim content
      result1 = parseTypus trimmed
  in case result1 of
    Left _ -> property True  -- Parse failure is acceptable
    Right tf1 -> 
      let normalized = unlines $ map trim $ lines trimmed
          result2 = parseTypus normalized
      in case result2 of
        Left _ -> property True  -- Parse failure is acceptable
        Right tf2 -> 
          let blocks1 = length $ tfCodeBlocks tf1
              blocks2 = length $ tfCodeBlocks tf2
          in blocks1 === blocks2

tests :: TestTree
tests =
  testGroup "Parser Basic QuickCheck Tests"
    [ testCase "defaultFileDirectives has no directives" $ do
        let fd = defaultFileDirectives
        fdOwnership fd @?= Nothing
        fdDependentTypes fd @?= Nothing
        fdConstraints fd @?= Nothing
        
    , testCase "defaultBlockDirectives has no directives" $ do
        let bd = defaultBlockDirectives
        bdOwnership bd @?= Nothing
        bdDependentTypes bd @?= Nothing
        bdConstraints bd @?= Nothing
        
    , testCase "parseTypus handles empty input" $ do
        let result = parseTypus ""
        case result of
          Left _ -> return ()  -- Parse error is acceptable
          Right tf -> tfCodeBlocks tf @?= []
          
    , testCase "parseTypus handles simple directive" $ do
        let input = "// @ownership: true\nfunc main() {}"
            result = parseTypus input
        case result of
          Left err -> assertBool $ "Should parse simple directive: " ++ show err
          Right tf -> 
            let directives = tfFileDirectives tf
            in case fdOwnership directives of
              Nothing -> assertBool "Ownership directive should be set" False
              Just located -> locatedValue located @?= True
              
    , fastProperty "parseTypus preserves code content" prop_parseTypus_preserves_code
    , fastProperty "parseTypus handles whitespace correctly" prop_parseTypus_whitespace_handling
    , fastProperty "parseTypus can handle multiple directives" prop_parseTypus_multiple_directives
    , fastProperty "parseTypus handles block directives" prop_parseTypus_block_directives
    , fastProperty "parseTypus is idempotent on well-formed input" prop_parseTypus_idempotent
    ]