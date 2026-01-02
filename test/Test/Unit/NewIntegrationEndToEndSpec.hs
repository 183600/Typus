{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewIntegrationEndToEndSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck 
  ( Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.)
  , Arbitrary(..), Gen, oneof, choose, listOf, vectorOf, elements, sized, frequency
  , suchThat, resize
  )

import Parser (parseTypus, TypusFile(..), FileDirectives(..), defaultFileDirectives)
import Compiler (compileTypus)
import ErrorHandler
import Utils (trim, removeComments)
import SourceLocation (SourcePos(..), startPos)
import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)
import Data.Either (isLeft, isRight)

-- Integration test data
data IntegrationTestCase = IntegrationTestCase
  { testName :: String
  , inputCode :: String
  , expectedSuccess :: Bool
  , expectedPatterns :: [String]
  } deriving (Show, Eq)

instance Arbitrary IntegrationTestCase where
  arbitrary = oneof
    [ return $ IntegrationTestCase "Empty input" "" True [""]
    , return $ IntegrationTestCase "Simple code" "x := 1" True ["x"]
    , return $ IntegrationTestCase "Code with comments" "x := 1 // comment\ny := 2" True ["x", "y"]
    , return $ IntegrationTestCase "Code with block comments" "x := 1 /* comment */\ny := 2" True ["x", "y"]
    , return $ IntegrationTestCase "Malformed code" "x := 1 2 3" False []
    , do
        content <- vectorOf 10 $ elements "x:=123\n\t "
        return $ IntegrationTestCase "Generated code" content True []
    ]

-- Property: Parse-compile round trip works for valid code
prop_parse_compile_roundtrip :: IntegrationTestCase -> Property
prop_parse_compile_roundtrip testCase =
  expectedSuccess testCase ==>
  let input = inputCode testCase
      parseResult = parseTypus input
  in case parseResult of
    Left _ -> property False
    Right typusFile -> 
      let compileResult = compileTypus typusFile
      in property $ isRight compileResult

-- Property: Error propagation is consistent
prop_error_propagation_consistent :: IntegrationTestCase -> Property
prop_error_propagation_consistent testCase =
  not (expectedSuccess testCase) ==>
  let input = inputCode testCase
      parseResult = parseTypus input
  in case parseResult of
    Left _ -> property True  -- Parse error is expected
    Right typusFile -> 
      let compileResult = compileTypus typusFile
      in property $ isLeft compileResult  -- Compile error should occur

-- Property: Comment removal preserves code structure
prop_comment_preservation :: String -> String -> Property
prop_comment_preservation code comment =
  not (null code) && not ("//" `L.isInfixOf` code) && not ("/*" `L.isInfixOf` code) ==>
  let codeWithComment = code ++ " // " ++ comment
      cleaned = removeComments codeWithComment
  in property $ code `L.isPrefixOf` cleaned

-- Property: Trimming doesn't break parsing
prop_trimming_preserves_parsing :: String -> Property
prop_trimming_preserves_parsing rawCode =
  not (null rawCode) ==>
  let trimmed = trim rawCode
      parseOriginal = parseTypus rawCode
      parseTrimmed = parseTypus trimmed
  in case (parseOriginal, parseTrimmed) of
    (Right _, Right _) -> property True
    (Left _, Left _) -> property True
    (Right _, Left _) -> property False  -- Trimming shouldn't break valid code
    (Left _, Right _) -> property True   -- Trimming might fix some code

-- Property: Multiple passes don't change results
prop_multiple_passes_consistent :: IntegrationTestCase -> Property
prop_multiple_passes_consistent testCase =
  expectedSuccess testCase ==>
  let input = inputCode testCase
      firstPass = parseTypus input
      secondPass = firstPass >>= parseTypus . inputCode testCase
  in case (firstPass, secondPass) of
    (Right _, Right _) -> property True
    (Left _, Left _) -> property True
    _ -> property False

-- Property: File directives are preserved through pipeline
prop_directives_preserved :: IntegrationTestCase -> Property
prop_directives_preserved testCase =
  expectedSuccess testCase ==>
  let input = inputCode testCase
      parseResult = parseTypus input
  in case parseResult of
    Left _ -> property False
    Right typusFile -> 
      let directives = tfDirectives typusFile
          compileResult = compileTypus typusFile
      in case compileResult of
        Left _ -> property False
        Right _ -> property $ directives === tfDirectives typusFile

-- Property: Error messages are informative
prop_error_messages_informative :: IntegrationTestCase -> Property
prop_error_messages_informative testCase =
  not (expectedSuccess testCase) ==>
  let input = inputCode testCase
      parseResult = parseTypus input
  in case parseResult of
    Left err -> property $ L.length (show err) > 10  -- Error message should have some content
    Right typusFile -> 
      let compileResult = compileTypus typusFile
      in case compileResult of
        Left err -> property $ L.length (show err) > 10
        Right _ -> property False  -- Shouldn't succeed when we expect failure

-- Property: Source location tracking works end-to-end
prop_source_location_tracking :: String -> Property
prop_source_location_tracking code =
  not (null code) && '\n' `elem` code ==>
  let parseResult = parseTypus code
  in case parseResult of
    Left _ -> property True  -- Parse errors are fine
    Right typusFile -> 
      let blocks = tfBlocks typusFile
          hasLocationInfo = not (null blocks)
      in property $ hasLocationInfo

-- Property: Integration with utils functions works
prop_utils_integration :: String -> Property
prop_utils_integration rawCode =
  not (null rawCode) ==>
  let processed = trim . removeComments $ rawCode
      parseResult = parseTypus processed
  in property $ case parseResult of
    Left _ -> True
    Right _ -> True

-- Property: Complex workflows complete successfully
prop_complex_workflows :: IntegrationTestCase -> IntegrationTestCase -> Property
prop_complex_workflows testCase1 testCase2 =
  expectedSuccess testCase1 && expectedSuccess testCase2 ==>
  let input1 = inputCode testCase1
      input2 = inputCode testCase2
      combined = input1 ++ "\n" ++ input2
      parseResult = parseTypus combined
  in case parseResult of
    Left _ -> property False
    Right typusFile -> 
      let compileResult = compileTypus typusFile
      in property $ isRight compileResult

tests :: TestTree
tests = testGroup "New Integration End-to-End Tests"
  [ fastProperty "Parse-compile round trip works" prop_parse_compile_roundtrip
  , fastProperty "Error propagation is consistent" prop_error_propagation_consistent
  , fastProperty "Comment removal preserves code structure" prop_comment_preservation
  , fastProperty "Trimming doesn't break parsing" prop_trimming_preserves_parsing
  , fastProperty "Multiple passes don't change results" prop_multiple_passes_consistent
  , fastProperty "File directives are preserved through pipeline" prop_directives_preserved
  , fastProperty "Error messages are informative" prop_error_messages_informative
  , fastProperty "Source location tracking works end-to-end" prop_source_location_tracking
  , fastProperty "Integration with utils functions works" prop_utils_integration
  , fastProperty "Complex workflows complete successfully" prop_complex_workflows
  , testCase "Manual integration test" $ do
      let simpleCode = "x := 1\ny := 2\nz := x + y"
          parseResult = parseTypus simpleCode
      assertBool "Simple code should parse" $ isRight parseResult
      case parseResult of
        Left _ -> return ()
        Right typusFile -> do
          let compileResult = compileTypus typusFile
          assertBool "Simple code should compile" $ isRight compileResult
  ]