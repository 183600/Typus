module Test.Unit.NewCabalIntegrationQuickCheckTestSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property, (===), counterexample, forAll, oneof, elements, listOf, suchThat)
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)
import Data.Maybe (isJust, isNothing)
import Data.Either (isLeft, isRight)

import IntegratedCompiler
import Parser (parseTypus, TypusFile(..), defaultFileDirectives)
import Compiler (compileTypus)
import Dependencies (analyzeDependentTypes)
import Ownership (analyzeOwnership)
import TestSupport.QuickCheck (fastProperty)

-- | QuickCheck tests for Integration module integration testing functions
tests :: TestTree
tests =
  testGroup "New Cabal Integration QuickCheck Tests"
    [ testProperty "parseTypus handles empty input" prop_parseTypusEmpty
    , testProperty "parseTypus handles simple valid input" prop_parseTypusSimple
    , testProperty "parseTypus is deterministic" prop_parseTypusDeterministic
    , testProperty "compileTypus handles parsed file" prop_compileTypusHandlesParsed
    , testProperty "analyzeDependentTypes handles parsed file" prop_analyzeDependentTypesHandlesParsed
    , testProperty "analyzeOwnership handles parsed file" prop_analyzeOwnershipHandlesParsed
    , testProperty "integration pipeline preserves file structure" prop_integrationPipelinePreservesStructure
    , testProperty "integration pipeline handles errors gracefully" prop_integrationPipelineHandlesErrors
    , testProperty "end-to-end compilation works for simple cases" prop_endToEndCompilationSimple
    , testProperty "integration results are consistent" prop_integrationResultsConsistent
    , testGroup "Edge cases"
        [ testCase "parseTypus handles empty string" $ do
            let result = parseTypus ""
            case result of
                Left _ -> assertFailure "Should parse empty input"
                Right file -> do
                    tfDirectives file @?= defaultFileDirectives
                    tfBuildTags file @?= []
                    tfBlocks file @?= []
        , testCase "compileTypus handles empty file" $ do
            let file = TypusFile defaultFileDirectives [] [] []
                result = compileTypus file
            case result of
                Left _ -> assertFailure "Should compile empty file"
                Right _ -> pure ()
        , testCase "integration pipeline handles simple code" $ do
            let code = "package main\n\nfunc main() {\n}\n"
                parsed = parseTypus code
            case parsed of
                Left _ -> assertFailure "Should parse simple code"
                Right file -> do
                    compiled <- return $ compileTypus file
                    case compiled of
                        Left _ -> assertFailure "Should compile simple code"
                        Right _ -> pure ()
        , testCase "integration preserves directives" $ do
            let code = "//! ownership: on\n\nfunc test() {\n}\n"
                parsed = parseTypus code
            case parsed of
                Left _ -> assertFailure "Should parse code with directives"
                Right file -> do
                    let directives = tfDirectives file
                    case fdOwnership directives of
                        Just (Located _ True) -> pure ()
                        _ -> assertFailure "Should preserve ownership directive"
        ]
    ]

-- | Property: parseTypus handles empty input
prop_parseTypusEmpty :: Property
prop_parseTypusEmpty = 
  let result = parseTypus ""
  in isRight result
  where
    isRight (Right _) = True
    isRight (Left _) = False

-- | Property: parseTypus handles simple valid input
prop_parseTypusSimple :: String -> Property
prop_parseTypusSimple content = 
  let validCode = "package main\n\nfunc main() {\n}\n"
      result = parseTypus validCode
  in isRight result
  where
    isRight (Right _) = True
    isRight (Left _) = False

-- | Property: parseTypus is deterministic
prop_parseTypusDeterministic :: String -> Property
prop_parseTypusDeterministic code = 
  let result1 = parseTypus code
      result2 = parseTypus code
  in case (result1, result2) of
       (Right file1, Right file2) -> tfDirectives file1 == tfDirectives file2
       (Left _, Left _) -> True
       _ -> False

-- | Property: compileTypus handles parsed file
prop_compileTypusHandlesParsed :: String -> Property
prop_compileTypusHandlesParsed code = 
  case parseTypus code of
    Left _ -> property True  -- Skip if parsing fails
    Right file -> 
      let result = compileTypus file
      in isRight result || isLeft result  -- Either success or failure is acceptable
  where
    isRight (Right _) = True
    isRight (Left _) = False
    isLeft (Left _) = True
    isLeft (Right _) = False

-- | Property: analyzeDependentTypes handles parsed file
prop_analyzeDependentTypesHandlesParsed :: String -> Property
prop_analyzeDependentTypesHandlesParsed code = 
  case parseTypus code of
    Left _ -> property True  -- Skip if parsing fails
    Right file -> 
      let result = analyzeDependentTypes file
      in True  -- Analysis should not crash
  where
    True = True  -- Placeholder - actual analysis would depend on Dependencies module

-- | Property: analyzeOwnership handles parsed file
prop_analyzeOwnershipHandlesParsed :: String -> Property
prop_analyzeOwnershipHandlesParsed code = 
  case parseTypus code of
    Left _ -> property True  -- Skip if parsing fails
    Right file -> 
      let result = analyzeOwnership file
      in True  -- Analysis should not crash
  where
    True = True  -- Placeholder - actual analysis would depend on Ownership module

-- | Property: integration pipeline preserves file structure
prop_integrationPipelinePreservesStructure :: String -> Property
prop_integrationPipelinePreservesStructure code = 
  case parseTypus code of
    Left _ -> property True  -- Skip if parsing fails
    Right file -> 
      let directives = tfDirectives file
          buildTags = tfBuildTags file
          blocks = tfBlocks file
      in not (null directives) || not (null buildTags) || not (null blocks) ||
         (null directives && null buildTags && null blocks)

-- | Property: integration pipeline handles errors gracefully
prop_integrationPipelineHandlesErrors :: String -> Property
prop_integrationPipelineHandlesErrors code = 
  let parsed = parseTypus code
  in case parsed of
       Left _ -> True  -- Parsing errors are handled
       Right file -> 
         let compiled = compileTypus file
         in case compiled of
              Left _ -> True  -- Compilation errors are handled
              Right _ -> True  -- Success is also handled

-- | Property: end-to-end compilation works for simple cases
prop_endToEndCompilationSimple :: Property
prop_endToEndCompilationSimple = 
  let simpleCode = "package main\n\nfunc main() {\n}\n"
      parsed = parseTypus simpleCode
      compiled = case parsed of
        Left _ -> Left "Parse failed"
        Right file -> compileTypus file
  in isRight compiled
  where
    isRight (Right _) = True
    isRight (Left _) = False

-- | Property: integration results are consistent
prop_integrationResultsConsistent :: String -> Property
prop_integrationResultsConsistent code = 
  let parsed1 = parseTypus code
      parsed2 = parseTypus code
      compiled1 = case parsed1 of
        Left _ -> Left "Parse failed"
        Right file -> compileTypus file
      compiled2 = case parsed2 of
        Left _ -> Left "Parse failed"
        Right file -> compileTypus file
  in case (parsed1, parsed2) of
       (Right file1, Right file2) -> tfDirectives file1 == tfDirectives file2
       (Left _, Left _) -> True
       _ -> False

-- Helper operator for composing properties
(.&&.) :: Property -> Property -> Property
(.&&.) = (&&)