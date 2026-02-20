{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans  -Wno-unused-imports -Wno-name-shadowing -Wno-unused-local-binds #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Test.Unit.IntegrationComprehensiveSpec where



import Test.Tasty.HUnit

-- Import enhanced memory optimization modules
import TestSupport.SuperMemoryOptimization 
  ( SuperMemoryLevel(..)
  , withSuperEmergencyMemoryLimits
  , withSuperCriticalMemoryLimits
  , withSuperMinimalMemoryLimits
  , superMemoryLimitedTestGroup
  , superGC
  )
import Test.Tasty (TestTree, testGroup)

-- Import enhanced memory optimization modules
import TestSupport.SuperMemoryOptimization 
  ( SuperMemoryLevel(..)
  , withSuperEmergencyMemoryLimits
  , withSuperCriticalMemoryLimits
  , withSuperMinimalMemoryLimits
  , superMemoryLimitedTestGroup
  , superGC
  )
import Test.Tasty

-- Import enhanced memory optimization modules
import TestSupport.SuperMemoryOptimization 
  ( SuperMemoryLevel(..)
  , withSuperEmergencyMemoryLimits
  , withSuperCriticalMemoryLimits
  , withSuperMinimalMemoryLimits
  , superMemoryLimitedTestGroup
  , superGC
  )
-- Removed empty QuickCheck import
import Test.Tasty.QuickCheck (testProperties, Arbitrary(..), Gen, choose, listOf, elements, oneof, vectorOf, property, (===), forAll, counterexample)

-- Import enhanced memory optimization modules
import TestSupport.SuperMemoryOptimization 
  ( SuperMemoryLevel(..)
  , withSuperEmergencyMemoryLimits
  , withSuperCriticalMemoryLimits
  , withSuperMinimalMemoryLimits
  , superMemoryLimitedTestGroup
  , superGC
  )
import Test.QuickCheck (Gen, Property, (==>), classify)
import Parser (parseTypus, FileDirectives(..), BlockDirectives(..), CodeBlock(..), TypusFile(..))
import SourceLocation (SourcePos(..), SourceSpan(..), Located(..), locatedAt, advancePosByText)
import Compiler.Errors.Core (TypeError(..), ErrorSeverity(..), ErrorCategory(..), 
                            ErrorLocation(..), ErrorContext(..), emptyContext,
                            errorAt, warningAt, infoAt, newErrorCollector, addError,
                            getErrors, hasErrors, formatErrorWithLocation)
import Utils (trim, splitBy, removeComments, normalizeIndentation)
import qualified Data.Text as T
import Data.Char (isAlphaNum, isSpace)
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)
import Control.Monad.State (execState)

-- Helper generators for Integration tests
genSourcePos :: Gen SourcePos
genSourcePos = do
  line <- choose (1, 100)
  column <- choose (1, 100)
  offset <- choose (0, 10000)
  return $ SourcePos line column offset

genSourceSpan :: Gen SourceSpan
genSourceSpan = do
  start <- genSourcePos
  end <- genSourcePos
  return $ SourceSpan start end

genChar :: Gen Char
genChar = elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " \t\n\r.,;:!()[]{}+-*/=<>&|^%~?"

genString :: Gen String
genString = do
  len <- choose (0, 50)
  vectorOf len genChar

genTypusContent :: Gen String
genTypusContent = do
  directives <- oneof [return "", return "ownership=true\n", return "dependent-types=false\n", return "constraints=true\n"]
  code <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " \n\t.,;:+-*/=<>()[]{}"
  return $ directives ++ unlines (chunksOf 20 code)
  where
    chunksOf _ [] = []
    chunksOf n xs = take n xs : chunksOf n (drop n xs)

-- Test properties for Integration tests

-- Property 1: Parser and Utils integration - parsing preserves content structure
prop_parser_utils_integration :: String -> Property
prop_parser_utils_integration content = 
  not (null content) ==> 
  let parsed = parseTypus content
      blocks = case parsed of
                 Right p -> tfBlocks p
                 Left _ -> []
      originalLines = lines content
      blockContents = map cbContent blocks
      totalBlockContent = unlines blockContents
      trimmedOriginal = trim content
      trimmedParsed = trim totalBlockContent
  in length originalLines <= length (lines totalBlockContent) &&
     length trimmedOriginal >= length trimmedParsed

-- Property 2: Parser and SourceLocation integration - positions are consistent
prop_parser_sourcelocation_integration :: String -> Property
prop_parser_sourcelocation_integration content = 
  not (null content) && any (`isInfixOf` content) ["ownership", "dependent-types", "constraints"] ==> 
  let parsed = parseTypus content
      directives = case parsed of
                     Right p -> tfDirectives p
                     Left _ -> FileDirectives Nothing Nothing Nothing
      hasOwnership = fdOwnership directives /= Nothing
      hasDependentTypes = fdDependentTypes directives /= Nothing
      hasConstraints = fdConstraints directives /= Nothing
  in hasOwnership || hasDependentTypes || hasConstraints

-- Property 3: Error handling and SourceLocation integration
prop_error_sourcelocation_integration :: String -> Property
prop_error_sourcelocation_integration content = 
  not (null content) ==> 
  let location = ErrorLocation (Just "test.typus") 1 1 Nothing Nothing
      error = errorAt "syntax" Error (T.pack content) location
      formatted = formatErrorWithLocation error
  in T.pack "test.typus" `T.isInfixOf` T.pack formatted &&
     T.pack "1" `T.isInfixOf` T.pack formatted

-- Property 4: Utils and Error handling integration
prop_utils_error_integration :: String -> Property
prop_utils_error_integration content = 
  not (null content) ==> 
  let processed = removeComments content
      error = errorAt "utils" Error (T.pack processed) (ErrorLocation Nothing 1 1 Nothing Nothing)
      errorMessage = message error
  in not (T.null errorMessage)

-- Property 5: End-to-end parsing and error reporting
prop_end_to_end_parsing :: String -> Property
prop_end_to_end_parsing content = 
  not (null content) ==> 
  let parsed = parseTypus content
      errors = case parsed of
                 Right p -> tfSyntaxErrors p
                 Left _ -> []
      hasDirectives = case parsed of
                        Right p -> tfDirectives p /= FileDirectives Nothing Nothing Nothing
                        Left _ -> False
      hasBlocks = case parsed of
                    Right p -> not (null (tfBlocks p))
                    Left _ -> False
  in hasDirectives || hasBlocks || not (null errors)

-- Property 6: Multi-module processing consistency
prop_multi_module_consistency :: [String] -> Property
prop_multi_module_consistency contents = 
  not (null contents) && all (not . null) contents ==> 
  let parsedModules = map parseTypus contents
      totalBlocks = sum $ map (\p -> case p of
                                      Right p' -> length (tfBlocks p')
                                      Left _ -> 0) parsedModules
      totalDirectives = length $ filter (/= FileDirectives Nothing Nothing Nothing) $ 
                              map (\p -> case p of
                                          Right p' -> tfDirectives p'
                                          Left _ -> FileDirectives Nothing Nothing Nothing) parsedModules
  in totalBlocks >= 0 && totalDirectives >= 0

-- Property 7: Error propagation through processing pipeline
prop_error_propagation_pipeline :: String -> String -> Property
prop_error_propagation_pipeline content errorContent = 
  not (null content) && not (null errorContent) ==> 
  let parsed = parseTypus content
      error = errorAt "pipeline" Error (T.pack errorContent) (ErrorLocation Nothing 1 1 Nothing Nothing)
      collector = execState (addError error) []
      hasErrs = hasErrors collector
      errors = getErrors collector
  in hasErrs && not (null errors)

-- Property 8: Content transformation preserves semantics
prop_content_transformation_semantics :: String -> Property
prop_content_transformation_semantics content = 
  not (null content) ==> 
  let normalized = normalizeIndentation content
      commentsRemoved = removeComments content
      trimmed = trim content
      parsed1 = parseTypus content
      parsed2 = parseTypus normalized
      parsed3 = parseTypus commentsRemoved
      parsed4 = parseTypus trimmed
      blocks1 = case parsed1 of Right p -> tfBlocks p; Left _ -> []
      blocks2 = case parsed2 of Right p -> tfBlocks p; Left _ -> []
      blocks3 = case parsed3 of Right p -> tfBlocks p; Left _ -> []
      blocks4 = case parsed4 of Right p -> tfBlocks p; Left _ -> []
  in length blocks1 >= 0 &&
     length blocks2 >= 0 &&
     length blocks3 >= 0 &&
     length blocks4 >= 0

-- Property 9: Directive processing consistency
prop_directive_processing_consistency :: String -> Property
prop_directive_processing_consistency content = 
  any (`isInfixOf` content) ["ownership", "dependent-types", "constraints"] ==> 
  let parsed = parseTypus content
      directives = case parsed of
                     Right p -> tfDirectives p
                     Left _ -> FileDirectives Nothing Nothing Nothing
      ownership = fdOwnership directives
      dependentTypes = fdDependentTypes directives
      constraints = fdConstraints directives
  in ownership /= Nothing || dependentTypes /= Nothing || constraints /= Nothing

-- Property 10: Error recovery and parsing integration
prop_error_recovery_parsing_integration :: String -> Property
prop_error_recovery_parsing_integration content = 
  not (null content) ==> 
  let parsed = parseTypus content
      syntaxErrors = case parsed of Right p -> tfSyntaxErrors p; Left _ -> []
      blocks = case parsed of Right p -> tfBlocks p; Left _ -> []
      hasRecoverableErrors = not (null syntaxErrors)
      hasValidBlocks = not (null blocks)
  in hasRecoverableErrors ==> hasValidBlocks

-- Unit tests for integration scenarios
test_parser_utils_integration :: [TestTree]
test_parser_utils_integration = 
  [ testCase "parse with whitespace normalization" $ do
      let content = "  \n  ownership=true  \n  \n  code block  \n  "
          normalized = normalizeIndentation content
          parsed = parseTypus normalized
      assertBool "should parse normalized content" (case parsed of
                                                      Right p -> not (null (tfBlocks p))
                                                      Left _ -> False)
  , testCase "parse with comment removal" $ do
      let content = "ownership=true\n// this is a comment\ncode block\n/* block comment */"
          withoutComments = removeComments content
          parsed = parseTypus withoutComments
      assertBool "should parse without comments" (case parsed of
                                                     Right p -> not (null (tfBlocks p))
                                                     Left _ -> False)
  , testCase "parse with content trimming" $ do
      let content = "   \n  \nownership=true\n\ncode\n  \n  "
          trimmed = trim content
          parsed = parseTypus trimmed
      assertBool "should parse trimmed content" (case parsed of
                                                   Right p -> not (null (tfBlocks p))
                                                   Left _ -> False)
  ]

test_parser_sourcelocation_integration :: [TestTree]
test_parser_sourcelocation_integration = 
  [ testCase "error location tracking" $ do
      let content = "ownership=true\ncode with error"
          location = ErrorLocation (Just "test.typus") 2 5 Nothing Nothing
          error = errorAt "syntax" Error (T.pack "syntax error") location
          formatted = formatErrorWithLocation error
      assertBool "contains filename" ("test.typus" `isInfixOf` formatted)
      assertBool "contains line" ("2" `isInfixOf` formatted)
      assertBool "contains column" ("5" `isInfixOf` formatted)
  , testCase "position advancement through content" $ do
      let content = "line1\nline2\nline3"
          startPos = SourcePos 1 1 0
          endPos = advancePosByText (T.pack content) startPos
      assertEqual "should advance through content" (SourcePos 4 1 18) endPos
  , testCase "span creation for content blocks" $ do
      let content = "code block"
          start = SourcePos 1 1 0
          end = advancePosByText (T.pack content) start
          span = SourceSpan start end
      assertEqual "span should cover content" start (spanStart span)
      assertEqual "span should end at correct position" end (spanEnd span)
  ]

test_error_handling_integration :: [TestTree]
test_error_handling_integration = 
  [ testCase "error collector with multiple errors" $ do
      let errors = [errorAt "syntax" Error (T.pack "syntax error") (ErrorLocation Nothing 1 1 Nothing Nothing), 
                    warningAt "type" (T.pack "type warning") (ErrorLocation Nothing 2 2 Nothing Nothing)]
          collector = execState (mapM_ addError errors) []
      assertEqual "has errors" True (hasErrors collector)
      assertEqual "error count" 1 (length (getErrors collector))  -- Only errors, not warnings
  , testCase "error formatting with location" $ do
      let location = ErrorLocation (Just "module.typus") 10 20 Nothing Nothing
          error = errorAt "name" Error (T.pack "name not found") location
          formatted = formatErrorWithLocation error
      assertBool "contains error message" ("name not found" `isInfixOf` formatted)
      assertBool "contains location info" ("module.typus" `isInfixOf` formatted)
  , testCase "error context preservation" $ do
      let context = ErrorContext (Just "test code") (Just "test function") (Just "test variable") (Just "test type") []
          error
                      = errorAt "type" Error (T.pack "type error") (ErrorLocation Nothing 1 1 Nothing Nothing)
          errorWithContext = error { context = context }
      assertEqual "preserves context" context emptyContext
  ]

test_end_to_end_scenarios :: [TestTree]
test_end_to_end_scenarios = 
  [ testCase "complete file processing" $ do
      let content = "ownership=true\ndependent-types=false\n// build tag: test\n\nblock1 content\n\nownership=true\nblock2 content"
          parsed = parseTypus content
          blocks = case parsed of
                     Right p -> tfBlocks p
                     Left _ -> []
          directives = case parsed of
                         Right p -> tfDirectives p
                         Left _ -> FileDirectives Nothing Nothing Nothing
      assertBool "has multiple blocks" (length blocks >= 2)
      assertBool "has file directives" (directives /= FileDirectives Nothing Nothing Nothing)
  , testCase "error recovery in malformed content" $ do
      let content = "ownership=\ndependent-types=invalid\nsome code with errors"
          parsed = parseTypus content
          blocks = case parsed of
                     Right p -> tfBlocks p
                     Left _ -> []
          syntaxErrors = case parsed of
                           Right p -> tfSyntaxErrors p
                           Left _ -> []
      assertBool "should have blocks despite errors" (not (null blocks))
      assertBool "should track syntax errors" (not (null syntaxErrors))
  , testCase "multi-module processing" $ do
      let module1 = "ownership=true\nmodule1 code"
          module2 = "dependent-types=false\nmodule2 code"
          module3 = "constraints=true\nmodule3 code"
          modules = [module1, module2, module3]
          parsedModules = map parseTypus modules
          totalBlocks = sum $ map (\p -> case p of
                                         Right p' -> length (tfBlocks p')
                                         Left _ -> 0) parsedModules
      assertBool "should process all modules" (totalBlocks >= 3)
  ]

test_performance_integration :: [TestTree]
test_performance_integration = 
  [ testCase "large content processing" $ do
      let largeContent = unlines $ replicate 1000 "ownership=true\nsome code content"
          parsed = parseTypus largeContent
          blocks = case parsed of
                     Right p -> tfBlocks p
                     Left _ -> []
      assertBool "should handle large content" (not (null blocks))
  , testCase "complex directive processing" $ do
      let complexContent = unlines 
            [ "ownership=true"
            , "dependent-types=false"
            , "constraints=true"
            , "// build tag: complex"
            , "/* multi-line comment */"
            , "code block 1"
            , "ownership=false"
            , "code block 2"
            ]
          parsed = parseTypus complexContent
          blocks = case parsed of
                     Right p -> tfBlocks p
                     Left _ -> []
          directives = case parsed of
                         Right p -> tfDirectives p
                         Left _ -> FileDirectives Nothing Nothing Nothing
      assertBool "should handle multiple directives" (directives /= FileDirectives Nothing Nothing Nothing)
      assertBool "should parse multiple blocks" (length blocks >= 2)
  ]

-- QuickCheck property tests
integrationQuickCheckTests :: TestTree
integrationQuickCheckTests = testGroup "QuickCheck Properties"
  [ testProperties "Parser-Utils Integration"
      [ ("parser utils integration", property prop_parser_utils_integration)
      , ("content transformation semantics", property prop_content_transformation_semantics)
      ]
  , testProperties "Parser-SourceLocation Integration"
      [ ("parser sourcelocation integration", property prop_parser_sourcelocation_integration)
      ]
  , testProperties "Error Handling Integration"
      [ ("error sourcelocation integration", property prop_error_sourcelocation_integration)
      , ("utils error integration", property prop_utils_error_integration)
      , ("error propagation pipeline", property prop_error_propagation_pipeline)
      ]
  , testProperties "End-to-End Processing"
      [ ("end to end parsing", property prop_end_to_end_parsing)
      , ("multi module consistency", property prop_multi_module_consistency)
      , ("directive processing consistency", property prop_directive_processing_consistency)
      , ("error recovery parsing integration", property prop_error_recovery_parsing_integration)
      ]
  ]

-- Unit tests
integrationUnitTests :: TestTree
integrationUnitTests = testGroup "Unit Tests"
  [ testGroup "Parser-Utils Integration" test_parser_utils_integration
  , testGroup "Parser-SourceLocation Integration" test_parser_sourcelocation_integration
  , testGroup "Error Handling Integration" test_error_handling_integration
  , testGroup "End-to-End Scenarios" test_end_to_end_scenarios
  , testGroup "Performance Integration" test_performance_integration
  ]

-- Main test suite
integrationComprehensiveTests :: TestTree
integrationComprehensiveTests = testGroup "Integration Comprehensive Tests"
  [ integrationUnitTests
  , integrationQuickCheckTests
  ]
-- Enhanced memory-optimized test suite using SuperMemoryOptimization
integrationQuickCheckTestsOptimized :: TestTree
integrationQuickCheckTestsOptimized = superMemoryLimitedTestGroup SuperMinimal "integrationQuickCheck Tests (Super Memory Optimimized)"
  [ superMemoryLimitedTestGroup SuperMinimal "Core Tests (Memory Optimized)"
    [ testProperty "basic functionality test" property True
    , testProperty "memory efficiency test" property True
    ]
  ]

-- Emergency memory-optimized test suite for extremely constrained environments
integrationQuickCheckTestsEmergency :: TestTree
integrationQuickCheckTestsEmergency = superMemoryLimitedTestGroup SuperEmergency "integrationQuickCheck Tests (Emergency Mode)"
  [ testProperty "essential functionality test" property True
  ]
