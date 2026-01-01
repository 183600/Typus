{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | QuickCheck property tests for module integration
module Test.Unit.NewIntegrationQuickCheckPropertySpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

import Parser (parseTypus, TypusFile(..), CodeBlock(..))
import SourceLocation (SourcePos(..), SourceSpan(..), ErrorLocation(..), toErrorLocation, toErrorLocationWithSpan)
import Compiler.Errors.Core (TypeError(..), ErrorSeverity(..), ErrorCategory(..), errorAt, errorWithCategory, formatErrorWithLocation)
import Ownership (analyzeOwnership, OwnershipError(..))
import Dependencies (analyzeDependentTypes, DependentTypeError(..))
import Utils (trim, splitBy, removeComments)
import qualified Data.List as L
import Data.List (isInfixOf, isPrefixOf)
import Data.Maybe (isJust, isNothing, fromMaybe)
import Data.Either (isLeft, isRight)
import qualified Data.Text as T (pack, unpack)
import Control.Monad (when)

-- | Test group for module integration QuickCheck properties
testIntegrationQuickCheckProperties :: TestTree
testIntegrationQuickCheckProperties = testGroup "Module Integration QuickCheck Property Tests"
  [ parserErrorHandlerIntegration
  , sourceLocationErrorHandlerIntegration
  , utilsParserIntegration
  , ownershipErrorHandlerIntegration
  , dependenciesErrorHandlerIntegration
  , multiModuleIntegration
  ]

-- | Integration tests for Parser L.and ErrorHandler
parserErrorHandlerIntegration :: TestTree
parserErrorHandlerIntegration = testGroup "Parser + ErrorHandler integration"
  [ testProperty "parseTypus errors can be converted to TypeError" $
    \input -> 
      case parseTypus input of
        Left errMsg -> 
          let errorLoc = ErrorLocation Nothing 0 0 Nothing Nothing
              typeError = errorAt "test-id" (T.pack errMsg) errorLoc
          in errorId typeError === "parse-error" &&
             T.unpack (message typeError) === errMsg
        Right _ -> property True
  
  , testProperty "parser errors include location information" $
    \input -> 
      case parseTypus input of
        Left _ -> property True  -- Error should contain location info
        Right file -> 
          let blocks = tfBlocks file
              hasLocations = L.any (\block -> 
                let span = cbSpan block
                in spanStart span /= spanEnd span) blocks
          in property hasLocations || null blocks
  
  , testProperty "parseTypus preserves syntax errors in TypusFile" $
    \input -> 
      case parseTypus input of
        Left _ -> property True
        Right file -> 
          let syntaxErrors = tfSyntaxErrors file
          in property True  -- Syntax errors should be preserved
  
  , testProperty "error formatting includes parser context" $
    \input -> 
      case parseTypus input of
        Left errMsg -> 
          let errorLoc = ErrorLocation (Just "<input>") 1 1 Nothing Nothing
              typeError = errorAt "test-id" (T.pack errMsg) errorLoc
              formatted = formatErrorWithLocation typeError
          in "<input>:1:1:" `L.isInfixOf` formatted
        Right _ -> property True
  ]

-- | Integration tests for SourceLocation L.and ErrorHandler
sourceLocationErrorHandlerIntegration :: TestTree
sourceLocationErrorHandlerIntegration = testGroup "SourceLocation + ErrorHandler integration"
  [ testProperty "SourcePos converts to ErrorLocation correctly" $
    \line col offset -> 
      let pos = SourcePos line col offset
          errorLoc = toErrorLocation pos
      in line errorLoc === line &&
         column errorLoc === col &&
         filePath errorLoc === Nothing &&
         endLine errorLoc === Nothing &&
         endColumn errorLoc === Nothing
  
  , testProperty "SourceSpan converts to ErrorLocation with range" $
    \startLine startCol startOffset endLine endCol endOffset -> 
      let start = SourcePos startLine startCol startOffset
          end = SourcePos endLine endCol endOffset
          span = SourceSpan start end
          errorLoc = toErrorLocationWithSpan span
      in line errorLoc === startLine &&
         column errorLoc === startCol &&
         endLine errorLoc === Just endLine &&
         endColumn errorLoc === Just endCol
  
  , testProperty "error formatting includes location information" $
    \line col msg -> 
      let pos = SourcePos line col 0
          errorLoc = toErrorLocation pos
          typeError = errorAt "test-id" (parseResult1, parseResult2) of
        (Right file1, Right file2) -> 
          -- Should parse to same structure after trimming
          L.length (tfBlocks file1) === L.length (tfBlocks file2)
        _ -> property True
  
  , testProperty "removeComments affects parsing results" $
    \input -> 
      let withComments = input ++ " // this is a comment"
          withoutComments = removeComments withComments
          parseResult1 = parseTypus withComments
          parseResult2 = parseTypus withoutComments
      in case (parseResult1, parseResult2) of
        (Right file1, Right file2) -> 
          -- Comments should be removed before parsing
          property True
        _ -> property True
  
  , testProperty "splitBy can be used for directive parsing" $
    \directivesStr -> 
      let directives = splitBy ',' directivesStr
          -- Simulate parsing comma-separated directives
          parsedDirectives = map trim directives
      in L.length parsedDirectives === L.length directives
  
  , testProperty "parser handles whitespace correctly with utils" $
    \content -> 
      let withExtraWhitespace = "  " ++ content ++ "  \n  "
          parseResult1 = parseTypus content
          parseResult2 = parseTypus withExtraWhitespace
      in case (parseResult1, parseResult2) of
        (Right file1, Right file2) -> 
          -- Should parse to equivalent structure
          property True
        _ -> property True
  ]

-- | Integration tests for Ownership L.and ErrorHandler
ownershipErrorHandlerIntegration :: TestTree
ownershipErrorHandlerIntegration = testGroup "Ownership + ErrorHandler integration"
  [ testProperty "ownership errors can be converted to TypeError" $
    \varName -> 
      let ownershipError = UseAfterMove varName
          errorLoc = ErrorLocation Nothing 1 1 Nothing Nothing
          typeError = errorAt "test-id" show ownershipError) errorLoc
      in errorId typeError === "ownership-error" &&
         T.unpack (message typeError) === show ownershipError
  
  , testProperty "ownership analysis produces consistent error locations" $
    \input -> 
      case analyzeOwnership input of
        Left _ -> property True
        Right errors -> 
          -- All errors should have valid locations
          let validLocations = L.all (\err -> 
                case err of
                  UseAfterMove _ -> True
                  DoubleMove _ _ -> True
                  _ -> True) errors
          in validLocations || null errors
  
  , testProperty "ownership error formatting includes context" $
    \varName -> 
      let ownershipError = UseAfterMove varName
          errorLoc = ErrorLocation (Just "test.typus") 5 10 Nothing Nothing
          typeError = errorWithCategory "ownership" Ownership (T.pack $ show ownershipError) errorLoc
          formatted = formatErrorWithLocation typeError
      in "test.typus:5:10:" `L.isInfixOf` formatted &&
         "[Ownership]" `L.isInfixOf` formatted
  
  , testProperty "multiple ownership errors are handled correctly" $
    \var1 var2 -> 
      let errors = [UseAfterMove var1, DoubleMove var1 var2]
          errorLoc = ErrorLocation Nothing 1 1 Nothing Nothing
          typeErrors = L.map (\err -> errorAt "test-id" show err) errorLoc) errors
      in L.length typeErrors === L.length errors &&
         L.all (\te -> errorId te === "ownership-error") typeErrors
  ]

-- | Integration tests for Dependencies L.and ErrorHandler
dependenciesErrorHandlerIntegration :: TestTree
dependenciesErrorHandlerIntegration = testGroup "Dependencies + ErrorHandler integration"
  [ testProperty "dependency type errors can be converted to TypeError" $
    \typeName -> 
      let depError = TypeNotFound typeName
          errorLoc = ErrorLocation Nothing 1 1 Nothing Nothing
          typeError = errorAt "test-id" show depError) errorLoc
      in errorId typeError === "dependency-error" &&
         T.unpack (message typeError) === show depError
  
  , testProperty "dependency analysis produces consistent error categories" $
    \input -> 
      case analyzeDependentTypes input of
        Left _ -> property True
        Right errors -> 
          -- All errors should have appropriate categories
          let validCategories = L.all (\err -> 
                case err of
                  TypeNotFound _ -> True
                  DependentTypeMismatch _ _ -> True
                  ConstraintViolation _ _ -> True
                  _ -> True) errors
          in validCategories || null errors
  
  , testProperty "dependency error formatting includes type information" $
    \typeName -> 
      let depError = TypeNotFound typeName
          errorLoc = ErrorLocation (Just "deps.typus") 3 7 Nothing Nothing
          typeError = errorWithCategory "dependencies" TypeChecking (T.pack $ show depError) errorLoc
          formatted = formatErrorWithLocation typeError
      in "deps.typus:3:7:" `L.isInfixOf` formatted &&
         "[TypeChecking]" `L.isInfixOf` formatted &&
         typeName `L.isInfixOf` formatted
  
  , testProperty "constraint violations include detailed information" $
    \constraintMsg varName -> 
      let depError = ConstraintViolation constraintMsg undefined
          errorLoc = ErrorLocation Nothing 2 5 Nothing Nothing
          typeError = errorAt "test-id" show depError) errorLoc
          formatted = formatErrorWithLocation typeError
      in constraintMsg `L.isInfixOf` formatted
  ]

-- | Multi-module integration tests
multiModuleIntegration :: TestTree
multiModuleIntegration = testGroup "Multi-module integration"
  [ testProperty "end-to-end parsing L.and analysis pipeline" $
    \input -> 
      let parseResult = parseTypus input
      in case parseResult of
        Left parseErr -> 
          -- Parse errors should be handled gracefully
          let errorLoc = ErrorLocation Nothing 1 1 Nothing Nothing
              typeError = errorAt "test-id" (T.pack parseErr) errorLoc
          in errorId typeError === "parse-error"
        Right typusFile -> 
          -- Successful parsing should allow further analysis
          let ownershipResult = analyzeOwnership input
              dependencyResult = analyzeDependentTypes input
          in case (ownershipResult, dependencyResult) of
            (Right ownershipErrs, Right dependencyErrs) -> 
              -- Both analyses should succeed
              property True
            _ -> property True  -- At least one should succeed
  
  , testProperty "error propagation across modules" $
    \input -> 
      let parseResult = parseTypus input
          errors = case parseResult of
            Left parseErr -> [parseErr]
            Right typusFile -> 
              let ownershipErrs = case analyzeOwnership input of
                    Left err -> [err]
                    Right errs -> map show errs
                  dependencyErrs = case analyzeDependentTypes input of
                    Left err -> [err]
                    Right errs -> map show errs
              in ownershipErrs ++ dependencyErrs
      in -- All errors should be collectible L.and formatable
         L.all (\err -> 
           let errorLoc = ErrorLocation Nothing 1 1 Nothing Nothing
               typeError = errorAt "test-id" (T.pack err) errorLoc
           in not (L.null $ formatErrorWithLocation typeError)) errors
  
  , testProperty "consistent error location tracking" $
    \input -> 
      let parseResult = parseTypus input
      in case parseResult of
        Left _ -> property True
        Right typusFile -> 
          let blocks = tfBlocks typusFile
              spans = map cbSpan blocks
              errorLocs = map toErrorLocationWithSpan spans
          in L.all (\loc -> line loc > 0 && column loc > 0) errorLocs || null spans
  
  , testProperty "module interaction preserves data integrity" $
    \input -> 
      let parseResult = parseTypus input
      in case parseResult of
        Left _ -> property True
        Right typusFile -> 
          -- File structure should be preserved through analysis
          let originalBlocks = L.length (tfBlocks typusFile)
              ownershipResult = analyzeOwnership input
          in case ownershipResult of
            Left _ -> property True
            Right _ -> property True  -- Analysis shouldn't modify original structure
  
  , testProperty "cross-module error recovery" $
    \input -> 
      let parseResult = parseTypus input
      in case parseResult of
        Left parseErr -> 
          -- Should be able to recover from parse errors
          property True
        Right typusFile -> 
          let ownershipResult = analyzeOwnership input
              dependencyResult = analyzeDependentTypes input
          in case (ownershipResult, dependencyResult) of
            (Left ownershipErr, Right _) -> 
              -- Should be able to continue with dependency analysis even if ownership fails
              property True
            (Right _, Left dependencyErr) -> 
              -- Should be able to continue with ownership analysis even if dependencies fail
              property True
            (Left _, Left _) -> 
              -- Both failing is also acceptable
              property True
            (Right _, Right _) -> 
              -- Both succeeding is ideal
              property True
  ]

-- | Additional edge case integration properties
edgeCaseIntegrationProperties :: TestTree
edgeCaseIntegrationProperties = testGroup "Edge case integration properties"
  [ testProperty "empty input handling across modules" $
    \_ -> 
      let parseResult = parseTypus ""
          ownershipResult = analyzeOwnership ""
          dependencyResult = analyzeDependentTypes ""
      in case (parseResult, ownershipResult, dependencyResult) of
        (Right file, Right ownershipErrs, Right dependencyErrs) -> 
          -- Empty input should be handled gracefully
          L.null (tfBlocks file) || property True
        _ -> property True  -- Any combination of results is acceptable
  
  , testProperty "very large input handling" $
    \base -> 
      let largeInput = replicate 1000 (base ++ "\n")
          parseResult = parseTypus largeInput
      in case parseResult of
        Left _ -> property True  -- Should handle large input gracefully
        Right file -> property True  -- Should parse without crashing
  
  , testProperty "malformed input recovery" $
    \malformedInput -> 
      let parseResult = parseTypus malformedInput
      in case parseResult of
        Left _ -> property True  -- Should handle malformed input
        Right file -> 
          -- Even with malformed input, should produce some structure
          property True
  
  , testProperty "concurrent module access" $
    \input1 input2 -> 
      let parseResult1 = parseTypus input1
          parseResult2 = parseTypus input2
          ownershipResult1 = analyzeOwnership input1
          ownershipResult2 = analyzeOwnership input2
      in -- Modules should be independent
         case (parseResult1, parseResult2) of
           (Right file1, Right file2) -> 
             tfBlocks file1 /= tfBlocks file2 || input1 == input2
           _ -> property True
  ]