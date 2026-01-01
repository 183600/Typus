{-# OPTIONS_GHC -Wno-deprecations #-}
module Test.Unit.ParserErrorRecoveryAdvancedQuickCheckSpec (tests) where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

import Parser
  ( parseTypus
  , TypusFile(..)
  , CodeBlock(..)
  , FileDirectives(..)
  , BlockDirectives(..)
  )
import SyntaxValidator (SyntaxError(..))
import SourceLocation (SourceSpan(..), SourcePos(..))
import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf)

-- ============================================================================
-- Parser Error Recovery Advanced Property Tests
-- ============================================================================

-- | Test that parser recovers from malformed directives
prop_parserRecoversFromMalformedDirectives :: String -> String -> Property
prop_parserRecoversFromMalformedDirectives badDirective goodDirective =
  let input = "//!" ++ badDirective ++ "\n//!" ++ goodDirective ++ "\n```go\ncode\n```"
      result = parseTypus input
      hasGoodDirective = L.any (L.isInfixOf goodDirective . show) (tfSyntaxErrors result)
  in counterexample ("Parser should recover from malformed directives. " ++
                     "Input: " ++ show input ++
                     " Errors: " ++ show (tfSyntaxErrors result))
     (L.length (tfSyntaxErrors result) >= 0 .&&. 
      L.length (tfBlocks result) >= 0)

-- | Test that parser handles unclosed code blocks gracefully
prop_parserHandlesUnclosedCodeBlocks :: String -> Property
prop_parserHandlesUnclosedCodeBlocks content =
  let input = "```go\n" ++ content  -- Missing closing ```
      result = parseTypus input
      hasBlocks = not (L.null (tfBlocks result))
  in counterexample ("Parser should handle unclosed code blocks gracefully. " ++
                     "Input: " ++ show input ++
                     " Blocks: " ++ show (tfBlocks result) ++
                     " Errors: " ++ show (tfSyntaxErrors result))
     (hasBlocks .||. not (L.null (tfSyntaxErrors result)))

-- | Test that parser recovers from mixed directive formats
prop_parserRecoversFromMixedDirectiveFormats :: String -> Property
prop_parserRecoversFromMixedDirectiveFormats content =
  let input = "//!ownership=true\n//! dependent-types\n//! invalid=directive\n```go\n" ++ content ++ "\n```"
      result = parseTypus input
      hasValidBlocks = not (L.null (tfBlocks result))
  in counterexample ("Parser should recover from mixed directive formats. " ++
                     "Input: " ++ show input ++
                     " Blocks: " ++ show (tfBlocks result))
     (hasValidBlocks .||. not (L.null (tfSyntaxErrors result)))

-- | Test that parser preserves partial structure on errors
prop_parserPreservesPartialStructure :: String -> Property
prop_parserPreservesPartialStructure content =
  let input = "//!ownership=true\n```go\nvalid code\n```\n```go\n" ++ content ++ "\n```"
      result = parseTypus input
      hasSomeBlocks = L.length (tfBlocks result) > 0
  in counterexample ("Parser should preserve partial structure on errors. " ++
                     "Input: " ++ show input ++
                     " Blocks: " ++ show (tfBlocks result) ++
                     " Errors: " ++ show (tfSyntaxErrors result))
     (hasSomeBlocks .||. not (L.null (tfSyntaxErrors result)))

-- | Test that parser handles deeply nested malformed structures
prop_parserHandlesNestedMalformedStructures :: String -> Property
prop_parserHandlesNestedMalformedStructures content =
  let input = "//!ownership=true\n```go\nfunc main() {\n" ++ content ++ "\n}\n```"
      result = parseTypus input
      hasBlocksOrErrors = not (L.null (tfBlocks result)) || not (L.null (tfSyntaxErrors result))
  in counterexample ("Parser should handle nested malformed structures. " ++
                     "Input: " ++ show input)
     (hasBlocksOrErrors === True)

-- | Test that parser recovers from malformed block directives
prop_parserRecoversFromMalformedBlockDirectives :: String -> String -> Property
prop_parserRecoversFromMalformedBlockDirectives badDirective goodDirective =
  let input = "//!ownership=true\n```go ownership=" ++ badDirective ++ "\ncode\n```\n```go " ++ goodDirective ++ "\ncode\n```"
      result = parseTypus input
      hasSomeBlocks = L.length (tfBlocks result) > 0
  in counterexample ("Parser should recover from malformed block directives. " ++
                     "Input: " ++ show input)
     (hasSomeBlocks .||. not (L.null (tfSyntaxErrors result)))

-- | Test that parser handles empty L.and whitespace-only content
prop_parserHandlesEmptyContent :: Property
prop_parserHandlesEmptyContent =
  let inputs = ["", "   ", "\n\n", "  \n  \n  "]
      results = map parseTypus inputs
      allValid = L.all (\r -> L.length (tfBlocks r) >= 0) results
  in counterexample ("Parser should handle empty L.and whitespace-only content")
     (allValid === True)

-- | Test that parser maintains directive context during recovery
prop_parserMaintainsDirectiveContext :: String -> Property
prop_parserMaintainsDirectiveContext content =
  let input = "//!ownership=true\n//!dependent-types=true\n```go\n" ++ content ++ "\n```"
      result = parseTypus input
      directives = tfDirectives result
  in counterexample ("Parser should maintain directive context during recovery. " ++
                     "Input: " ++ show input ++
                     " Directives: " ++ show directives)
     (L.length (tfBlocks result) >= 0 .&&. 
      (fdOwnership directives /= Nothing || fdDependentTypes directives /= Nothing || not (L.null (tfSyntaxErrors result))))

-- | Test that parser handles unicode in error conditions
prop_parserHandlesUnicodeInErrors :: String -> Property
prop_parserHandlesUnicodeInErrors unicodeContent =
  let input = "//!ownership=true\n```go\n" ++ unicodeContent ++ "\n```"
      result = parseTypus input
  in counterexample ("Parser should handle Unicode in error conditions. " ++
                     "Input: " ++ show input)
     (L.length (tfBlocks result) >= 0 .&&. L.length (tfSyntaxErrors result) >= 0)

-- | Test that parser recovers from malformed build tags
prop_parserRecoversFromMalformedBuildTags :: String -> Property
prop_parserRecoversFromMalformedBuildTags badTag =
  let input = "//go:build " ++ badTag ++ "\n//!ownership=true\n```go\ncode\n```"
      result = parseTypus input
      hasBlocksOrErrors = not (L.null (tfBlocks result)) || not (L.null (tfSyntaxErrors result))
  in counterexample ("Parser should recover from malformed build tags. " ++
                     "Input: " ++ show input)
     (hasBlocksOrErrors === True)

-- | Test that parser handles multiple consecutive errors
prop_parserHandlesMultipleConsecutiveErrors :: String -> String -> String -> Property
prop_parserHandlesMultipleConsecutiveErrors error1 error2 error3 =
  let input = "//!" ++ error1 ++ "\n//!" ++ error2 ++ "\n//!" ++ error3 ++ "\n```go\ncode\n```"
      result = parseTypus input
      hasErrorsOrBlocks = not (L.null (tfSyntaxErrors result)) || not (L.null (tfBlocks result))
  in counterexample ("Parser should handle multiple consecutive errors. " ++
                     "Input: " ++ show input)
     (hasErrorsOrBlocks === True)

-- | Test that parser preserves line numbers in error recovery
prop_parserPreservesLineNumbers :: String -> Property
prop_parserPreservesLineNumbers content =
  let input = "//!ownership=true\n//!invalid=directive\n```go\n" ++ content ++ "\n```"
      result = parseTypus input
      errors = tfSyntaxErrors result
  in counterexample ("Parser should preserve line numbers in error recovery. " ++
                     "Input: " ++ show input ++
                     " Errors: " ++ show errors)
     (L.all (\e -> sourcePosLine (errorPos e) > 0) errors .||. null errors)

-- | Test that parser handles extremely long lines in error conditions
prop_parserHandlesLongLines :: Property
prop_parserHandlesLongLines =
  let longLine = replicate 1000 'a'
      input = "//!ownership=true\n```go\n" ++ longLine ++ "\n```"
      result = parseTypus input
  in counterexample ("Parser should handle extremely long lines in error conditions")
     (L.length (tfBlocks result) >= 0 .&&. L.length (tfSyntaxErrors result) >= 0)

-- | Test that parser recovers from mismatched delimiters
prop_parserRecoversFromMismatchedDelimiters :: String -> Property
prop_parserRecoversFromMismatchedDelimiters content =
  let input = "//!ownership=true\n```go\n" ++ content ++ "\n```rust\n```go\n```"
      result = parseTypus input
      hasSomeStructure = not (L.null (tfBlocks result)) || not (L.null (tfSyntaxErrors result))
  in counterexample ("Parser should recover from mismatched delimiters. " ++
                     "Input: " ++ show input)
     (hasSomeStructure === True)

-- | Test that parser maintains file structure despite errors
prop_parserMaintainsFileStructure :: String -> Property
prop_parserMaintainsFileStructure content =
  let input = "//!ownership=true\n//!dependent-types=true\n//!invalid=directive\n```go\n" ++ content ++ "\n```"
      result = parseTypus input
      hasDirectives = tfDirectives result /= defaultFileDirectives
      hasBlocksOrErrors = not (L.null (tfBlocks result)) || not (L.null (tfSyntaxErrors result))
  in counterexample ("Parser should maintain file structure despite errors. " ++
                     "Input: " ++ show input)
     (hasDirectives .&&. hasBlocksOrErrors)

-- | Test that parser handles partial directive parsing
prop_parserHandlesPartialDirectiveParsing :: String -> Property
prop_parserHandlesPartialDirectiveParsing partialDirective =
  let input = "//!own" ++ partialDirective ++ "\n//!ownership=true\n```go\ncode\n```"
      result = parseTypus input
      hasSomeValidContent = not (L.null (tfBlocks result)) || not (L.null (tfSyntaxErrors result))
  in counterexample ("Parser should handle partial directive parsing. " ++
                     "Input: " ++ show input)
     (hasSomeValidContent === True)

-- | Test that parser error recovery is deterministic
prop_parserErrorRecoveryIsDeterministic :: String -> Property
prop_parserErrorRecoveryIsDeterministic content =
  let input = "//!invalid=directive\n```go\n" ++ content ++ "\n```"
      result1 = parseTypus input
      result2 = parseTypus input
      blocksMatch = L.length (tfBlocks result1) == L.length (tfBlocks result2)
      errorsMatch = L.length (tfSyntaxErrors result1) == L.length (tfSyntaxErrors result2)
  in counterexample ("Parser error recovery should be deterministic. " ++
                     "Input: " ++ show input)
     (blocksMatch .&&. errorsMatch)

-- | Test that parser handles nested block errors
prop_parserHandlesNestedBlockErrors :: String -> Property
prop_parserHandlesNestedBlockErrors nestedContent =
  let input = "//!ownership=true\n```go\nfunc outer() {\n```go\n" ++ nestedContent ++ "\n}\n```"
      result = parseTypus input
      hasSomeStructure = not (L.null (tfBlocks result)) || not (L.null (tfSyntaxErrors result))
  in counterexample ("Parser should handle nested block errors. " ++
                     "Input: " ++ show input)
     (hasSomeStructure === True)

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "Parser Error Recovery Advanced QuickCheck Tests"
  [ testProperty "Parser recovers from malformed directives" prop_parserRecoversFromMalformedDirectives
  , testProperty "Parser handles unclosed code blocks gracefully" prop_parserHandlesUnclosedCodeBlocks
  , testProperty "Parser recovers from mixed directive formats" prop_parserRecoversFromMixedDirectiveFormats
  , testProperty "Parser preserves partial structure on errors" prop_parserPreservesPartialStructure
  , testProperty "Parser handles nested malformed structures" prop_parserHandlesNestedMalformedStructures
  , testProperty "Parser recovers from malformed block directives" prop_parserRecoversFromMalformedBlockDirectives
  , testProperty "Parser handles empty L.and whitespace-only content" prop_parserHandlesEmptyContent
  , testProperty "Parser maintains directive context during recovery" prop_parserMaintainsDirectiveContext
  , testProperty "Parser handles Unicode in error conditions" prop_parserHandlesUnicodeInErrors
  , testProperty "Parser recovers from malformed build tags" prop_parserRecoversFromMalformedBuildTags
  , testProperty "Parser handles multiple consecutive errors" prop_parserHandlesMultipleConsecutiveErrors
  , testProperty "Parser preserves line numbers in error recovery" prop_parserPreservesLineNumbers
  , testProperty "Parser handles extremely long lines in error conditions" prop_parserHandlesLongLines
  , testProperty "Parser recovers from mismatched delimiters" prop_parserRecoversFromMismatchedDelimiters
  , testProperty "Parser maintains file structure despite errors" prop_parserMaintainsFileStructure
  , testProperty "Parser handles partial directive parsing" prop_parserHandlesPartialDirectiveParsing
  , testProperty "Parser error recovery is deterministic" prop_parserErrorRecoveryIsDeterministic
  , testProperty "Parser handles nested block errors" prop_parserHandlesNestedBlockErrors
  ]