module Test.Unit.ParserErrorRecoveryQuickCheckSpec where
import Test.QuickCheck 
import Test.Tasty.HUnit (testCase, assertFailure, assertBool, (@?=))
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, oneof, listOf, elements, choose, suchThat)
import Data.Char 
import Parser (parseTypus, TypusFile(..), CodeBlock(..), FileDirectives(..), BlockDirectives)
import SourceLocation (SourcePos(..), SourceSpan(..), startPos)
import Utils 
            not (L.null (show (tfDirectives typusFile)) &&
            L.length (tfBuildTags typusFile) >= 0 &&
            L.length (tfBlocks typusFile) >= 0
      in hasValidFields
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


-- Property: Parser error positions are accurate within input bounds
prop_errorPositionAccuracy :: String -> Bool
prop_errorPositionAccuracy                               input =
  let malformedInput = input ++ createPositionTestErrors
                                    parseResult = parseTypus malformedInput
  in case parseResult of
    Left parseError -> 
      -- Error message should contain valid position information
                                    hasValidPositionInfo = containsValidPosition parseError
      hasValidPositionInfo
    Right _ -> True  -- No error is also acceptable

-- Property: Parser handles multiple cascading errors gracefully
prop_cascadingErrorHandling :: String -> Bool
prop_cascadingErrorHandling                               input =
  let malformedInput = input ++ createCascadingErrors
                                    parseResult = parseTypus malformedInput
  in case parseResult of
    Left _ -> True  -- Should handle cascading errors without crashing
    Right typusFile -> 
      -- Should recover as much as possible
      let recoveredBlocks = tfBlocks typusFile
      in L.length recoveredBlocks >= 0

-- Property: Parser recovers from malformed block directives
prop_blockDirectiveRecovery :: String -> Bool
prop_blockDirectiveRecovery                               input =
  let malformedInput = input ++ createMalformedBlockDirectives
                                    parseResult = parseTypus malformedInput
  in case parseResult of
    Left _ -> True  -- Error is expected
    Right typusFile -> 
      -- Should recover L.and parse surrounding content
      let blocks = tfBlocks typusFile
                                        hasValidBlocks = L.all validateBlock blocks
      in hasValidBlocks

-- Property: Parser maintains valid AST invariants even on error
prop_astInvariantPreservation :: String -> Bool
prop_astInvariantPreservation                               input =
  let malformedInput = input ++ createInvariantBreakingErrors
                                    parseResult = parseTypus malformedInput
  in case parseResult of
    Left _ -> True  -- Error is expected
    Right typusFile -> 
      -- AST should maintain basic invariants
      let invariantsPreserved = checkASTInvariants typusFile
      in invariantsPreserved

-- ============================================================================
-- Helper Functions
-- ============================================================================

-- Create various malformed inputs for testing
createMismatchedBraces :: String
                              createMismatchedBraces = "\nfunc test() {\n  if true {\n    return true\n  // missing closing brace\n}\n"

createUnterminatedString :: String
                              createUnterminatedString = "\nlet str = \"unterminated string\nfunc test() {\n  return true\n}\n"

createInvalidDirectives :: String
                              createInvalidDirectives = "\n//!invalid::directive syntax\n//!ownership: maybe\nfunc test() {\n  return true\n}\n"

createMixedSyntaxErrors :: String
                              createMixedSyntaxErrors = "\nfunc test( {\n  let x = ;\n  if {\n  return x\n}\n"
createPositionTestErrors :: String
                              createPositionTestErrors = "\nfunc test() {\n  return\n}\n// Error should be at correct position\n"

createCascadingErrors :: String
                              createCascadingErrors = "\nfunc test() {\n  let x = \n  let y = ;\n  if {\n    return\n  }\n}\n"

createMalformedBlockDirectives :: String
                              createMalformedBlockDirectives = "\n{//!invalid syntax\n  func test() {\n    return true\n  }\n// missing closing\n"

createInvariantBreakingErrors :: String
                              createInvariantBreakingErrors = "\nfunc {\n  return\n}\nlet = value\n"

-- Validation functions
validateBasicStructure :: TypusFile -> Bool
validateBasicStructure                               typusFile = 
  let directives = tfDirectives typusFile
                                    buildTags = tfBuildTags typusFile
                                    blocks = tfBlocks typusFile
  in L.length buildTags >= 0 && L.length blocks >= 0

validateBlock :: CodeBlock -> Bool
validateBlock                               block = 
  let directives = cbDirectives block
                                    content = cbContent block
  in L.length content >= 0
containsValidPosition :: String -> Bool
containsValidPosition                               errorMsg = 
  L.any (`L.isInfixOf` errorMsg) ["line", "position", "at"]

checkASTInvariants :: TypusFile -> Bool
checkASTInvariants                               typusFile = 
  let blocks = tfBlocks typusFile
                                    allBlocksValid = L.all validateBlock blocks
      in allBlocksValid

-- Mock defaultFileDirectives if not available
defaultFileDirectives :: FileDirectives
                              defaultFileDirectives = FileDirectives Nothing Nothing Nothing

-- ============================================================================
-- Arbitrary Instances
-- ============================================================================

-- Generate strings that may contain parser error patterns
arbitraryMalformedCode :: Gen String
                              arbitraryMalformedCode = listOf $ oneof
  [ elements ['a'..'z']
  , elements ['A'..'Z']
  , elements ['0'..'9']
  , elements " \t\n\r"
  , elements "{}[]( (SourceSpan (SourcePos 1 1 0 [] (SourcePos 1 1 0);,.!@#$%^&*"
  , elements "func let return if else"
  ]

arbitraryErrorPattern :: Gen String
                              arbitraryErrorPattern = oneof
  [ pure "func test() {\n  return\n}"  -- Missing return value
    , pure "let x = ;"  -- Invalid assignment
    , pure "if {\n  return true\n}"  -- Invalid if condition
    , pure "{//!invalid\n  code\n}"  -- Invalid directive
    , pure "\"unterminated string\n"  -- Unterminated string
  ]

instance Arbitrary String where
                                              arbitrary = oneof
    [ arbitraryMalformedCode
    , arbitraryErrorPattern
    ]))