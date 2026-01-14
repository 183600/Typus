{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Test.Unit.ParserComprehensiveSpec where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertEqual, assertBool, Assertion)
import Test.Tasty.QuickCheck (testProperties, Arbitrary(..), Gen, choose, listOf, elements, oneof, vectorOf, property, (===), forAll, counterexample)
import Test.QuickCheck (Gen, Property, (==>), classify)
import Parser (parseTypus, FileDirectives(..), BlockDirectives(..), CodeBlock(..), 
              TypusFile(..), defaultFileDirectives, defaultBlockDirectives)
import SourceLocation (SourcePos(..), SourceSpan(..), Located(..), locatedAt)
import qualified Data.Text as T
import Data.Char (isAlphaNum, isSpace)
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)
import qualified SyntaxValidator

-- Helper generators for Parser tests
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

genLocated :: Gen a -> Gen (Located a)
genLocated gen = do
  value <- gen
  span <- genSourceSpan
  return $ locatedWithSpan value span

genLocatedBool :: Gen (Located Bool)
genLocatedBool = genLocated arbitrary

genLocatedString :: Gen (Located String)
genLocatedString = genLocated $ do
  len <- choose (1, 20)
  vectorOf len $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_-"

genFileDirectives :: Gen FileDirectives
genFileDirectives = FileDirectives <$> oneof [pure Nothing, Just <$> genLocatedBool]
                                   <*> oneof [pure Nothing, Just <$> genLocatedBool]
                                   <*> oneof [pure Nothing, Just <$> genLocatedBool]

genBlockDirectives :: Gen BlockDirectives
genBlockDirectives = BlockDirectives <$> oneof [pure Nothing, Just <$> genLocatedBool]
                                    <*> oneof [pure Nothing, Just <$> genLocatedBool]
                                    <*> oneof [pure Nothing, Just <$> genLocatedBool]

genCodeBlock :: Gen CodeBlock
genCodeBlock = do
  directives <- genBlockDirectives
  content <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " \t\n.,;:+-*/=<>()[]{}"
  span <- genSourceSpan
  return $ CodeBlock directives content span

genTypusFile :: Gen TypusFile
genTypusFile = do
  directives <- genFileDirectives
  buildTags <- listOf genLocatedString
  blocks <- listOf genCodeBlock
  syntaxErrors <- arbitrary -- Assuming SyntaxError has Arbitrary instance
  return $ TypusFile directives buildTags blocks syntaxErrors

genSimpleContent :: Gen String
genSimpleContent = do
  len <- choose (1, 100)
  vectorOf len $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " \n\t.,;:+-*/=<>()[]{}"

genDirectiveContent :: Gen String
genDirectiveContent = do
  ownership <- oneof ["", "ownership=true", "ownership=false"]
  dependentTypes <- oneof ["", "dependent-types=true", "dependent-types=false"]
  constraints <- oneof ["", "constraints=true", "constraints=false"]
  let directives = filter (not . null) [ownership, dependentTypes, constraints]
  return $ if null directives then "" else unwords directives ++ "\n"

-- Test properties for Parser module

-- Property 1: Parsing empty content returns minimal file
prop_parse_empty_content :: Bool
prop_parse_empty_content = 
  let result = parseTypus ""
      expected = TypusFile defaultFileDirectives [] [] []
  in result == expected

-- Property 2: Parsing content with only directives
prop_parse_directives_only :: String -> Property
prop_parse_directives_only directives = 
  not (null directives) && all (`elem` "ownership=trueownership=falsedependent-types=truedependent-types=falseconstraints=trueconstraints=false\n\t ") directives ==>
  let result = parseTypus directives
      hasDirectives = tfDirectives result /= defaultFileDirectives
      hasNoBlocks = null (tfBlocks result)
  in hasDirectives ==> hasNoBlocks

-- Property 3: Parsing content with simple code blocks
prop_parse_simple_blocks :: String -> Property
prop_parse_simple_blocks content = 
  not (null content) && not (any (`elem` "\r\n") content) ==>
  let result = parseTypus content
      hasBlocks = not (null (tfBlocks result))
  in hasBlocks ==> all (not . null . cbContent) (tfBlocks result)

-- Property 4: Parsing preserves content structure
prop_parse_preserves_structure :: String -> Property
prop_parse_preserves_structure content = 
  not (null content) ==> 
  let result = parseTypus content
      originalLines = lines content
      blockContents = map cbContent (tfBlocks result)
      totalBlockContent = unlines blockContents
  in length originalLines <= length (lines totalBlockContent)

-- Property 5: Default directives are used when none specified
prop_parse_default_directives :: String -> Property
prop_parse_default_directives content = 
  not (any (`isInfixOf` content) ["ownership", "dependent-types", "constraints"]) ==>
  let result = parseTypus content
  in tfDirectives result == defaultFileDirectives

-- Property 6: Parsing handles whitespace correctly
prop_parse_whitespace_handling :: String -> String -> Property
prop_parse_whitespace_handling content whitespace = 
  not (null content) ==> 
  let contentWithWhitespace = whitespace ++ content ++ whitespace
      result1 = parseTypus content
      result2 = parseTypus contentWithWhitespace
  in length (tfBlocks result1) == length (tfBlocks result2)

-- Property 7: Parsing is deterministic
prop_parse_deterministic :: String -> Bool
prop_parse_deterministic content = 
  let result1 = parseTypus content
      result2 = parseTypus content
  in result1 == result2

-- Property 8: Parsing handles multiple blocks
prop_parse_multiple_blocks :: [String] -> Property
prop_parse_multiple_blocks blocks = 
  not (null blocks) && all (not . null) blocks ==>
  let content = unlines blocks
      result = parseTypus content
  in length (tfBlocks result) >= length blocks

-- Property 9: File directives are parsed correctly
prop_parse_file_directives :: String -> Property
prop_parse_file_directives directiveStr = 
  any (`isInfixOf` directiveStr) ["ownership", "dependent-types", "constraints"] ==>
  let content = directiveStr ++ "\n" ++ "some code"
      result = parseTypus content
      directives = tfDirectives result
  in directives /= defaultFileDirectives

-- Property 10: Block directives are parsed correctly
prop_parse_block_directives :: String -> Property
prop_parse_block_directives directiveStr = 
  any (`isInfixOf` directiveStr) ["ownership", "dependent-types", "constraints"] ==>
  let content = "some code with " ++ directiveStr
      result = parseTypus content
      blocks = tfBlocks result
  in not (null blocks) ==> any (\b -> cbDirectives b /= defaultBlockDirectives) blocks

-- Unit tests for edge cases
test_parser_edge_cases :: [TestTree]
test_parser_edge_cases = 
  [ testCase "parse empty string" $ 
      assertEqual defaultFileDirectives (tfDirectives (parseTypus ""))
  , testCase "parse only whitespace" $ 
      assertEqual defaultFileDirectives (tfDirectives (parseTypus "   \n\t  "))
  , testCase "parse single line comment" $ 
      let result = parseTypus "// this is a comment"
      in assertBool "should parse without error" (not (null (tfBlocks result)))
  , testCase "parse multiple lines" $ 
      let content = "line1\nline2\nline3"
          result = parseTypus content
      in assertBool "should create blocks" (not (null (tfBlocks result)))
  , testCase "parse with ownership directive" $ 
      let content = "ownership=true\nsome code"
          result = parseTypus content
          directives = tfDirectives result
      in assertBool "should have ownership directive" (fdOwnership directives /= Nothing)
  , testCase "parse with dependent-types directive" $ 
      let content = "dependent-types=false\nsome code"
          result = parseTypus content
          directives = tfDirectives result
      in assertBool "should have dependent-types directive" (fdDependentTypes directives /= Nothing)
  , testCase "parse with constraints directive" $ 
      let content = "constraints=true\nsome code"
          result = parseTypus content
          directives = tfDirectives result
      in assertBool "should have constraints directive" (fdConstraints directives /= Nothing)
  , testCase "parse with multiple directives" $ 
      let content = "ownership=true\ndependent-types=false\nconstraints=true\nsome code"
          result = parseTypus content
          directives = tfDirectives result
      in assertBool "should have all directives" 
         (fdOwnership directives /= Nothing && 
          fdDependentTypes directives /= Nothing && 
          fdConstraints directives /= Nothing)
  ]

test_parser_error_handling :: [TestTree]
test_parser_error_handling = 
  [ testCase "parse malformed content" $ 
      let content = "ownership=\ndependent-types=invalid\nsome code"
          result = parseTypus content
      in assertBool "should handle malformed input gracefully" (not (null (tfBlocks result)))
  , testCase "parse with special characters" $ 
      let content = "code with !@#$%^&*(){}[]|\\:;\"'<>?,./"
          result = parseTypus content
      in assertBool "should handle special characters" (not (null (tfBlocks result)))
  , testCase "parse with unicode characters" $ 
      let content = "code with çñüßαβγδεζηθ"
          result = parseTypus content
      in assertBool "should handle unicode" (not (null (tfBlocks result)))
  , testCase "parse very long line" $ 
      let content = replicate 1000 'a'
          result = parseTypus content
      in assertBool "should handle long lines" (not (null (tfBlocks result)))
  ]

test_parser_integration :: [TestTree]
test_parser_integration = 
  [ testCase "parse complete file structure" $ 
      let content = "ownership=true\ndependent-types=false\n// build tag: test\n\nblock1 content\n\nownership=true\nblock2 content"
          result = parseTypus content
      in assertBool "should parse complete structure" 
         (length (tfBlocks result) >= 2 && 
          tfDirectives result /= defaultFileDirectives)
  , testCase "parse nested directives" $ 
      let content = "ownership=true\n// file level\n\nownership=false\n// block level\nsome code"
          result = parseTypus content
          fileDirectives = tfDirectives result
          blocks = tfBlocks result
      in assertBool "should handle nested directives" 
         (fdOwnership fileDirectives /= Nothing && 
          not (null blocks) && 
          any (\b -> bdOwnership (cbDirectives b) /= Nothing) blocks)
  ]

-- QuickCheck property tests
parserQuickCheckTests :: TestTree
parserQuickCheckTests = testGroup "QuickCheck Properties"
  [ testProperties "Basic Parsing"
      [ ("parse empty content", property prop_parse_empty_content)
      , ("parse directives only", property prop_parse_directives_only)
      , ("parse simple blocks", property prop_parse_simple_blocks)
      , ("parse preserves structure", property prop_parse_preserves_structure)
      ]
  , testProperties "Directive Handling"
      [ ("default directives", property prop_parse_default_directives)
      , ("file directives", property prop_parse_file_directives)
      , ("block directives", property prop_parse_block_directives)
      ]
  , testProperties "Robustness"
      [ ("whitespace handling", property prop_parse_whitespace_handling)
      , ("deterministic parsing", property prop_parse_deterministic)
      , ("multiple blocks", property prop_parse_multiple_blocks)
      ]
  ]

-- Unit tests
parserUnitTests :: TestTree
parserUnitTests = testGroup "Unit Tests"
  [ testGroup "Edge Cases" test_parser_edge_cases
  , testGroup "Error Handling" test_parser_error_handling
  , testGroup "Integration" test_parser_integration
  ]

-- Main test suite
parserComprehensiveTests :: TestTree
parserComprehensiveTests = testGroup "Parser Comprehensive Tests"
  [ parserUnitTests
  , parserQuickCheckTests
  ]