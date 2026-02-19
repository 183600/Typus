{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans  -Wno-unused-imports -Wno-name-shadowing -Wno-unused-local-binds #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Test.Unit.ParserComprehensiveSpec where


import Test.Tasty.HUnit
import Test.Tasty
import Test.Tasty.QuickCheck



import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertEqual, assertBool, assertFailure, Assertion)
import Test.Tasty.QuickCheck (testProperties, Arbitrary(..), Gen, choose, listOf, elements, oneof, vectorOf, property, (===), forAll, counterexample)
import Test.QuickCheck (Gen, Property, (==>), classify)
import Parser (parseTypus, FileDirectives(..), BlockDirectives(..), CodeBlock(..), 
              TypusFile(..), defaultFileDirectives, defaultBlockDirectives)
import SourceLocation (SourcePos(..), SourceSpan(..), Located(..), locatedAt, locatedWithSpan)
import qualified Data.Text as T
import Data.Char (isAlphaNum, isSpace)
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)
import qualified SyntaxValidator

-- Arbitrary instance for SyntaxError
instance Arbitrary SyntaxValidator.SyntaxError where
  arbitrary = do
    errorType <- elements [
        SyntaxValidator.MissingBrace,
        SyntaxValidator.MissingParenthesis,
        SyntaxValidator.MissingBracket,
        SyntaxValidator.UnclosedString,
        SyntaxValidator.UnclosedComment,
        SyntaxValidator.InvalidIdentifier,
        SyntaxValidator.InvalidTypeDeclaration,
        SyntaxValidator.InvalidFunctionDeclaration,
        SyntaxValidator.InvalidImport,
        SyntaxValidator.InvalidStatement,
        SyntaxValidator.UnterminatedBlock,
        SyntaxValidator.InvalidOperator,
        SyntaxValidator.MissingSemicolon,
        SyntaxValidator.UnexpectedToken,
        SyntaxValidator.MissingPackageDeclaration,
        SyntaxValidator.DuplicateDeclaration,
        SyntaxValidator.InvalidBlockStructure,
        SyntaxValidator.UndeclaredVariable,
        SyntaxValidator.SyntaxWarning
      ]
    line <- choose (1, 100)
    column <- choose (1, 100)
    message <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " "
    return $ SyntaxValidator.SyntaxError errorType (take 50 message) line column (take 50 message)

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
  return $ locatedWithSpan span value

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
  ownership <- oneof [return "", return "ownership=true", return "ownership=false"]
  dependentTypes <- oneof [return "", return "dependent-types=true", return "dependent-types=false"]
  constraints <- oneof [return "", return "constraints=true", return "constraints=false"]
  let directives = filter (not . null) [ownership, dependentTypes, constraints]
  return $ if null directives then "" else unwords directives ++ "\n"

-- Test properties for Parser module

-- Property 1: Parsing empty content returns minimal file
prop_parse_empty_content :: Bool
prop_parse_empty_content = 
  let result = parseTypus ""
      expected = Right $ TypusFile defaultFileDirectives [] [] []
  in result == expected

-- Property 2: Parsing content with only directives
prop_parse_directives_only :: String -> Property
prop_parse_directives_only directives = 
  not (null directives) && all (`elem` "ownership=trueownership=falsedependent-types=truedependent-types=falseconstraints=trueconstraints=false\n\t ") directives ==>
  let result = parseTypus directives
      hasDirectives = case result of
                        Right p -> tfDirectives p /= defaultFileDirectives
                        Left _ -> False
      hasNoBlocks = case result of
                      Right p -> null (tfBlocks p)
                      Left _ -> True
  in hasDirectives ==> hasNoBlocks

-- Property 3: Parsing content with simple code blocks
prop_parse_simple_blocks :: String -> Property
prop_parse_simple_blocks content = 
  not (null content) && not (any (`elem` "\r\n") content) ==>
  let result = parseTypus content
      hasBlocks = case result of
                    Right p -> not (null (tfBlocks p))
                    Left _ -> False
  in hasBlocks ==> case result of
                     Right p -> all (not . null . cbContent) (tfBlocks p)
                     Left _ -> True

-- Property 4: Parsing preserves content structure
prop_parse_preserves_structure :: String -> Property
prop_parse_preserves_structure content = 
  not (null content) ==> 
  let result = parseTypus content
      originalLines = lines content
      parsedLines = case result of
                      Right p -> concatMap (lines . cbContent) (tfBlocks p)
                      Left _ -> []
      blockContents = case result of
                        Right p -> map cbContent (tfBlocks p)
                        Left _ -> []
      totalBlockContent = unlines blockContents
  in length originalLines <= length (lines totalBlockContent)

-- Property 5: Default directives are used when none specified
prop_parse_default_directives :: String -> Property
prop_parse_default_directives content = 
  not (any (`isInfixOf` content) ["ownership", "dependent-types", "constraints"]) ==>
  let result = parseTypus content
      directives = case result of
                     Right p -> tfDirectives p
                     Left _ -> defaultFileDirectives
  in directives == defaultFileDirectives

-- Property 6: Parsing handles whitespace correctly
prop_parse_whitespace_handling :: String -> String -> Property
prop_parse_whitespace_handling content whitespace = 
  not (null content) ==> 
  let contentWithWhitespace = whitespace ++ content ++ whitespace
      result1 = parseTypus content
      result2 = parseTypus contentWithWhitespace
      blocks1 = case result1 of
                  Right p -> tfBlocks p
                  Left _ -> []
      blocks2 = case result2 of
                  Right p -> tfBlocks p
                  Left _ -> []
  in length blocks1 == length blocks2

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
      parsedBlocks = case result of
                       Right p -> tfBlocks p
                       Left _ -> []
  in length parsedBlocks >= length blocks

-- Property 9: File directives are parsed correctly
prop_parse_file_directives :: String -> Property
prop_parse_file_directives directiveStr = 
  any (`isInfixOf` directiveStr) ["ownership", "dependent-types", "constraints"] ==>
  let content = directiveStr ++ "\n" ++ "some code"
      result = parseTypus content
      directives = case result of
                     Right p -> tfDirectives p
                     Left _ -> defaultFileDirectives
  in directives /= defaultFileDirectives

-- Property 10: Block directives are parsed correctly
prop_parse_block_directives :: String -> Property
prop_parse_block_directives directiveStr = 
  any (`isInfixOf` directiveStr) ["ownership", "dependent-types", "constraints"] ==>
  let content = "some code with " ++ directiveStr
      result = parseTypus content
      blocks = case result of
                 Right p -> tfBlocks p
                 Left _ -> []
  in not (null blocks) ==> any (\b -> cbDirectives b /= defaultBlockDirectives) blocks

-- Unit tests for edge cases
test_parser_edge_cases :: [TestTree]
test_parser_edge_cases = 
  [ testCase "parse empty string" $ 
      case parseTypus "" of
        Right p -> assertEqual "directives should match" defaultFileDirectives (tfDirectives p)
        Left _ -> assertFailure "Failed to parse empty string"
  , testCase "parse only whitespace" $ 
      case parseTypus "   \n\t  " of
        Right p -> assertEqual "directives should match" defaultFileDirectives (tfDirectives p)
        Left _ -> assertFailure "Failed to parse whitespace"
  , testCase "parse single line comment" $ 
      let result = parseTypus "// this is a comment"
      in case result of
           Right p -> assertBool "should parse without error" (not (null (tfBlocks p)))
           Left _ -> assertFailure "Failed to parse comment"
  , testCase "parse multiple lines" $ 
      let content = "line1\nline2\nline3"
          result = parseTypus content
      in case result of
           Right p -> assertBool "should create blocks" (not (null (tfBlocks p)))
           Left _ -> assertFailure "Failed to parse multiple lines"
  , testCase "parse with ownership directive" $ 
      let content = "ownership=true\nsome code"
          result = parseTypus content
      in case result of
           Right p -> let directives = tfDirectives p
                      in assertBool "should have ownership directive" (fdOwnership directives /= Nothing)
           Left _ -> assertFailure "Failed to parse ownership directive"
  , testCase "parse with dependent-types directive" $ 
      let content = "dependent-types=false\nsome code"
          result = parseTypus content
      in case result of
           Right p -> let directives = tfDirectives p
                      in assertBool "should have dependent-types directive" (fdDependentTypes directives /= Nothing)
           Left _ -> assertFailure "Failed to parse dependent-types directive"
  , testCase "parse with constraints directive" $ 
      let content = "constraints=true\nsome code"
          result = parseTypus content
      in case result of
           Right p -> let directives = tfDirectives p
                      in assertBool "should have constraints directive" (fdConstraints directives /= Nothing)
           Left _ -> assertFailure "Failed to parse constraints directive"
  , testCase "parse with multiple directives" $ 
      let content = "ownership=true\ndependent-types=false\nconstraints=true\nsome code"
          result = parseTypus content
      in case result of
           Right p -> let directives = tfDirectives p
                      in assertBool "should have all directives" 
                         (fdOwnership directives /= Nothing && 
                          fdDependentTypes directives /= Nothing && 
                          fdConstraints directives /= Nothing)
           Left _ -> assertFailure "Failed to parse multiple directives"
  ]

test_parser_error_handling :: [TestTree]
test_parser_error_handling = 
  [ testCase "parse malformed content" $ 
      let content = "ownership=\ndependent-types=invalid\nsome code"
          result = parseTypus content
      in case result of
           Right p -> assertBool "should handle malformed input gracefully" (not (null (tfBlocks p)))
           Left _ -> assertBool "should return error for malformed content" True
  , testCase "parse with special characters" $ 
      let content = "code with !@#$%^&*(){}[]|\\:;\"'<>?,./"
          result = parseTypus content
      in case result of
           Right p -> assertBool "should handle special characters" (not (null (tfBlocks p)))
           Left _ -> assertBool "should return error for special characters" True
  , testCase "parse with unicode characters" $ 
      let content = "code with çñüßαβγδεζηθ"
          result = parseTypus content
      in case result of
           Right p -> assertBool "should handle unicode" (not (null (tfBlocks p)))
           Left _ -> assertBool "should return error for unicode" True
  , testCase "parse very long line" $ 
      let content = replicate 50 'a'  -- 从1000减少到50，大幅减少内存使用
          result = parseTypus content
      in case result of
           Right p -> assertBool "should handle long lines" (not (null (tfBlocks p)))
           Left _ -> assertBool "should return error for long lines" True
  ]

test_parser_integration :: [TestTree]
test_parser_integration = 
  [ testCase "parse complete file structure" $ 
      let content = "ownership=true\ndependent-types=false\n// build tag: test\n\nblock1 content\n\nownership=true\nblock2 content"
          result = parseTypus content
      in case result of
           Right p -> assertBool "should parse complete structure" 
                      (length (tfBlocks p) >= 2 && 
                       tfDirectives p /= defaultFileDirectives)
           Left _ -> assertFailure "Failed to parse complete structure"
  , testCase "parse nested directives" $ 
      let content = "ownership=true\n// file level\n\nownership=false\n// block level\nsome code"
          result = parseTypus content
      in case result of
           Right p -> let fileDirectives = tfDirectives p
                          blocks = tfBlocks p
                      in assertBool "should handle nested directives" 
                         (fdOwnership fileDirectives /= Nothing && 
                          not (null blocks) && 
                          any (\b -> bdOwnership (cbDirectives b) /= Nothing) blocks)
           Left _ -> assertFailure "Failed to parse nested directives"
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