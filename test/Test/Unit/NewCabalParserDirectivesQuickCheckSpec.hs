{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.NewCabalParserDirectivesQuickCheckSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Parser
import SourceLocation (SourceSpan(..), SourcePos(..))
import qualified Data.List as L
import Data.List (isInfixOf, isPrefixOf)
import Data.Maybe (isJust, isNothing)
import qualified Data.Text as T

-- | Test parser directive properties
testParserDirectiveProperties :: TestTree
testParserDirectiveProperties = testGroup "Parser Directive Properties"
  [ testProperty "empty file parses to default directives" propEmptyFileDefaults
  , testProperty "parseTypus preserves content order" propParsePreservesOrder
  , testProperty "directives are parsed correctly" propDirectivesParsed
  , testProperty "build tags are extracted" propBuildTagsExtracted
  , testProperty "code blocks maintain content" propCodeBlocksMaintainContent
  , testProperty "parsing is deterministic" propParsingDeterministic
  ]

-- | Empty file should parse to default directives
propEmptyFileDefaults :: Bool
propEmptyFileDefaults =
  let result = parseTypus ""
      expectedDirectives = defaultFileDirectives
  in tfDirectives result == expectedDirectives &&
     L.null (tfBuildTags result) &&
     L.null (tfBlocks result)

-- | ParseTypus should preserve the order of content
propParsePreservesOrder :: String -> String -> String -> Property
propParsePreservesOrder content1 content2 content3 =
  let combined = content1 ++ "\n" ++ content2 ++ "\n" ++ content3
      result = parseTypus combined
      blocks = tfBlocks result
      blockContents = map cbContent blocks
  in not (null blocks) ==> 
     let reconstructed = unlines blockContents
     in content1 `L.isInfixOf` reconstructed &&
        content2 `L.isInfixOf` reconstructed &&
        content3 `L.isInfixOf` reconstructed

-- | Directives should be parsed correctly when present
propDirectivesParsed :: Bool -> Bool -> Bool -> Property
propDirectivesParsed ownership dependent constraints =
  let directiveStr = "//! ownership=" ++ show ownership ++ 
                     ", dependent-types=" ++ show dependent ++ 
                     ", constraints=" ++ show constraints
      content = directiveStr ++ "\n// Some content"
      result = parseTypus content
      fileDirectives = tfDirectives result
  in case (fdOwnership fileDirectives, fdDependentTypes fileDirectives, fdConstraints fileDirectives) of
       (Just (Located ownership' _ _), Just (Located dependent' _ _), Just (Located constraints' _ _)) ->
         ownership' == ownership && dependent' == dependent && constraints' == constraints
       _ -> False

-- | Build tags should be extracted from comments
propBuildTagsExtracted :: String -> Property
propBuildTagsExtracted tag =
  not (null tag) && not (L.any isSpace tag) ==>
  let content = "/" "+build " ++ tag ++ "\n// Some content"
      result = parseTypus content
      buildTags = tfBuildTags result
  in not (null buildTags) && tag `elem` map locValue buildTags

-- | Code blocks should maintain their content
propCodeBlocksMaintainContent :: String -> Property
propCodeBlocksMaintainContent content =
  not (null content) ==> 
  let testContent = "// Some directive\n" ++ content ++ "\n// More content"
      result = parseTypus testContent
      blocks = tfBlocks result
  in not (null blocks) ==> 
     let blockContent = L.head (map cbContent blocks)
     in content `L.isInfixOf` blockContent

-- | Parsing should be deterministic (same input gives same output)
propParsingDeterministic :: String -> Bool
propParsingDeterministic content =
  let result1 = parseTypus content
      result2 = parseTypus content
  in result1 == result2

-- | Test directive parsing edge cases
testDirectiveParsingEdgeCases :: TestTree
testDirectiveParsingEdgeCases = testGroup "Directive Parsing Edge Cases"
  [ testCase "parse file with ownership directive" $
      let content = "//! ownership=true\n// Some code"
          result = parseTypus content
          directives = tfDirectives result
      in case fdOwnership directives of
           Just (Located True _ _) -> pure ()
           _ -> fail "Ownership directive not parsed correctly"
           
  , testCase "parse file with dependent-types directive" $
      let content = "//! dependent-types=false\n// Some code"
          result = parseTypus content
          directives = tfDirectives result
      in case fdDependentTypes directives of
           Just (Located False _ _) -> pure ()
           _ -> fail "Dependent types directive not parsed correctly"
           
  , testCase "parse file with multiple directives" $
      let content = "//! ownership=true, dependent-types=true, constraints=false\n// Some code"
          result = parseTypus content
          directives = tfDirectives result
      in case (fdOwnership directives, fdDependentTypes directives, fdConstraints directives) of
           (Just (Located True _ _), Just (Located True _ _), Just (Located False _ _)) -> pure ()
           _ -> fail "Multiple directives not parsed correctly"
           
  , testCase "parse file with build tags" $
      let content = "/" "+build linux,amd64\n" "+build !windows\n// Some code"
          result = parseTypus content
          buildTags = tfBuildTags result
      in L.length buildTags == 2 && 
         "linux,amd64" `elem` map locValue buildTags &&
         "!windows" `elem` map locValue buildTags
         
  , testCase "parse file with code blocks" $
      let content = "//! ownership=true\nfunc main() {\n    println(\"Hello\")\n}\n//! ownership=false\nfunc helper() {\n    return 42\n}"
          result = parseTypus content
          blocks = tfBlocks result
      in L.length blocks == 2 &&
         "func main()" `L.isInfixOf` cbContent (L.head blocks) &&
         "func helper()" `L.isInfixOf` cbContent (blocks !! 1)
  ]

-- | Test block directive parsing
testBlockDirectiveParsing :: TestTree
testBlockDirectiveParsing = testGroup "Block Directive Parsing"
  [ testCase "parse block with ownership directive" $
      let content = "//! ownership=true\n// @ownership=false\n// Some code"
          result = parseTypus content
          blocks = tfBlocks result
      in case blocks of
           (block:_) -> case bdOwnership (cbDirectives block) of
                        Just (Located False _ _) -> pure ()
                        _ -> fail "Block ownership directive not parsed correctly"
           [] -> fail "No blocks found"
           
  , testCase "parse block with dependent-types directive" $
      let content = "//! dependent-types=true\n// @dependent-types=false\n// Some code"
          result = parseTypus content
          blocks = tfBlocks result
      in case blocks of
           (block:_) -> case bdDependentTypes (cbDirectives block) of
                        Just (Located False _ _) -> pure ()
                        _ -> fail "Block dependent-types directive not parsed correctly"
           [] -> fail "No blocks found"
           
  , testCase "parse block with multiple directives" $
      let content = "//! ownership=true\n// @ownership=false, dependent-types=true, constraints=false\n// Some code"
          result = parseTypus content
          blocks = tfBlocks result
      in case blocks of
           (block:_) -> 
             let directives = cbDirectives block
             in case (bdOwnership directives, bdDependentTypes directives, bdConstraints directives) of
                  (Just (Located False _ _), Just (Located True _ _), Just (Located False _ _)) -> pure ()
                  _ -> fail "Multiple block directives not parsed correctly"
           [] -> fail "No blocks found"
  ]

-- | Test syntax error handling
testSyntaxErrorHandling :: TestTree
testSyntaxErrorHandling = testGroup "Syntax Error Handling"
  [ testCase "parse file with syntax errors" $
      let content = "//! ownership=true\nfunc invalid syntax here {\n    missing closing brace"
          result = parseTypus content
          syntaxErrors = tfSyntaxErrors result
      in not (null syntaxErrors) ==> pure ()
      
  , testCase "parse valid file has no syntax errors" $
      let content = "//! ownership=true\nfunc main() {\n    println(\"Hello\")\n}"
          result = parseTypus content
          syntaxErrors = tfSyntaxErrors result
      in null syntaxErrors ==> pure ()
  ]

-- | Test parser robustness
testParserRobustness :: TestTree
testParserRobustness = testGroup "Parser Robustness"
  [ testCase "parse empty file" $
      let result = parseTypus ""
      in L.null (tfBlocks result) && tfDirectives result == defaultFileDirectives
      
  , testCase "parse file with only comments" $
      let content = "// This is a comment\n// Another comment"
          result = parseTypus content
      in not (L.null (tfBlocks result)) ==> pure ()
      
  , testCase "parse file with only directives" $
      let content = "//! ownership=true\n//! dependent-types=false"
          result = parseTypus content
          directives = tfDirectives result
      in case (fdOwnership directives, fdDependentTypes directives) of
           (Just (Located True _ _), Just (Located False _ _)) -> pure ()
           _ -> fail "Directives not parsed correctly"
           
  , testCase "parse file with unicode content" $
      let content = "//! ownership=true\n// Unicode test: 你好世界 🌍"
          result = parseTypus content
      in not (L.null (tfBlocks result)) ==> pure ()
  ]

-- | All parser directive tests
testParserDirectivesQuickCheck :: TestTree
testParserDirectivesQuickCheck = testGroup "New Cabal Parser Directives QuickCheck Tests"
  [ testParserDirectiveProperties
  , testDirectiveParsingEdgeCases
  , testBlockDirectiveParsing
  , testSyntaxErrorHandling
  , testParserRobustness
  ]