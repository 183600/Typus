{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Test.Unit.IntegrationEnhancedQuickCheckSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Utils
  ( trim
  , splitBy
  , removeComments
  , normalizeIndentation
  , breakOn
  )
import SourceLocation
  ( SourcePos(..)
  , SourceSpan(..)
  , Located(..)
  , startPos
  , posAfter
  , spanBetween
  , locatedAt
  , advancePosBy
  , isValidSpan
  )
import Parser
  ( FileDirectives(..)
  , BlockDirectives(..)
  , CodeBlock(..)
  , TypusFile(..)
  , parseTypus
  )
import Data.Char (isSpace)
import Data.List (isPrefixOf, isInfixOf)
import qualified Data.Text as T

-- ============================================================================
-- Integration QuickCheck Tests
-- ============================================================================

-- | Test Utils and SourceLocation integration
prop_utils_sourcelocation_position_tracking :: String -> Bool
prop_utils_sourcelocation_position_tracking content = 
  let lines' = lines content
      positions = scanl (\pos line -> advancePosBy line pos) startPos lines'
      lineCount = length lines'
      positionCount = length positions
  in lineCount + 1 == positionCount  -- +1 for initial startPos

prop_utils_sourcelocation_span_creation :: String -> String -> Bool
prop_utils_sourcelocation_span_creation before after = 
  let startPos' = advancePosBy before startPos
      endPos = advancePosBy after startPos'
      span = spanBetween startPos' endPos
  in spanStart span == startPos' && spanEnd span == endPos

prop_utils_sourcelocation_located_content :: String -> Int -> Bool
prop_utils_sourcelocation_located_content content value = 
  let pos = advancePosBy content startPos
      located = locatedAt pos value
  in locValue located == value && locPos located == pos

-- | Test Utils and Parser integration
prop_utils_parser_comment_handling :: String -> String -> Property
prop_utils_parser_comment_handling content comments = 
  let contentWithComments = content ++ "\n// " ++ comments ++ "\n"
      withoutComments = removeComments contentWithComments
      result = parseTypus withoutComments
  in case result of
       Left _ -> property True
       Right typusFile ->
         let blocks = tfBlocks typusFile
         in property $ not (null blocks) ==> 
            let firstBlock = head blocks
                blockContent = cbContent firstBlock
            in content `isInfixOf` blockContent

prop_utils_parser_whitespace_handling :: String -> Property
prop_utils_parser_whitespace_handling content = 
  let contentWithExtraWhitespace = "  \n  " ++ content ++ "  \n  "
      normalized = normalizeIndentation contentWithExtraWhitespace
      result = parseTypus normalized
  in case result of
       Left _ -> property True
       Right typusFile ->
         let blocks = tfBlocks typusFile
         in property $ not (null blocks) ==> 
            let firstBlock = head blocks
                blockContent = cbContent firstBlock
            in content `isInfixOf` blockContent

prop_utils_parser_directive_parsing :: String -> Bool -> Property
prop_utils_parser_directive_parsing content flag = 
  let directive = "// ownership: " ++ show flag ++ "\n"
      fullContent = directive ++ content
      result = parseTypus fullContent
  in case result of
       Left _ -> property True
       Right typusFile ->
         let directives = tfDirectives typusFile
         in case fdOwnership directives of
              Nothing -> property False
              Just locatedValue -> property $ locValue locatedValue == flag

-- | Test SourceLocation and Parser integration
prop_sourcelocation_parser_span_tracking :: String -> Property
prop_sourcelocation_parser_span_tracking content = 
  let result = parseTypus content
  in case result of
       Left _ -> property True
       Right typusFile ->
         let blocks = tfBlocks typusFile
         in property $ not (null blocks) ==> 
            let firstBlock = head blocks
                span = cbSpan firstBlock
            in isValidSpan span
prop_sourcelocation_parser_position_extraction :: String -> Bool
prop_sourcelocation_parser_position_extraction content = 
  let result = parseTypus content
      blocks = tfBlocks result
  in not (null blocks) ==>
     let firstBlock = head blocks
         span = cbSpan firstBlock
         start = spanStart span
         end = spanEnd span
     in posLine start >= 1 && posColumn start >= 1 && 
        posLine end >= posLine start

-- | Test three-way integration
prop_utils_sourcelocation_parser_complex_content :: String -> String -> String -> Property
prop_utils_sourcelocation_parser_complex_content directives content comments = 
  let fullDirectives = "// " ++ directives ++ ": true\n"
      fullComments = "\n// " ++ comments
      fullContent = fullDirectives ++ content ++ fullComments
      withoutComments = removeComments fullContent
      normalized = normalizeIndentation withoutComments
      result = parseTypus normalized
  in case result of
       Left _ -> property True
       Right typusFile ->
         let blocks = tfBlocks typusFile
         in property $ not (null blocks) ==> 
            let firstBlock = head blocks
                blockContent = cbContent firstBlock
                span = cbSpan firstBlock
            in content `isInfixOf` blockContent && isValidSpan span

-- | Test error handling integration
prop_integration_error_recovery :: String -> String -> Property
prop_integration_error_recovery malformedContent goodContent = 
  let mixedContent = malformedContent ++ "\n" ++ goodContent
      result = parseTypus mixedContent
  in case result of
       Left _ -> property True
       Right typusFile -> 
         let blocks = tfBlocks typusFile
         in property $ not (null blocks)  -- Should still parse good content even with some errors

prop_integration_malformed_directive_recovery :: String -> Property
prop_integration_malformed_directive_recovery content = 
  let malformedDirective = "// ownership invalid\n"
      fullContent = malformedDirective ++ content
      result = parseTypus fullContent
  in case result of
       Left _ -> property True
       Right typusFile ->
         let blocks = tfBlocks typusFile
         in property $ not (null blocks) ==> 
            let firstBlock = head blocks
                blockContent = cbContent firstBlock
            in content `isInfixOf` blockContent

-- | Test content preservation across processing pipeline
prop_integration_content_preservation :: String -> Property
prop_integration_content_preservation content = 
  let withComments = "// test comment\n" ++ content ++ "\n// another comment"
      withoutComments = removeComments withComments
      normalized = normalizeIndentation withoutComments
      result = parseTypus normalized
  in case result of
       Left _ -> property True
       Right typusFile ->
         let blocks = tfBlocks typusFile
             extractedContent = concatMap cbContent blocks
         in property $ content `isInfixOf` extractedContent

-- | Test directive and content interaction
prop_integration_directive_content_interaction :: String -> String -> Bool -> Property
prop_integration_directive_content_interaction directive content flag = 
  let directiveLine = "// " ++ directive ++ ": " ++ show flag ++ "\n"
      fullContent = directiveLine ++ content
      result = parseTypus fullContent
  in case result of
       Left _ -> property True
       Right typusFile ->
         let directives = tfDirectives typusFile
             blocks = tfBlocks typusFile
         in property $ not (null blocks) ==> 
            let firstBlock = head blocks
                blockContent = cbContent firstBlock
            in content `isInfixOf` blockContent

-- | Test multi-block processing
prop_integration_multi_block_handling :: [String] -> Property
prop_integration_multi_block_handling contentBlocks = 
  all (not . null) contentBlocks ==>
    let content = unlines contentBlocks
        result = parseTypus content
    in case result of
         Left _ -> property True
         Right typusFile ->
           let blocks = tfBlocks typusFile
               blockContents = map cbContent blocks
           in property $ length blockContents == length contentBlocks

-- | Test position tracking with directives
prop_integration_directive_position_tracking :: String -> Bool -> Property
prop_integration_directive_position_tracking directive flag = 
  let directiveLine = "// " ++ directive ++ ": " ++ show flag ++ "\n"
      content = "some content\n"
      fullContent = directiveLine ++ content
      result = parseTypus fullContent
  in case result of
       Left _ -> property True
       Right typusFile ->
         let blocks = tfBlocks typusFile
         in property $ not (null blocks) ==>
            let firstBlock = head blocks
                span = cbSpan firstBlock
                start = spanStart span
            in posLine start >= 2  -- Should be after directive line

-- | Test comment removal with position tracking
prop_integration_comment_position_tracking :: String -> String -> Property
prop_integration_comment_position_tracking content comment = 
  let commentLine = "// " ++ comment ++ "\n"
      fullContent = content ++ "\n" ++ commentLine
      withoutComments = removeComments fullContent
      result = parseTypus withoutComments
  in case result of
       Left _ -> property True
       Right typusFile ->
         let blocks = tfBlocks typusFile
         in property $ not (null blocks) ==>
            let firstBlock = head blocks
                blockContent = cbContent firstBlock
     in content `isInfixOf` blockContent

-- | Test whitespace normalization with position tracking
prop_integration_whitespace_position_tracking :: String -> Property
prop_integration_whitespace_position_tracking content = 
  let indentedContent = "  " ++ content
      normalized = normalizeIndentation indentedContent
      result = parseTypus normalized
  in case result of
       Left _ -> property True
       Right typusFile ->
         let blocks = tfBlocks typusFile
         in property $ not (null blocks) ==>
            let firstBlock = head blocks
                span = cbSpan firstBlock
                start = spanStart span
            in posColumn start <= posColumn startPos + 4  -- Should be reasonably positioned

-- | Test complex parsing scenarios
prop_integration_complex_scenario :: String -> String -> String -> Bool -> Bool -> Property
prop_integration_complex_scenario directive1 directive2 content flag1 flag2 = 
  let directiveLine1 = "// " ++ directive1 ++ ": " ++ show flag1 ++ "\n"
      directiveLine2 = "// " ++ directive2 ++ ": " ++ show flag2 ++ "\n"
      commentLine = "// comment\n"
      fullContent = directiveLine1 ++ directiveLine2 ++ content ++ "\n" ++ commentLine
      withoutComments = removeComments fullContent
      result = parseTypus withoutComments
  in case result of
       Left _ -> property True
       Right typusFile ->
         let directives = tfDirectives typusFile
             blocks = tfBlocks typusFile
         in property $ not (null blocks) ==> 
            let firstBlock = head blocks
                blockContent = cbContent firstBlock
            in content `isInfixOf` blockContent

-- | Test round-trip processing
prop_integration_round_trip :: String -> Property
prop_integration_round_trip content = 
  let result1 = parseTypus content
  in case result1 of
       Left _ -> property True
       Right typusFile1 ->
         let blocks1 = tfBlocks typusFile1
             reconstructed = concatMap cbContent blocks1
             result2 = parseTypus reconstructed
         in case result2 of
              Left _ -> property True
              Right typusFile2 ->
                let blocks2 = tfBlocks typusFile2
                in property $ length blocks1 == length blocks2

-- | Test error location consistency
prop_integration_error_location_consistency :: String -> Property
prop_integration_error_location_consistency content = 
  let result = parseTypus content
  in case result of
       Left _ -> property True
       Right typusFile ->
         let syntaxErrors = tfSyntaxErrors typusFile
         in property $ null syntaxErrors || 
            all (\err -> posLine (spanStart err) >= 1) syntaxErrors

-- | Tasty test suite
testSuite :: TestTree
testSuite = testGroup "Integration Enhanced QuickCheck Properties"
  [ -- Utils and SourceLocation integration
    testProperty "utils and sourcelocation position tracking" prop_utils_sourcelocation_position_tracking,
    testProperty "utils and sourcelocation span creation" prop_utils_sourcelocation_span_creation,
    testProperty "utils and sourcelocation located content" prop_utils_sourcelocation_located_content,
    
    -- Utils and Parser integration
    testProperty "utils and parser comment handling" prop_utils_parser_comment_handling,
    testProperty "utils and parser whitespace handling" prop_utils_parser_whitespace_handling,
    testProperty "utils and parser directive parsing" prop_utils_parser_directive_parsing,
    
    -- SourceLocation and Parser integration
    testProperty "sourcelocation and parser span tracking" prop_sourcelocation_parser_span_tracking,
    testProperty "sourcelocation and parser position extraction" prop_sourcelocation_parser_position_extraction,
    
    -- Three-way integration
    testProperty "utils sourcelocation parser complex content" prop_utils_sourcelocation_parser_complex_content,
    
    -- Error handling integration
    testProperty "integration error recovery" prop_integration_error_recovery,
    testProperty "integration malformed directive recovery" prop_integration_malformed_directive_recovery,
    
    -- Content preservation
    testProperty "integration content preservation" prop_integration_content_preservation,
    
    -- Directive and content interaction
    testProperty "integration directive content interaction" prop_integration_directive_content_interaction,
    
    -- Multi-block processing
    testProperty "integration multi block handling" prop_integration_multi_block_handling,
    
    -- Position tracking
    testProperty "integration directive position tracking" prop_integration_directive_position_tracking,
    testProperty "integration comment position tracking" prop_integration_comment_position_tracking,
    testProperty "integration whitespace position tracking" prop_integration_whitespace_position_tracking,
    
    -- Complex scenarios
    testProperty "integration complex scenario" prop_integration_complex_scenario,
    
    -- Round-trip processing
    testProperty "integration round trip" prop_integration_round_trip,
    
    -- Error location consistency
    testProperty "integration error location consistency" prop_integration_error_location_consistency
  ]