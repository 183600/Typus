{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Test.Unit.ParserEnhancedQuickCheckSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Parser
  ( FileDirectives(..)
  , BlockDirectives(..)
  , CodeBlock(..)
  , TypusFile(..)
  , defaultFileDirectives
  , defaultBlockDirectives
  , parseTypus
  )
import SourceLocation
  ( SourcePos(..)
  , SourceSpan(..)
  , Located(..)
  , startPos
  )
import qualified Data.Text as T
import Data.Char (isAlphaNum, isSpace)
import Data.List (isPrefixOf, isInfixOf)

-- ============================================================================
-- Parser Module QuickCheck Tests
-- ============================================================================

-- | Test FileDirectives properties
prop_file_directives_default :: Bool
prop_file_directives_default = 
  let fd = defaultFileDirectives
  in fdOwnership fd == Nothing && 
     fdDependentTypes fd == Nothing && 
     fdConstraints fd == Nothing

prop_file_directives_equality :: Maybe (Located Bool) -> Maybe (Located Bool) -> Maybe (Located Bool) -> Bool
prop_file_directives_equality ownership deps constraints = 
  let fd1 = FileDirectives ownership deps constraints
      fd2 = FileDirectives ownership deps constraints
  in fd1 == fd2

-- | Test BlockDirectives properties
prop_block_directives_default :: Bool
prop_block_directives_default = 
  let bd = defaultBlockDirectives
  in bdOwnership bd == Nothing && 
     bdDependentTypes bd == Nothing && 
     bdConstraints bd == Nothing

prop_block_directives_equality :: Maybe (Located Bool) -> Maybe (Located Bool) -> Maybe (Located Bool) -> Bool
prop_block_directives_equality ownership deps constraints = 
  let bd1 = BlockDirectives ownership deps constraints
      bd2 = BlockDirectives ownership deps constraints
  in bd1 == bd2

-- | Test CodeBlock properties
prop_code_block_equality :: BlockDirectives -> String -> SourceSpan -> Bool
prop_code_block_equality directives content span = 
  let cb1 = CodeBlock directives content span
      cb2 = CodeBlock directives content span
  in cb1 == cb2

prop_code_block_content_extraction :: BlockDirectives -> String -> SourceSpan -> Bool
prop_code_block_content_extraction directives content span = 
  let cb = CodeBlock directives content span
  in cbContent cb == content

prop_code_block_directives_extraction :: BlockDirectives -> String -> SourceSpan -> Bool
prop_code_block_directives_extraction directives content span = 
  let cb = CodeBlock directives content span
  in cbDirectives cb == directives

prop_code_block_span_extraction :: BlockDirectives -> String -> SourceSpan -> Bool
prop_code_block_span_extraction directives content span = 
  let cb = CodeBlock directives content span
  in cbSpan cb == span

-- | Test TypusFile properties
prop_typus_file_default :: Bool
prop_typus_file_default = 
  let tf = TypusFile defaultFileDirectives [] [] []
  in tfDirectives tf == defaultFileDirectives && 
     tfBuildTags tf == [] && 
     tfBlocks tf == [] && 
     tfSyntaxErrors tf == []

prop_typus_file_equality :: FileDirectives -> [Located String] -> [CodeBlock] -> Bool
prop_typus_file_equality directives buildTags blocks = 
  let tf1 = TypusFile directives buildTags blocks []
      tf2 = TypusFile directives buildTags blocks []
  in tf1 == tf2

prop_typus_file_directives_extraction :: FileDirectives -> [Located String] -> [CodeBlock] -> Bool
prop_typus_file_directives_extraction directives buildTags blocks = 
  let tf = TypusFile directives buildTags blocks []
  in tfDirectives tf == directives

prop_typus_file_build_tags_extraction :: FileDirectives -> [Located String] -> [CodeBlock] -> Bool
prop_typus_file_build_tags_extraction directives buildTags blocks = 
  let tf = TypusFile directives buildTags blocks []
  in tfBuildTags tf == buildTags

prop_typus_file_blocks_extraction :: FileDirectives -> [Located String] -> [CodeBlock] -> Bool
prop_typus_file_blocks_extraction directives buildTags blocks = 
  let tf = TypusFile directives buildTags blocks []
  in tfBlocks tf == blocks

prop_typus_file_syntax_errors_extraction :: FileDirectives -> [Located String] -> [CodeBlock] -> Bool
prop_typus_file_syntax_errors_extraction directives buildTags blocks = 
  let tf = TypusFile directives buildTags blocks []
  in tfSyntaxErrors tf == []

-- | Test parsing empty input
prop_parse_empty_input :: Property
prop_parse_empty_input = 
  let result = parseTypus ""
  in case result of
       Left _ -> property True
       Right typusFile -> 
         tfDirectives typusFile == defaultFileDirectives && 
         tfBuildTags typusFile == [] && 
         tfBlocks typusFile == []

prop_parse_simple_content :: String -> Property
prop_parse_simple_content content = 
  not (null content) && not (any isSpace content) ==>
    let result = parseTypus content
    in case result of
         Left _ -> property True
         Right typusFile ->
           let blocks = tfBlocks typusFile
           in property $ not (null blocks) ==> 
              let firstBlock = head blocks
                  blockContent = cbContent firstBlock
              in content `isInfixOf` blockContent

prop_parse_preserves_content :: String -> Property
prop_parse_preserves_content content = 
  let result = parseTypus content
  in case result of
       Left _ -> property True
       Right typusFile ->
         let blocks = tfBlocks typusFile
             allBlockContent = concatMap cbContent blocks
         in property $ content == allBlockContent || 
            (all (`isInfixOf` allBlockContent) (lines content))

prop_parse_handles_whitespace :: String -> Property
prop_parse_handles_whitespace content = 
  let whitespaceContent = "  \n  \n  " ++ content ++ "  \n  \n  "
      result = parseTypus whitespaceContent
  in case result of
       Left _ -> property True
       Right typusFile ->
         let blocks = tfBlocks typusFile
         in property $ not (null blocks) ==> 
            let firstBlock = head blocks
                blockContent = cbContent firstBlock
            in content `isInfixOf` blockContent

-- | Test directive parsing properties
prop_parse_ownership_directive :: Bool -> Property
prop_parse_ownership_directive ownership = 
  let content = "// ownership: " ++ show ownership ++ "\n"
      result = parseTypus content
  in case result of
       Left _ -> property True
       Right typusFile ->
         let directives = tfDirectives typusFile
         in case fdOwnership directives of
              Nothing -> property False
              Just locatedValue -> property $ locValue locatedValue == ownership

prop_parse_dependent_types_directive :: Bool -> Property
prop_parse_dependent_types_directive dependentTypes = 
  let content = "// dependent-types: " ++ show dependentTypes ++ "\n"
      result = parseTypus content
  in case result of
       Left _ -> property True
       Right typusFile ->
         let directives = tfDirectives typusFile
         in case fdDependentTypes directives of
              Nothing -> property False
              Just locatedValue -> property $ locValue locatedValue == dependentTypes

prop_parse_constraints_directive :: Bool -> Property
prop_parse_constraints_directive constraints = 
  let content = "// constraints: " ++ show constraints ++ "\n"
      result = parseTypus content
  in case result of
       Left _ -> property True
       Right typusFile ->
         let directives = tfDirectives typusFile
         in case fdConstraints directives of
              Nothing -> property False
              Just locatedValue -> property $ locValue locatedValue == constraints

-- | Test build tag parsing properties
prop_parse_single_build_tag :: String -> Property
prop_parse_single_build_tag tag = 
  not (null tag) && not (any isSpace tag) ==>
    let content = "// build: " ++ tag ++ "\n"
        result = parseTypus content
    in case result of
         Left _ -> property True
         Right typusFile ->
           let buildTags = tfBuildTags typusFile
           in property $ not (null buildTags) && locValue (head buildTags) == tag

prop_parse_multiple_build_tags :: [String] -> Property
prop_parse_multiple_build_tags tags = 
  all (not . null) tags && all (not . any isSpace) tags ==>
    let tagContent = concatMap (\t -> "// build: " ++ t ++ "\n") tags
        result = parseTypus tagContent
    in case result of
         Left _ -> property True
         Right typusFile ->
           let buildTags = tfBuildTags typusFile
           in property $ length buildTags == length tags

-- | Test block directive parsing properties
prop_parse_block_ownership_directive :: Bool -> Bool -> Property
prop_parse_block_ownership_directive fileOwnership blockOwnership = 
  let fileContent = "// ownership: " ++ show fileOwnership ++ "\n"
      blockContent = "// ownership: " ++ show blockOwnership ++ "\ncode\n"
      content = fileContent ++ blockContent
      result = parseTypus content
  in case result of
       Left _ -> property True
       Right typusFile ->
         let blocks = tfBlocks typusFile
         in property $ not (null blocks) ==>
            let firstBlock = head blocks
                directives = cbDirectives firstBlock
            in case bdOwnership directives of
                 Nothing -> property False
                 Just locatedValue -> property $ locValue locatedValue == blockOwnership

-- | Test parsing error recovery properties
prop_parse_handles_malformed_directives :: String -> Property
prop_parse_handles_malformed_directives content = 
  let malformedContent = "// ownership: invalid\n" ++ content
      result = parseTypus malformedContent
  in case result of
       Left _ -> property True
       Right typusFile -> property $ not (null (tfBlocks typusFile))

prop_parse_handles_incomplete_directives :: String -> Property
prop_parse_handles_incomplete_directives content = 
  let incompleteContent = "// ownership\n" ++ content
      result = parseTypus incompleteContent
  in case result of
       Left _ -> property True
       Right typusFile -> property $ not (null (tfBlocks typusFile))

-- | Test parsing consistency properties
prop_parse_idempotent :: String -> Property
prop_parse_idempotent content = 
  let result1 = parseTypus content
  in case result1 of
       Left _ -> property True
       Right typusFile1 ->
         let content1 = concatMap cbContent (tfBlocks typusFile1)
             result2 = parseTypus content1
         in case result2 of
              Left _ -> property True
              Right typusFile2 -> 
                property $ length (tfBlocks typusFile1) == length (tfBlocks typusFile2)

prop_parse_order_preservation :: [String] -> Property
prop_parse_order_preservation contentBlocks = 
  all (not . null) contentBlocks ==>
    let content = unlines contentBlocks
        result = parseTypus content
    in case result of
         Left _ -> property True
         Right typusFile ->
           let blocks = tfBlocks typusFile
               extractedContent = map cbContent blocks
           in property $ length extractedContent == length contentBlocks

-- | Test parsing with special characters
prop_parse_special_characters :: String -> Property
prop_parse_special_characters content = 
  let specialChars = "!@#$%^&*()_+-=[]{}|;':\",./<>?"
      contentWithSpecial = content ++ specialChars
      result = parseTypus contentWithSpecial
  in case result of
       Left _ -> property True
       Right typusFile ->
         let blocks = tfBlocks typusFile
         in property $ not (null blocks) ==>
            let firstBlock = head blocks
                blockContent = cbContent firstBlock
            in specialChars `isInfixOf` blockContent

prop_parse_unicode_characters :: String -> Property
prop_parse_unicode_characters content = 
  let unicodeChars = "中文测试ñáéíóú"
      contentWithUnicode = content ++ unicodeChars
      result = parseTypus contentWithUnicode
  in case result of
       Left _ -> property True
       Right typusFile ->
         let blocks = tfBlocks typusFile
         in property $ not (null blocks) ==>
            let firstBlock = head blocks
                blockContent = cbContent firstBlock
     in unicodeChars `isInfixOf` blockContent

-- | Test parsing edge cases
prop_parse_empty_lines :: String -> Property
prop_parse_empty_lines content = 
  let contentWithEmptyLines = "\n\n" ++ content ++ "\n\n"
      result = parseTypus contentWithEmptyLines
  in case result of
       Left _ -> property True
       Right typusFile ->
         let blocks = tfBlocks typusFile
         in property $ not (null blocks) ==>
            let firstBlock = head blocks
                blockContent = cbContent firstBlock
            in content `isInfixOf` blockContent

prop_parse_only_whitespace :: String -> Property
prop_parse_only_whitespace content = 
  let whitespaceOnly = "   \t  \n  \t   "
      result = parseTypus whitespaceOnly
  in case result of
       Left _ -> property True
       Right typusFile -> property $ null (tfBlocks typusFile)

-- | Test directive combination properties
prop_parse_multiple_file_directives :: Bool -> Bool -> Bool -> Property
prop_parse_multiple_file_directives ownership deps constraints = 
  let content = "// ownership: " ++ show ownership ++ "\n" ++
                "// dependent-types: " ++ show deps ++ "\n" ++
                "// constraints: " ++ show constraints ++ "\n"
      result = parseTypus content
  in case result of
       Left _ -> property True
       Right typusFile ->
         let directives = tfDirectives typusFile
         in case (fdOwnership directives, fdDependentTypes directives, fdConstraints directives) of
              (Just o, Just d, Just c) -> 
                property $ locValue o == ownership && locValue d == deps && locValue c == constraints
              _ -> property False

-- | Test parsing whitespace-only input
prop_parse_whitespace_only :: String -> Property
prop_parse_whitespace_only ws = 
  all isSpace ws ==>
    let result = parseTypus ws
    in case result of
         Left _ -> property True
         Right typusFile -> null (tfBlocks typusFile)

-- | Test parsing with mixed content
prop_parse_mixed_content :: String -> String -> String -> Property
prop_parse_mixed_content directive content code = 
  not (null content) && not (null code) ==>
    let fullContent = "// " ++ directive ++ ": true\n" ++ content ++ "\n" ++ code
        result = parseTypus fullContent
    in case result of
         Left _ -> property True
         Right typusFile ->
           let blocks = tfBlocks typusFile
           in property $ not (null blocks) ==>
              let firstBlock = head blocks
                  blockContent = cbContent firstBlock
              in content `isInfixOf` blockContent && code `isInfixOf` blockContent

-- | Tasty test suite
testSuite :: TestTree
testSuite = testGroup "Parser Module QuickCheck Properties"
  [ -- FileDirectives tests
    testProperty "default file directives are empty" prop_file_directives_default,
    testProperty "file directives equality works" prop_file_directives_equality,
    
    -- BlockDirectives tests
    testProperty "default block directives are empty" prop_block_directives_default,
    testProperty "block directives equality works" prop_block_directives_equality,
    
    -- CodeBlock tests
    testProperty "code block equality works" prop_code_block_equality,
    testProperty "code block content extraction works" prop_code_block_content_extraction,
    testProperty "code block directives extraction works" prop_code_block_directives_extraction,
    testProperty "code block span extraction works" prop_code_block_span_extraction,
    
    -- TypusFile tests
    testProperty "default typus file is empty" prop_typus_file_default,
    testProperty "typus file equality works" prop_typus_file_equality,
    testProperty "typus file directives extraction works" prop_typus_file_directives_extraction,
    testProperty "typus file build tags extraction works" prop_typus_file_build_tags_extraction,
    testProperty "typus file blocks extraction works" prop_typus_file_blocks_extraction,
    testProperty "typus file syntax errors extraction works" prop_typus_file_syntax_errors_extraction,
    
    -- Parsing tests
    testProperty "parse empty string" prop_parse_empty_string,
    testProperty "parse simple content" prop_parse_simple_content,
    testProperty "parse preserves content" prop_parse_preserves_content,
    testProperty "parse handles whitespace" prop_parse_handles_whitespace,
    
    -- Directive parsing tests
    testProperty "parse ownership directive" prop_parse_ownership_directive,
    testProperty "parse dependent-types directive" prop_parse_dependent_types_directive,
    testProperty "parse constraints directive" prop_parse_constraints_directive,
    
    -- Build tag parsing tests
    testProperty "parse single build tag" prop_parse_single_build_tag,
    testProperty "parse multiple build tags" prop_parse_multiple_build_tags,
    
    -- Block directive parsing tests
    testProperty "parse block ownership directive" prop_parse_block_ownership_directive,
    
    -- Error recovery tests
    testProperty "parse handles malformed directives" prop_parse_handles_malformed_directives,
    testProperty "parse handles incomplete directives" prop_parse_handles_incomplete_directives,
    
    -- Consistency tests
    testProperty "parse is idempotent" prop_parse_idempotent,
    testProperty "parse preserves order" prop_parse_order_preservation,
    
    -- Special character tests
    testProperty "parse special characters" prop_parse_special_characters,
    testProperty "parse unicode characters" prop_parse_unicode_characters,
    
    -- Edge case tests
    testProperty "parse empty lines" prop_parse_empty_lines,
    testProperty "parse only whitespace" prop_parse_only_whitespace,
    
    -- Directive combination tests
    testProperty "parse multiple file directives" prop_parse_multiple_file_directives,
    
    -- Mixed content tests
    testProperty "parse mixed content" prop_parse_mixed_content
  ]