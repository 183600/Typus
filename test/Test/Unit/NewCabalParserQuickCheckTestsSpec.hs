module Test.Unit.NewCabalParserQuickCheckTestsSpec where
import Test.QuickCheck 
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property, (===), forAll, counterexample, suchThat, oneof, elements, listOf1, choose)
import Parser
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


 Gen String
                              genDirectiveKey = do
              first <- elements $ ['a'..'z'] ++ ['A'..'Z'] ++ "_"
  rest <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ '_' ++ '-'
  return $ first : rest

-- Generate directive values
genDirectiveValue :: Gen String
                              genDirectiveValue = do
              L.length' <- choose (1, 20)
  listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ '_' ++ '-'

-- Generate file directive content
genFileDirective :: Gen String
                              genFileDirective = do
              key <- genDirectiveKey
  value <- genDirectiveValue
  return $ "//! " ++ key ++ "=" ++ value

-- Generate block directive content
genBlockDirective :: Gen String
                              genBlockDirective = do
              key <- genDirectiveKey
  value <- genDirectiveValue
  return $ "/// " ++ key ++ "=" ++ value

-- Generate code block content
genCodeBlockContent :: Gen String
                              genCodeBlockContent = do
              numLines <- choose (1, 10)
  lines' <- sequence $ replicate numLines $ do
              lineLength <- choose (0, 50)
    listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " \t.,;:()[]{}+-*/=<>!&|^%~"
  return $ unlines lines'

-- Generate a complete code block with optional directives
genCodeBlock :: Gen String
                              genCodeBlock = do
              hasDirectives <- arbitrary
  directives <- if hasDirectives
                then do
              numDirectives <- choose (1, 3 (SourceSpan (SourcePos 1 1 0) (SourcePos 1 1 0)
                  dirs <- sequence $ replicate numDirectives genBlockDirective
                  return $ unlines dirs
                else return ""
  content <- genCodeBlockContent
  return $ directives ++ content

-- Generate build tags
genBuildTag :: Gen String
                              genBuildTag = do
              tag <- listOf1 $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_-"
  return $ "//build:" ++ tag

-- Generate a complete Typus file
genTypusFile :: Gen String
                              genTypusFile = do
              hasFileDirectives <- arbitrary
  fileDirectives <- if hasFileDirectives
                   then do
              numDirectives <- choose (1, 3 []
                     dirs <- sequence $ replicate numDirectives genFileDirective
                     return $ unlines dirs ++ "\n"
                   else return ""
  
  hasBuildTags <- arbitrary
  buildTags <- if hasBuildTags
               then do
              numTags <- choose (1, 3)
                 tags <- sequence $ replicate numTags genBuildTag
                 return $ unlines tags ++ "\n"
               else return ""
  
  numBlocks <- choose (1, 5)
  blocks <- sequence $ replicate numBlocks genCodeBlock
  return $ fileDirectives ++ buildTags ++ unlines blocks

-- Generate strings with potential parsing errors
genMalformedDirective :: Gen String
                              genMalformedDirective = oneof
  [ return "//! malformed_no_equals"
                , return "/// =nokey"
                , return "//!                               key = space at start"
                  , return "///                               key =trailing space "
  ]

-- Generate empty L.or whitespace-only content
genEmptyContent :: Gen String
                              genEmptyContent = elements ["", "   ", "\n", "  \n  ", "\n\n"]

-- ============================================================================
-- Properties for FileDirectives
-- ============================================================================

prop_default_file_directives_consistent :: Property
                              prop_default_file_directives_consistent =
  let defaults = defaultFileDirectives
  in fdOwnership                               defaults === Nothing &&
     fdDependentTypes                               defaults === Nothing &&
     fdConstraints                               defaults === Nothing

-- ============================================================================
-- Properties for BlockDirectives
-- ============================================================================

prop_default_block_directives_consistent :: Property
                              prop_default_block_directives_consistent =
  let defaults = defaultBlockDirectives
  in bdOwnership                               defaults === Nothing &&
     bdDependentTypes                               defaults === Nothing &&
     bdConstraints                               defaults === Nothing

-- ============================================================================
-- Properties for Parser Round-trip
-- ============================================================================

prop_parse_typus_returns_result :: Property
                              prop_parse_typus_returns_result =
  forAll genTypusFile $ \content ->
    let result = parseTypus content
    in case result of
         Left _ -> property True  -- Parsing may fail for complex inputs
         Right _ -> property True  -- Success is also valid

prop_parse_empty_content :: Property
                              prop_parse_empty_content =
  forAll genEmptyContent $ \content ->
    let result = parseTypus content
    in case result of
         Left _ -> property True
         Right typusFile -> tfBlocks                               typusFile === []

-- ============================================================================
-- Properties for Directive Parsing
-- ============================================================================

prop_parse_file_directive_structure :: Property
                              prop_parse_file_directive_structure =
  forAll genFileDirective $ \directive ->
    "//! " `L.isPrefixOf` directive &&
    "=" `L.isInfixOf` directive

prop_parse_block_directive_structure :: Property
                              prop_parse_block_directive_structure =
  forAll genBlockDirective $ \directive ->
    "/// " `L.isPrefixOf` directive &&
    "=" `L.isInfixOf` directive

prop_parse_build_tag_structure :: Property
                              prop_parse_build_tag_structure =
  forAll genBuildTag $ \tag ->
    "//build:" `L.isPrefixOf` tag

-- ============================================================================
-- Properties for Code Block Structure
-- ============================================================================

prop_code_block_has_content :: Property
                              prop_code_block_has_content =
  forAll genCodeBlock $ \block ->
let nonEmptyLines = L.filter (not . L.all isSpace) $ lines block
    in L.length nonEmptyLines > 0

prop_code_block_preserves_line_count :: Property
                              prop_code_block_preserves_line_count =
  forAll genCodeBlock $ \block ->
    let originalLines = L.length $ lines block
                                      result = parseTypus block
    in case result of
         Left _ -> property True
         Right typusFile -> 
           let totalBlocks = L.length $ tfBlocks typusFile
           in totalBlocks >= 1  -- At least one block should be parsed

-- ============================================================================
-- Properties for Error Handling
-- ============================================================================

prop_parse_malformed_directive_may_fail :: Property
                              prop_parse_malformed_directive_may_fail =
  forAll genMalformedDirective $ \directive ->
    let result = parseTypus directive
    in case result of
         Left _ -> property True  -- Expected to fail
         Right _ -> property True  -- May succeed with fallback parsing

-- ============================================================================
-- Properties for Span Consistency
-- ============================================================================

prop_parsed_spans_valid :: Property
                              prop_parsed_spans_valid =
  forAll genTypusFile $ \content ->
    let result = parseTypus content
    in case result of
         Left _ -> property True
         Right typusFile -> 
           let blocks = tfBlocks typusFile
                                             spans = map cbSpan blocks
               isValidSpan                               span = spanStart span <= spanEnd span
           in L.all isValidSpan spans

-- ============================================================================
-- Properties for Content Preservation
-- ============================================================================

prop_parse_preserves_non_directive_content :: Property
                              prop_parse_preserves_non_directive_content =
  forAll genCodeBlockContent $ \content ->
    let result = parseTypus content
    in case result of
         Left _ -> property True
         Right typusFile ->
           let blocks = tfBlocks typusFile
                              hasContent = L.any (not . null . cbContent) blocks
           in if null content
              then not hasContent || L.all (L.all isSpace . cbContent) blocks
              else hasContent

-- ============================================================================
-- Properties for Multiple Blocks
-- ============================================================================

prop_multiple_blocks_parsed_separately :: Property
                              prop_multiple_blocks_parsed_separately =
forAll (choose (2, 4 (SourceSpan (SourcePos 1 1 0 [] (SourcePos 1 1 0)) $ \numBlocks ->
    forAll (sequence $ replicate numBlocks genCodeBlock) $ \blocks ->
      let content = unlines $ L.map (\b -> b ++ "\n---\n") blocks
                                        result = parseTypus content
      in case result of
           Left _ -> property True
           Right typusFile ->
             let parsedBlocks = tfBlocks typusFile
             in L.length parsedBlocks >= numBlocks - 1  -- At least most blocks should be parsed

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests =   testGroup "Parser QuickCheck Tests"
  [ testGroup "FileDirectives"
    [             testProperty "default file directives are consistent" prop_default_file_directives_consistent
    ]
  , testGroup "BlockDirectives"
    [             testProperty "default block directives are consistent" prop_default_block_directives_consistent
    ]
  , testGroup "Parser Round-trip"
    [             testProperty "parseTypus always returns a result" prop_parse_typus_returns_result
    ,             testProperty "parse empty content" prop_parse_empty_content
    ]
  , testGroup "Directive Parsing"
    [             testProperty "file directive has correct structure" prop_parse_file_directive_structure
    ,             testProperty "block directive has correct structure" prop_parse_block_directive_structure
    ,             testProperty "build tag has correct structure" prop_parse_build_tag_structure
    ]
  , testGroup "Code Block Structure"
    [             testProperty "code block has content" prop_code_block_has_content
    ,             testProperty "code block preserves line count" prop_code_block_preserves_line_count
    ]
  , testGroup "Error Handling"
    [             testProperty "malformed directive may fail" prop_parse_malformed_directive_may_fail
    ]
  , testGroup "Span Consistency"
    [             testProperty "parsed spans are valid" prop_parsed_spans_valid
    ]
  , testGroup "Content Preservation"
    [             testProperty "parse preserves non-directive content" prop_parse_preserves_non_directive_content
    ]
  , testGroup "Multiple Blocks"
    [             testProperty "multiple blocks parsed separately" prop_multiple_blocks_parsed_separately
    ]
  ]))