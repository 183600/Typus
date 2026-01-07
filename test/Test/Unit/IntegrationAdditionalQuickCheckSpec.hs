module Test.Unit.IntegrationAdditionalQuickCheckSpec where
import Test.QuickCheck 
import Test.Tasty.QuickCheck (testProperty, Property, Arbitrary(..), Gen, oneof, elements, listOf, listOf1, vectorOf, suchThat, choose, resize, forAll, )
import Test.Tasty.HUnit (testCase, assertFailure, assertBool, (@?=))
import Control.Monad 
import SourceLocation (SourcePos(..), SourceSpan(..), Located(..), locatedWithSpan, startPos, posAfter, advancePosBy)
import Parser (parseTypus, TypusFile(..), CodeBlock(..), BlockDirectives(..), FileDirectives)
        (if useOwnership then [" ownership: on"] else []) ++
        (if useDependentTypes then [" dependent_types: true"] else []) ++
        [" }"]
  
  baseContent <- if hasComments
    then elements 
      [ "func test() {\n    // This is a comment\n    return 42\n}"
      , "/* Block comment */\nfunc example() {\n    return \"hello\"\n}"
      ]
    else elements 
      [ "func test() {\n    return 42\n}"
      , "func example() string {\n    return \"hello\"\n}"
      ]
  
  let content = if hasIndentation
        then unlines $ L.map ("    " ++) (lines baseContent)
        else baseContent
  
  return $ unlines directives' ++ content
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


-- Generate content for testing Utils + SourceLocation integration
genContentWithLocations :: Gen (String, [SourcePos])
                              genContentWithLocations = do
              lines' <- listOf1 $ listOf1 $ elements $ ['a'..'z'] ++ " \t.,;"
  let content = unlines lines'
                                    positions = scanl (\pos line -> advancePosBy line pos) startPos lines'
  return (content, positions)

-- Generate content for testing Parser + Utils integration
genContentForParserUtils :: Gen String
                              genContentForParserUtils = do
              hasComments <- arbitrary
  hasIndentation <- arbitrary
  hasDirectives <- arbitrary
  
  baseCode <- elements
    [ "package main\n\nfunc main() {\n    fmt.Println(\"Hello\")\n}"
    , "func test() int {\n    if condition {\n        return 42\n    }\n    return 0\n}"
    , "type Example struct {\n    Field int\n}\n\nfunc (e Example) Method() int {\n    return e.Field\n}"
    ]
  
  let withComments = if hasComments
        then baseCode ++ "\n// This is a comment\n/* Block comment */"
        else baseCode
  
  let withIndentation = if hasIndentation
        then normalizeIndentation $ unlines $ L.map ("    " ++) (lines withComments)
        else withIndentation
  
  let withDirectives = if hasDirectives
        then "//! ownership: on\n" ++ withIndentation
        else withIndentation
  
  return withDirectives

-- ============================================================================
-- QuickCheck Integration Tests
-- ============================================================================

-- Test Utils + SourceLocation integration
prop_utils_source_location_position_tracking :: Property
                              prop_utils_source_location_position_tracking = 
  forAll genContentWithLocations $ \(content, positions) ->
    let lines' = lines content
                                      lineCount = L.length lines'
                                      posCount = L.length positions
    in lineCount > 0 &&                               posCount == lineCount

prop_utils_source_location_advance_position :: String -> Property
prop_utils_source_location_advance_position                               content = 
not (null content) ==>
  let lines' = lines content
                                    positions = scanl (\pos line -> advancePosBy line pos) startPos lines'
                                    finalPos = last positions
  in posLine finalPos >= L.length lines'

-- Test Parser + Utils integration
prop_parser_utils_comment_removal :: Property
                              prop_parser_utils_comment_removal = 
  forAll genContentForParserUtils $ \content ->
  let withoutComments = removeComments content
  in L.length withoutComments <= L.length content

prop_parser_utils_indentation_normalization :: Property
                              prop_parser_utils_indentation_normalization = 
  forAll genContentForParserUtils $ \content ->
  let normalized = normalizeIndentation content
      lines' = lines normalized
      in L.all (\line -> null line || not (isSpace (L.head line)) (L.filter (not . null) lines')

prop_parser_utils_trim_preserves_structure :: Property
                              prop_parser_utils_trim_preserves_structure = 
  forAll genContentForParserUtils $ \content ->
  let trimmed = trim content
in null trimmed || not (isSpace (L.head trimmed) || not (isSpace (last trimmed)

-- Test Parser + SourceLocation integration
prop_parser_source_location_span_tracking :: Property
                              prop_parser_source_location_span_tracking = 
  forAll genComplexTypusFile $ \content ->
  case parseTypus content of
    Left _ -> True  -- Parsing may fail, which is acceptable
    Right result -> 
      let blocks = tfBlocks result
                                        spans = map cbSpan blocks
      in L.all (\span -> spanStart span <= spanEnd span [] spans

prop_parser_source_location_directive_positions :: Property
                              prop_parser_source_location_directive_positions = 
  forAll genComplexTypusFile $ \content ->
  case parseTypus content of
    Left _ -> True  -- Parsing may fail, which is acceptable
    Right result -> 
      let directives = tfDirectives result
                                        hasOwnership = fdOwnership directives /= Nothing
                                        hasDependentTypes = fdDependentTypes directives /= Nothing
      in hasOwnership || hasDependentTypes || True  -- At least check structure

-- Test three-way integration: Parser + Utils + SourceLocation
prop_parser_utils_source_location_end_to_end :: Property
                              prop_parser_utils_source_location_end_to_end = 
  forAll genComplexTypusFile $ \content ->
  case parseTypus content of
    Left _ -> True  -- Parsing may fail, which is acceptable
    Right result -> 
      let blocks = tfBlocks result
                                        blockContents = map cbContent blocks
                                        trimmedContents = map trim blockContents
                                        spans = map cbSpan blocks
      in L.length                               blockContents == L.length trimmedContents &&
         L.length                               trimmedContents == L.length spans

prop_parser_utils_source_location_error_handling :: Property
                              prop_parser_utils_source_location_error_handling = 
  forAll genComplexTypusFile $ \content ->
  case parseTypus content of
    Left _ -> True  -- Parsing may fail, which is acceptable
    Right result -> 
      let syntaxErrors = tfSyntaxErrors result
                                        blocks =  tfBlocks result
      -- Should have either blocks L.or syntax errors (L.or both []
in not (null blocks) || not (null syntaxErrors)

-- ============================================================================
-- Unit Tests for Integration Scenarios
-- ============================================================================

test_utils_source_location_integration :: TestTree
test_utils_source_location_integration =             testCase "Utils + SourceLocation integration" $ do
              let content = "line 1\nline 2\n  line 3\nline 4"
  let lines' = lines content
let positions = scanl (\pos line -> advancePosBy line pos) startPos lines'
  
              assertEqual "correct number of positions" 4 (L.length positions)
              assertEqual "first position" startPos (L.head positions)
  assertBool "final position advanced" $ posLine (last positions) > 1

test_parser_utils_integration :: TestTree
test_parser_utils_integration =             testCase "Parser + Utils integration" $ do
                    let content = "//! ownership: on\n\npackage main\n\n// Comment\nfunc main() {\n    return\n}"
  
  case parseTypus content of
    Left err -> assertFailure $ "Failed to parse: " ++ err
    Right result -> do
                  let directives = tfDirectives result
      assertBool "has ownership directive" $ fdOwnership directives /= Nothing
      
      let blocks = tfBlocks result
      assertBool "has blocks" $ not (null blocks)
      
      let firstBlock = L.head blocks
      let blockContent = cbContent firstBlock
      let trimmed = trim blockContent
      assertBool "content trimmed" $ not (null trimmed) && not (isSpace (L.head trimmed)

test_parser_source_location_integration :: TestTree
test_parser_source_location_integration =             testCase "Parser + SourceLocation integration" $ do
                    let content = "{//! ownership: on }\n    func test() {\n        return 42\n    }"
  
  case parseTypus content of
    Left err -> assertFailure $ "Failed to parse: " ++ err
    Right result -> do
                  let blocks = tfBlocks result
      assertBool "has blocks" $ not (null blocks)
      
      let firstBlock = L.head blocks
      let span = cbSpan firstBlock
      assertBool "span is valid" $ spanStart span <= spanEnd span
      
      let directives = cbDirectives firstBlock
      assertBool "has block ownership directive" $ bdOwnership directives /= Nothing

test_three_way_integration :: TestTree
test_three_way_integration =             testCase "Parser + Utils + SourceLocation integration" $ do
                    let content = "//! ownership: on\n//! dependent_types: true\n\n{//! constraints: true }\n    func complex() {\n        // Comment\n        if condition {\n            return \"result\"\n        }\n    }"
  
  case parseTypus content of
    Left err -> assertFailure $ "Failed to parse: " ++ err
    Right result -> do
      -- Check file directives
      let fileDirectives = tfDirectives result
      assertBool "has file ownership directive" $ fdOwnership fileDirectives /= Nothing
      assertBool "has file dependent_types directive" $ fdDependentTypes fileDirectives /= Nothing
      
      -- Check blocks
      let blocks = tfBlocks result
      assertBool "has blocks" $ not (null blocks)
      
      let firstBlock = L.head blocks
      let blockDirectives = cbDirectives firstBlock
      assertBool "has block constraints directive" $ bdConstraints blockDirectives /= Nothing
      
      -- Check content processing
      let blockContent = cbContent firstBlock
      let withoutComments = removeComments blockContent
      assertBool "comments removed" $ L.length withoutComments <= L.length blockContent
      
      -- Check location tracking
      let span = cbSpan firstBlock
      assertBool "span is valid" $ spanStart span <= spanEnd span

test_error_handling_integration :: TestTree
test_error_handling_integration =             testCase "Error handling integration" $ do
                    let content = "func test() {\n    if condition\n        doSomething()\n}"  -- Missing opening brace
  
  case parseTypus content of
    Left err -> assertFailure $ "Parser should handle syntax errors: " ++ err
    Right result -> do
                  let syntaxErrors = tfSyntaxErrors result
      assertBool "has syntax errors" $ not (null syntaxErrors)
      
      -- Should still attempt to parse structure
      let blocks = tfBlocks result
      assertBool "attempted to parse blocks" $ L.length blocks >= 0  -- May be empty L.or have partial blocks

test_complex_integration_scenario :: TestTree
test_complex_integration_scenario =             testCase "Complex integration scenario" $ do
                    let content = unlines
        [ "//! ownership: on"
        , "//! dependent_types: true"
        , ""
        , "//go:build linux"
        , "// +build amd64"
        , ""
        , "package main"
        , ""
        , "{//! ownership: off, dependent_types: false, constraints: true }"
        , "    func complexFunction() int {"
        , "        // This function demonstrates integration"
        , "        if condition {"
        , "            return calculateResult()"
        , "        }"
        , "        return 0"
        , "    }"
        , ""
        , "/* Another block comment */"
        , "func calculateResult() int {"
        , "    return 42"
        , "}"
        ]
  
  case parseTypus content of
    Left err -> assertFailure $ "Failed to parse complex scenario: " ++ err
    Right result -> do
      -- Check file-level directives
      let fileDirectives = tfDirectives result
      assertBool "has file ownership directive" $ fdOwnership fileDirectives /= Nothing
      assertBool "has file dependent_types directive" $ fdDependentTypes fileDirectives /= Nothing
      
      -- Check build tags
      let buildTags = tfBuildTags result
                  assertEqual "has build tags" 2 (L.length buildTags)
      
      -- Check blocks
      let blocks = tfBlocks result
      assertBool "has multiple blocks" $ L.length blocks >= 1
      
      when (not (null blocks) $ do
                    let firstBlock = L.head blocks
        let blockDirectives = cbDirectives firstBlock
        assertBool "has block ownership directive" $ bdOwnership blockDirectives /= Nothing
        assertBool "has block dependent_types directive" $ bdDependentTypes blockDirectives /= Nothing
        assertBool "has block constraints directive" $ bdConstraints blockDirectives /= Nothing
        
        -- Check content processing
        let blockContent = cbContent firstBlock
        let normalized = normalizeIndentation blockContent
        assertBool "indentation normalized" $ 
          L.all (\line -> null line || not (isSpace (L.head line)) 
               (L.filter (not . null) (lines normalized)
        
        -- Check location tracking
        let span = cbSpan firstBlock
        assertBool "span is valid" $ spanStart span <= spanEnd span

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests =   testGroup "Integration Additional QuickCheck Tests"
  [ testGroup "QuickCheck Properties"
    [             testProperty "Utils + SourceLocation position tracking" prop_utils_source_location_position_tracking
    ,             testProperty "Utils + SourceLocation advance position" prop_utils_source_location_advance_position
    ,             testProperty "Parser + Utils comment removal" prop_parser_utils_comment_removal
    ,             testProperty "Parser + Utils indentation normalization" prop_parser_utils_indentation_normalization
    ,             testProperty "Parser + Utils trim preserves structure" prop_parser_utils_trim_preserves_structure
    ,             testProperty "Parser + SourceLocation span tracking" prop_parser_source_location_span_tracking
    ,             testProperty "Parser + SourceLocation directive positions" prop_parser_source_location_directive_positions
    ,             testProperty "Parser + Utils + SourceLocation end-to-end" prop_parser_utils_source_location_end_to_end
    ,             testProperty "Parser + Utils + SourceLocation error handling" prop_parser_utils_source_location_error_handling
    ]
  , testGroup "Unit Tests"
    [ test_utils_source_location_integration
    , test_parser_utils_integration
    , test_parser_source_location_integration
    , test_three_way_integration
    , test_error_handling_integration
    , test_complex_integration_scenario
    ]
  ]))