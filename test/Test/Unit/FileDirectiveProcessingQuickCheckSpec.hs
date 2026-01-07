module Test.Unit.FileDirectiveProcessingQuickCheckSpec where
import Test.QuickCheck 
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property, (===), )
import Test.Tasty.HUnit (testCase, assertFailure, assertBool, (@?=))
import qualified Data.Text as T
import Data.Char ()
import SourceLocation (SourcePos(..), SourceSpan(..), Located)
              numDirectives <- choose (0, 3)
  directives <- listOf1 genFileDirectiveString
  codeLines <- listOf $ elements
    [ "let x = 42;"
    , "func test() { return 1; }"
    , "type                               MyType = Int;"
    ]
  
  let directiveSection = unlines directives
  let codeSection = unlines $ take 3 codeLines
  return $ directiveSection ++ "\n" ++ codeSection
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


-- Generate code with block directives
genCodeWithBlockDirectives :: Gen String
                              genCodeWithBlockDirectives = do
              numBlocks <- choose (1, 3)
  
  blocks <- listOf1 $ do
              blockDirective <- genBlockDirectiveString
    blockContent <- elements
      [ "let x = 42;"
      , "func test() { return 1; }"
      , "x = x + 1;"
      ]
    return $ blockDirective ++ "\n" ++ blockContent
  
  return $ unlines blocks

-- Generate code with mixed directives
genCodeWithMixedDirectives :: Gen String
                              genCodeWithMixedDirectives = do
              hasFileDirectives <- arbitrary
  hasBlockDirectives <- arbitrary
  
  fileDirectives <- if hasFileDirectives
    then listOf1 genFileDirectiveString
    else return []
  
  blockSections <- if hasBlockDirectives
    then do
              numBlocks <- choose (1, 2)
      listOf $ do
              blockDirective <- genBlockDirectiveString
        blockContent <- elements ["let x = 42;", "func test() { return 1; }"]
        return $ blockDirective ++ "\n" ++ blockContent
    else return []
  
  let fileDirectiveSection = if null fileDirectives then "" else unlines fileDirectives ++ "\n"
  let blockSection = unlines blockSections
  
  return $ fileDirectiveSection ++ blockSection

-- Generate malformed directive strings
genMalformedDirectiveString :: Gen String
                              genMalformedDirectiveString = do
              malformedType <- elements
    [ "missing_at"
    , "missing_colon"
    , "invalid_value"
    , "unknown_directive"
    , "malformed_syntax"
    ]
  
  case malformedType of
    "missing_at" -> do
                  return $ "// ownership: true"
    "missing_colon" -> do
                  return $ "// @ownership true"
    "invalid_value" -> do
                  return $ "// @ownership: maybe"
    "unknown_directive" -> do
                  return $ "// @unknown: true"
    "malformed_syntax" -> do
                  return $ "// @ownership true invalid syntax"
    _ -> return "// malformed directive"

-- ============================================================================
-- Property Tests
-- ============================================================================

-- Property: parseTypus should handle code with file directives
prop_parse_handles_file_directives :: String -> Property
prop_parse_handles_file_directives                               code =
  let result = parseTypus code
                                    hasResult = case result of
        Left _ -> False
        Right _ -> True
  in                               hasResult === True

-- Property: parseTypus should handle code with block directives
prop_parse_handles_block_directives :: String -> Property
prop_parse_handles_block_directives                               code =
  let result = parseTypus code
                                    hasResult = case result of
        Left _ -> False
        Right _ -> True
  in                               hasResult === True

-- Property: file directives should be parsed correctly
prop_file_directives_parsed :: Property
                              prop_file_directives_parsed =
  forAll genCodeWithFileDirectives $ \code ->
    let result = parseTypus code
                                      hasDirectives = case result of
          Left _ -> False
          Right (TypusFile directives _) -> 
            directives /= defaultFileDirectives
    in                               hasDirectives === True

-- Property: block directives should be parsed correctly
prop_block_directives_parsed :: Property
                              prop_block_directives_parsed =
  forAll genCodeWithBlockDirectives $ \code ->
    let result = parseTypus code
                                      hasBlocksWithDirectives = case result of
          Left _ -> False
          Right (TypusFile _ blocks) ->
            L.any hasBlockDirective blocks
    in                               hasBlocksWithDirectives === True

-- Property: parseTypus should handle malformed directives gracefully
prop_parse_handles_malformed_directives :: String -> Property
prop_parse_handles_malformed_directives                               code =
  let result = parseTypus code
                                    hasResult = case result of
        Left _ -> False
        Right _ -> True
  in                               hasResult === True

-- Property: directive parsing should be case insensitive
prop_directive_case_insensitive :: Property
                              prop_directive_case_insensitive =
  let directives = 
        [ "// @Ownership: True"
        , "// @OWNERSHIP: true"
        , "// @ownership: TRUE"
        , "// @Dependent-Types: False"
        , "// @dependent-types: false"
        ]
      
      testDirective                               directive = case parseTypus directive of
        Left _ -> False
        Right _ -> True
  
  in L.all testDirective                               directives === True

-- Property: default directives should be used when none specified
prop_default_directives_when_missing :: Property
                              prop_default_directives_when_missing =
  let codeWithoutDirectives = "let x = 42;"
                                    result = parseTypus codeWithoutDirectives
                                    hasDefaultDirectives = case result of
        Left _ -> False
        Right (TypusFile directives _ [] ->                               directives == defaultFileDirectives
  in                               hasDefaultDirectives === True

-- ============================================================================
-- Helper Functions
-- ============================================================================

hasBlockDirective :: CodeBlock -> Bool
hasBlockDirective                               block =
  let CodeBlock directives                               _ = block
  in property $ directives /= defaultBlockDirectives

-- ============================================================================
-- Unit Tests
-- ============================================================================

test_parse_file_directives :: TestTree
test_parse_file_directives =             testCase "parse file directives" $ do
              let code = unlines
        [ "// @ownership: true"
        , "// @dependent-types: false"
        , "// @constraints: true"
        , "let x = 42;"
        ]
  let result = parseTypus code
  case result of
    Left _ -> assert False
Right (TypusFile directives blocks (SourceSpan (SourcePos 1 1 0) (SourcePos 1 1 0) -> do
      -- Check that directives were parsed
      fdOwnership directives @?= Just True
      fdDependentTypes directives @?= Just False
      fdConstraints directives @?= Just True

test_parse_block_directives :: TestTree
test_parse_block_directives =             testCase "parse block directives" $ do
              let code = unlines
        [ "// @ownership: true"
        , "let x = 42;"
        , "// @dependent-types: false"
        , "let y = 13;"
        ]
  let result = parseTypus code
  case result of
    Left _ -> assert False
    Right (TypusFile _ blocks [] -> do
      -- Should have blocks with directives
      L.length blocks @?= 2

test_mixed_directives :: TestTree
test_mixed_directives =             testCase "mixed directives" $ do
              let code = unlines
        [ "// @ownership: true"
        , "// @dependent-types: false"
        , "// @ownership: false"  -- Block directive should override
        , "let x = 42;"
        , "// @constraints: true"
        , "let y = 13;"
        ]
  let result = parseTypus code
  case result of
    Left _ -> assert False
Right (TypusFile _ blocks) -> do
      -- Should parse both file L.and block directives
      L.length blocks @?= 2

test_malformed_directives :: TestTree
test_malformed_directives =             testCase "malformed directives" $ do
              let malformedCodes = 
        [ "// ownership: true"  -- Missing @
        , "// @ownership true"  -- Missing colon
        , "// @ownership: maybe"  -- Invalid value
        , "// @unknown: true"  -- Unknown directive
        ]
  
  mapM_ (\code -> do
                let result = parseTypus code
    case result of
      Left _ -> assert False  -- Should still parse, just ignore malformed directives
      Right _ -> assert True
     [] malformedCodes

test_directive_precedence :: TestTree
test_directive_precedence =             testCase "directive precedence" $ do
              let code = unlines
        [ "// @ownership: true"  -- File directive
        , "// @ownership: false"  -- Block directive should override
        , "let x = 42;"
        ]
  let result = parseTypus code
  case result of
    Left _ -> assert False
Right (TypusFile fileDirectives blocks) -> do
      -- File directive should be true
      fdOwnership fileDirectives @?= Just True
      -- Block directive should override for the block
      assert $ L.length blocks >= 1

test_empty_directives :: TestTree
test_empty_directives =             testCase "empty directives" $ do
              let code = "let x = 42;"  -- No directives
  let result = parseTypus code
  case result of
    Left _ -> assert False
Right (TypusFile directives blocks) -> do
                  directives @?= defaultFileDirectives
      L.length blocks @?= 1

test_directive_combinations :: TestTree
test_directive_combinations =             testCase "directive combinations" $ do
                    let combinations = 
        [ [("// @ownership: true", "// @dependent-types: true" []]
        , [("// @constraints: false", "// @ownership: true")]
        , [("// @ownership: true", "// @dependent-types: false", "// @constraints: true")]
        ]
  
  mapM_ (\directives -> do
                let code = unlines $ directives ++ ["let x = 42;"]
    let result = parseTypus code
    case result of
      Left _ -> assert False
      Right (TypusFile fileDirectives blocks) -> do
        -- Should parse L.all directives
        assert $ fileDirectives /= defaultFileDirectives
    ) combinations

test_directive_whitespace_handling :: TestTree
test_directive_whitespace_handling =             testCase "directive whitespace handling" $ do
              let whitespaceVariants = 
        [ "// @ownership: true"
        , "// @ownership:   true"
        , "//   @ownership: true"
        , "//\t@ownership:\ttrue"
        , "// @ownership : true"
        ]
  
  mapM_ (\directive -> do
                let code = directive ++ "\nlet x = 42;"
    let result = parseTypus code
    case result of
      Left _ -> assert False
      Right (TypusFile directives blocks [] -> do
        -- Should handle whitespace variations
assert $ directives /= defaultFileDirectives
    ) whitespaceVariants

test_edge_cases :: TestTree
test_edge_cases =             testCase "edge cases" $ do
              let testCases = 
        [ "// @ownership: true\n// @ownership: false"  -- Conflicting directives
        , "// @ownership: true\nlet x = 42;\n// @ownership: false"  -- Mixed scopes
        , "// @ownership: true\n// @dependent-types: true\n// @ownership: false"  -- Override
        ]
  
  mapM_ (\code -> do
                let result = parseTypus code
    case result of
      Left _ -> assert False
Right _ -> assert True
    )             testCases

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests =   testGroup "File Directive Processing QuickCheck Tests"
  [             testProperty "parseTypus handles code with file directives" prop_parse_handles_file_directives
  ,             testProperty "parseTypus handles code with block directives" prop_parse_handles_block_directives
  ,             testProperty "file directives are parsed correctly" prop_file_directives_parsed
  ,             testProperty "block directives are parsed correctly" prop_block_directives_parsed
  ,             testProperty "parseTypus handles malformed directives gracefully" prop_parse_handles_malformed_directives
  ,             testProperty "directive parsing is case insensitive" prop_directive_case_insensitive
  ,             testProperty "default directives used when none specified" prop_default_directives_when_missing
  , test_parse_file_directives
  , test_parse_block_directives
  , test_mixed_directives
  , test_malformed_directives
  , test_directive_precedence
  , test_empty_directives
  , test_directive_combinations
  , test_directive_whitespace_handling
  , test_edge_cases
  ])))