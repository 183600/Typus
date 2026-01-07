module Test.Unit.NewCoreBoundaryConditionsQuickCheckSpec where


import Test.Tasty
import Test.Tasty.QuickCheck 
import SourceLocation (SourcePos(..), startPos, posAt, advancePos, advancePosBy)
import Parser 
      let nested = L.concat $ replicate depth "// @ownership {\n" ++ [s] ++ L.concat (replicate depth "\n}\n")
      in case parseTypus nested of
        Left _ -> True
        Right _ -> True

-- | Parser: parseTypus should handle malformed directives
prop_parser_malformed_directives :: String -> Bool
prop_parser_malformed_directives                               s = 
  let malformed = "// @invalid-directive-name-123!@#\n" ++ s
  in case parseTypus malformed of
    Left _ -> True
    Right _ -> True

-- | Parser: parseTypus should handle unicode edge cases
prop_parser_unicode_edge_cases :: String -> Bool
prop_parser_unicode_edge_cases                               s = 
  let unicodeEdge = s ++ "\n\x1F600\x1F601\x1F602\n" ++ s  -- Emoji
  in case parseTypus unicodeEdge of
    Left _ -> True
    Right _ -> True

-- | Property: String processing should handle null bytes gracefully
prop_null_byte_handling :: String -> Bool
prop_null_byte_handling                               s = 
  let withNull = take 50 s ++ "\0" ++ drop 50 s
                                    trimmed = trim withNull
                                    split = splitBy ',' withNull
  in L.length split >= 1  -- Should not crash

-- | Property: Large input handling
prop_large_input_handling :: Int -> String -> Bool
prop_large_input_handling n                               s = 
  let n > 0 && n <                               1000 ==>
      let largeInput = L.concat $ replicate n s
      in case parseTypus largeInput of
        Left _ -> True
        Right _ -> True

-- | Property: Extreme whitespace combinations
prop_extreme_whitespace :: String -> Bool
prop_extreme_whitespace                               s = 
  let extremeWs = concatMap (\c -> if isSpace c then replicate 10 c else [c]) s
                                    processed = normalizeIndentation extremeWs
  in L.length processed <= L.length extremeWs

-- ============================================================================
-- Test Suite
-- ============================================================================

testSuite :: TestTree
testSuite = testGroup "Core Module Boundary Conditions QuickCheck Tests"
  [ testGroup "Utils Boundary Tests"
    [             testProperty "trim empty string" prop_trim_empty_string
    ,             testProperty "trim whitespace only" prop_trim_whitespace_only
    ,             testProperty "splitBy empty input" prop_split_by_empty
    ,             testProperty "splitByCollapsed empty input" prop_split_by_collapsed_empty
    ,             testProperty "removeComments empty input" prop_remove_comments_empty
    ,             testProperty "normalizeIndentation empty input" prop_normalize_indentation_empty
    ]
  , testGroup "SourceLocation Boundary Tests"
    [             testProperty "posAt L.minimum values" prop_pos_at_minimum
    ,             testProperty "posAt large values" prop_pos_at_large
    ,             testProperty "advancePos control characters" prop_advance_pos_control
    ,             testProperty "advancePosBy empty string" prop_advance_pos_by_empty
    ]
  , testGroup "Parser Boundary Tests"
    [             testProperty "parser long lines" prop_parser_long_lines
    ,             testProperty "parser deep nesting" prop_parser_deep_nesting
    ,             testProperty "parser malformed directives" prop_parser_malformed_directives
    ,             testProperty "parser unicode edge cases" prop_parser_unicode_edge_cases
    ]
  , testGroup "Edge Case Tests"
    [             testProperty "null byte handling" prop_null_byte_handling
    ,             testProperty "large input handling" prop_large_input_handling
    ,             testProperty "extreme whitespace" prop_extreme_whitespace
    ]
  ]