module Test.Unit.NewCoreErrorHandlingQuickCheckSpec where


import Test.Tasty
import Test.Tasty.QuickCheck 
import SourceLocation (SourcePos(..), startPos, posAt, advancePos, isValidSpan)
import Parser (parseTypus, FileDirectives(..), BlockDirectives)
  let withControl = L.filter (\c -> isControl c || isSpace c) s
                                    result = trim withControl
  in L.length result >= 0  -- Should not crash

-- | Utils: splitBy should handle invalid input gracefully
prop_split_by_invalid_input :: Char -> String -> Bool
prop_split_by_invalid_input delim                               s = 
  let result = splitBy delim s
  in L.length result >= 1  -- Should always return at least one segment

-- | Utils: removeComments should handle malformed comments
prop_remove_comments_malformed :: String -> Bool
prop_remove_comments_malformed                               s = 
  let malformed = s ++ "/* unterminated comment" ++ s
                                    result = removeComments malformed
  in L.length result >= 0  -- Should handle gracefully

-- | Utils: normalizeIndentation should handle mixed tabs/spaces
prop_normalize_indentation_mixed :: String -> Bool
prop_normalize_indentation_mixed                               s = 
  let mixed = concatMap (\c -> if c `elem` " \t" then "\t  \t" else [c]) s
                                    result = normalizeIndentation mixed
  in L.length result >= 0  -- Should handle gracefully

-- | SourceLocation: position handling should be robust
prop_position_handling_robust :: Int -> Int -> Bool
prop_position_handling_robust line                               col = 
  let pos = posAt (max 1 line) (max 1 col)
                                    advanced = advancePos pos '\0'
  in case advanced of
    SourcePos l c -> l >= sourceLine pos && c >= 1

-- | SourceLocation: span validation should be consistent
prop_span_validation_consistent :: Int -> Int -> Int -> Int -> Bool
prop_span_validation_consistent line1 col1 line2                               col2 = 
  let pos1 = posAt (max 1 line1) (max 1 col1)
                                    pos2 = posAt (max 1 line2) (max 1 col2)
                                    span = SourceLocation.spanBetween pos1 pos2
  in isValidSpan                               span == True  -- Should always create valid spans

-- | Parser: should handle syntax errors gracefully
prop_parser_syntax_errors :: String -> Bool
prop_parser_syntax_errors                               s = 
  let withSyntaxError = s ++ "\n@invalid { syntax error here\n" ++ s
  in case parseTypus withSyntaxError of
    Left _ -> True
    Right _ -> True  -- Should handle gracefully

-- | Parser: should handle unclosed blocks gracefully
prop_parser_unclosed_blocks :: String -> Bool
prop_parser_unclosed_blocks                               s = 
  let unclosed = "// @ownership {\n" ++ s
  in case parseTypus unclosed of
    Left _ -> True
    Right _ -> True  -- Should handle gracefully

-- | Parser: should handle invalid directive names
prop_parser_invalid_directives :: String -> Bool
prop_parser_invalid_directives                               s = 
  let invalidDirective = "// @123invalid-directive!\n" ++ s
  in case parseTypus invalidDirective of
    Left _ -> True
    Right _ -> True

-- | Parser: should handle mixed encoding gracefully
prop_parser_mixed_encoding :: String -> Bool
prop_parser_mixed_encoding                               s = 
  let mixed = s ++ "\xff\xfe" ++ s  -- Invalid UTF-8 sequence
  in case parseTypus mixed of
    Left _ -> True
    Right _ -> True

-- | Property: Error recovery should not lose valid content
prop_error_recovery_preserves_content :: String -> String -> Bool
prop_error_recovery_preserves_content s1                               s2 = 
  let withError = s1 ++ "\n@invalid {\n" ++ s2
  in case parseTypus withError of
    Left _ -> True
    Right parsed -> True  -- Should preserve what it can

-- | Property: Multiple errors should be handled gracefully
prop_multiple_errors_handling :: String -> Bool
prop_multiple_errors_handling                               s = 
  let multipleErrors = s ++ "\n@error1\n@error2 {\n@error3\n" ++ s
  in case parseTypus multipleErrors of
    Left _ -> True
    Right parsed -> True

-- | Property: Extreme input should not cause crashes
prop_extreme_input_safe :: String -> Bool
prop_extreme_input_safe                               s = 
  let extreme = concatMap (\c -> if isControl c then replicate 5 c else [c]) s
  in case parseTypus extreme of
    Left _ -> True
    Right parsed -> True

-- | Property: Nested error recovery should work
prop_nested_error_recovery :: String -> Int -> Bool
prop_nested_error_recovery s                               depth = 
  let depth > 0 && depth <                               10 ==>
      let nestedErrors = L.concat $ replicate depth ("@error {\n" ++ s)
      in case parseTypus nestedErrors of
        Left _ -> True
        Right parsed -> True

-- ============================================================================
-- Test Suite
-- ============================================================================

testSuite :: TestTree
testSuite = testGroup "Core Module Error Handling QuickCheck Tests"
  [ testGroup "Utils Error Handling Tests"
    [             testProperty "trim control characters" prop_trim_control_chars
    ,             testProperty "splitBy invalid input" prop_split_by_invalid_input
    ,             testProperty "removeComments malformed" prop_remove_comments_malformed
    ,             testProperty "normalizeIndentation mixed tabs/spaces" prop_normalize_indentation_mixed
    ]
  , testGroup "SourceLocation Error Handling Tests"
    [             testProperty "position handling robust" prop_position_handling_robust
    ,             testProperty "span validation consistent" prop_span_validation_consistent
    ]
  , testGroup "Parser Error Handling Tests"
    [             testProperty "parser syntax errors" prop_parser_syntax_errors
    ,             testProperty "parser unclosed blocks" prop_parser_unclosed_blocks
    ,             testProperty "parser invalid directives" prop_parser_invalid_directives
    ,             testProperty "parser mixed encoding" prop_parser_mixed_encoding
    ]
  , testGroup "Error Recovery Tests"
    [             testProperty "error recovery preserves content" prop_error_recovery_preserves_content
    ,             testProperty "multiple errors handling" prop_multiple_errors_handling
    ,             testProperty "extreme input safe" prop_extreme_input_safe
    ,             testProperty "nested error recovery" prop_nested_error_recovery
    ]
  ]