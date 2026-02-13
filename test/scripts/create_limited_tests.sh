#!/bin/bash

# 创建一个精简的测试文件，只包含200个测试
echo "创建精简的测试文件..."

# 提取前200个测试属性
head -n 800 test/Test/Unit/NewCompactQuickCheckTests.hs > test/Test/Unit/LimitedQuickCheckTests.hs

# 添加文件结尾
echo "
-- ============================================================================
-- 组合所有测试
-- ============================================================================

-- | 组合所有测试
limitedQuickCheckTests :: TestTree
limitedQuickCheckTests = testGroup \"Limited QuickCheck Tests\"
  [ testGroup \"Core Utils Tests\" 
      [ testProperty \"trim idempotent\" prop_trim_idempotent
      , testProperty \"splitBy length\" prop_split_by_length
      , testProperty \"remove line comments preserves strings\" prop_remove_line_comments_preserves_strings
      , testProperty \"remove comments balanced\" prop_remove_comments_balanced
      , testProperty \"is complete string literal\" prop_is_complete_string_literal
      , testProperty \"breakOn correctness\" prop_break_on_correctness
      , testProperty \"safe process string safe\" prop_safe_process_string_safe
      , testProperty \"splitBy comma consistency\" prop_split_by_comma_consistency
      , testProperty \"normalize indentation relative\" prop_normalize_indentation_relative
      , testProperty \"trim empty\" prop_trim_empty
      , testProperty \"trim whitespace\" prop_trim_whitespace
      , testProperty \"splitBy empty\" prop_split_by_empty
      , testProperty \"splitBy comma empty\" prop_split_by_comma_empty
      , testProperty \"remove comments idempotent\" prop_remove_comments_idempotent
      , testProperty \"trim mixed whitespace\" prop_trim_mixed_whitespace
      , testProperty \"is problematic unclosed string\" prop_is_problematic_unclosed_string
      , testProperty \"remove line comments multiline\" prop_remove_line_comments_multiline
      , testProperty \"splitBy collapsed fold\" prop_split_by_collapsed_fold
      , testProperty \"normalize indentation empty lines\" prop_normalize_indentation_empty_lines
      , testProperty \"breakOn empty\" prop_break_on_empty
      , testProperty \"safe process string empty\" prop_safe_process_string_empty
      , testProperty \"is complete string literal empty\" prop_is_complete_string_literal_empty
      , testProperty \"trim never increases\" prop_trim_never_increases
      , testProperty \"trim all whitespace\" prop_trim_all_whitespace
      , testProperty \"trim tab space\" prop_trim_tab_space
      , testProperty \"splitBy comma collapsed\" prop_split_by_comma_collapsed
      , testProperty \"normalize indentation preserves nonempty\" prop_normalize_indentation_preserves_nonempty
      , testProperty \"is complete string literal escaped\" prop_is_complete_string_literal_escaped
      , testProperty \"splitBy consecutive\" prop_split_by_consecutive
      , testProperty \"normalize indentation empty\" prop_normalize_indentation_empty
      , testProperty \"remove comments single line\" prop_remove_comments_single_line
      , testProperty \"trim newlines\" prop_trim_newlines
      , testProperty \"remove line comments end\" prop_remove_line_comments_end
      , testProperty \"isRight property\" prop_is_right_property
      , testProperty \"splitBy comma numbers\" prop_split_by_comma_numbers
      , testProperty \"remove comments protect strings\" prop_remove_comments_protect_strings
      , testProperty \"normalize indentation tabs\" prop_normalize_indentation_tabs
      , testProperty \"splitBy special\" prop_split_by_special
      , testProperty \"is valid char ascii\" prop_is_valid_char_ascii
      , testProperty \"trim split interaction\" prop_trim_split_interaction
      ]
  , testGroup \"Parser Tests\"
      [ testProperty \"parse identifier basic\" prop_parse_identifier_basic
      , testProperty \"parse number\" prop_parse_number
      , testProperty \"parse string literal\" prop_parse_string_literal
      , testProperty \"parse skip comments\" prop_parse_skip_comments
      , testProperty \"parse whitespace\" prop_parse_whitespace
      , testProperty \"parse keywords\" prop_parse_keywords
      , testProperty \"parse operators\" prop_parse_operators
      , testProperty \"parse parentheses\" prop_parse_parentheses
      , testProperty \"parse array\" prop_parse_array
      , testProperty \"parse function def\" prop_parse_function_def
      , testProperty \"parse variable decl\" prop_parse_variable_decl
      , testProperty \"parse expression\" prop_parse_expression
      , testProperty \"parse if statement\" prop_parse_if_statement
      , testProperty \"parse while loop\" prop_parse_while_loop
      , testProperty \"parse return statement\" prop_parse_return_statement
      , testProperty \"parse assignment\" prop_parse_assignment
      , testProperty \"parse multiline\" prop_parse_multiline
      , testProperty \"parse nested structures\" prop_parse_nested_structures
      , testProperty \"parse error recovery\" prop_parse_error_recovery
      , testProperty \"parse unicode\" prop_parse_unicode
      , testProperty \"parse long identifier\" prop_parse_long_identifier
      , testProperty \"parse escape sequences\" prop_parse_escape_sequences
      , testProperty \"parse empty input\" prop_parse_empty_input
      , testProperty \"parse partial input\" prop_parse_partial_input
      , testProperty \"parse code after comment\" prop_parse_code_after_comment
      , testProperty \"parse complex expression\" prop_parse_complex_expression
      , testProperty \"parse function call\" prop_parse_function_call
      , testProperty \"parse struct definition\" prop_parse_struct_definition
      , testProperty \"parse type annotation\" prop_parse_type_annotation
      , testProperty \"parse import\" prop_parse_import
      , testProperty \"parse export\" prop_parse_export
      ]
  , testGroup \"Compiler Core Tests\"
      [ testProperty \"compile basic\" prop_compile_basic
      , testProperty \"ir generation consistent\" prop_ir_generation_consistent
      , testProperty \"ir optimization invariant\" prop_ir_optimization_invariant
      , testProperty \"type check correct\" prop_type_check_correct
      , testProperty \"type error detection\" prop_type_error_detection
      , testProperty \"ownership check basic\" prop_ownership_check_basic
      , testProperty \"ownership transfer\" prop_ownership_transfer
      , testProperty \"go ast generation\" prop_go_ast_generation
      , testProperty \"go lexer tokenize\" prop_go_lexer_tokenize
      , testProperty \"error collection\" prop_error_collection
      , testProperty \"error recovery\" prop_error_recovery
      , testProperty \"compiler idempotent\" prop_compiler_idempotent
      , testProperty \"ir serialization\" prop_ir_serialization
      , testProperty \"type inference\" prop_type_inference
      , testProperty \"ownership cycle detection\" prop_ownership_cycle_detection
      , testProperty \"compiler memory usage\" prop_compiler_memory_usage
      , testProperty \"ir optimization effective\" prop_ir_optimization_effective
      , testProperty \"type check generics\" prop_type_check_generics
      , testProperty \"ownership borrow check\" prop_ownership_borrow_check
      , testProperty \"go ast integrity\" prop_go_ast_integrity
      , testProperty \"lexer position info\" prop_lexer_position_info
      , testProperty \"error messages useful\" prop_error_messages_useful
      , testProperty \"compiler concurrent safe\" prop_compiler_concurrent_safe
      , testProperty \"ir control flow\" prop_ir_control_flow
      , testProperty \"type dependency analysis\" prop_type_dependency_analysis
      , testProperty \"ownership lifetime analysis\" prop_ownership_lifetime_analysis
      ]
  , testGroup \"Dependency Analysis Tests\"
      [ testProperty \"dependency analysis basic\" prop_dependency_analysis_basic
      , testProperty \"dependency graph build\" prop_dependency_graph_build
      , testProperty \"cycle detection\" prop_cycle_detection
      , testProperty \"no cycle detection\" prop_no_cycle_detection
      , testProperty \"dependency topological sort\" prop_dependency_topological_sort
      , testProperty \"type inference dependencies\" prop_type_inference_dependencies
      , testProperty \"ast node dependencies\" prop_ast_node_dependencies
      , testProperty \"dependency analysis performance\" prop_dependency_analysis_performance
      , testProperty \"dependency transitivity\" prop_dependency_transitivity
      , testProperty \"dependency completeness\" prop_dependency_completeness
      , testProperty \"dependency consistency\" prop_dependency_consistency
      , testProperty \"type system dependencies\" prop_type_system_dependencies
      , testProperty \"dependency incremental update\" prop_dependency_incremental_update
      , testProperty \"dependency caching\" prop_dependency_caching
      , testProperty \"dependency parallel\" prop_dependency_parallel
      , testProperty \"dependency error handling\" prop_dependency_error_handling
      , testProperty \"dependency modular\" prop_dependency_modular
      , testProperty \"dependency visualization\" prop_dependency_visualization
      , testProperty \"dependency statistics\" prop_dependency_statistics
      , testProperty \"dependency optimization\" prop_dependency_optimization
      , testProperty \"dependency filtering\" prop_dependency_filtering
      , testProperty \"dependency merging\" prop_dependency_merging
      , testProperty \"dependency comparison\" prop_dependency_comparison
      , testProperty \"dependency export\" prop_dependency_export
      , testProperty \"dependency import\" prop_dependency_import
      , testProperty \"dependency validation\" prop_dependency_validation
      ]
  , testGroup \"Ownership Analysis Tests\"
      [ testProperty \"ownership analysis basic\" prop_ownership_analysis_basic
      , testProperty \"ownership transfer detection\" prop_ownership_transfer_detection
      , testProperty \"borrow checking\" prop_borrow_checking
      , testProperty \"lifetime analysis\" prop_lifetime_analysis
      , testProperty \"move semantics\" prop_move_semantics
      , testProperty \"shared references\" prop_shared_references
      , testProperty \"ownership error detection\" prop_ownership_error_detection
      , testProperty \"ownership concurrent safe\" prop_ownership_concurrent_safe
      , testProperty \"ownership performance\" prop_ownership_performance
      , testProperty \"ownership graph build\" prop_ownership_graph_build
      , testProperty \"ownership rules validation\" prop_ownership_rules_validation
      , testProperty \"ownership completeness\" prop_ownership_completeness
      , testProperty \"ownership consistency\" prop_ownership_consistency
      , testProperty \"ownership incremental\" prop_ownership_incremental
      , testProperty \"ownership caching\" prop_ownership_caching
      , testProperty \"ownership parallel\" prop_ownership_parallel
      , testProperty \"ownership error handling\" prop_ownership_error_handling
      , testProperty \"ownership modular\" prop_ownership_modular
      , testProperty \"ownership visualization\" prop_ownership_visualization
      , testProperty \"ownership statistics\" prop_ownership_statistics
      , testProperty \"ownership optimization\" prop_ownership_optimization
      , testProperty \"ownership filtering\" prop_ownership_filtering
      , testProperty \"ownership merging\" prop_ownership_merging
      , testProperty \"ownership comparison\" prop_ownership_comparison
      , testProperty \"ownership export\" prop_ownership_export
      ]
  , testGroup \"Error Handling Tests\"
      [ testProperty \"error handler basic\" prop_error_handler_basic
      , testProperty \"error collection completeness\" prop_error_collection_completeness
      , testProperty \"error recovery\" prop_error_recovery
      , testProperty \"error messages useful\" prop_error_messages_useful
      , testProperty \"error context\" prop_error_context
      , testProperty \"error severity\" prop_error_severity
      , testProperty \"error suggestions\" prop_error_suggestions
      , testProperty \"error handling concurrent\" prop_error_handling_concurrent
      , testProperty \"error handling performance\" prop_error_handling_performance
      , testProperty \"error classification\" prop_error_classification
      , testProperty \"error aggregation\" prop_error_aggregation
      , testProperty \"error filtering\" prop_error_filtering
      , testProperty \"error sorting\" prop_error_sorting
      , testProperty \"error deduplication\" prop_error_deduplication
      , testProperty \"error statistics\" prop_error_statistics
      , testProperty \"error reporting\" prop_error_reporting
      , testProperty \"error export\" prop_error_export
      , testProperty \"error import\" prop_error_import
      , testProperty \"error validation\" prop_error_validation
      , testProperty \"error repair suggestions\" prop_error_repair_suggestions
      , testProperty \"error incremental update\" prop_error_incremental_update
      , testProperty \"error caching\" prop_error_caching
      , testProperty \"error parallel\" prop_error_parallel
      , testProperty \"error modular\" prop_error_modular
      ]
  ]" >> test/Test/Unit/LimitedQuickCheckTests.hs

# 添加模块声明
sed -i '1i{-# LANGUAGE OverloadedStrings #-}\n{-# LANGUAGE ScopedTypeVariables #-}\nmodule Test.Unit.LimitedQuickCheckTests where\n\nimport Test.Tasty\nimport Test.Tasty.QuickCheck\nimport Test.Tasty.HUnit\nimport qualified Utils as U\nimport Data.List (isInfixOf, isPrefixOf, isSuffixOf, intercalate)\nimport Data.Char (isSpace, isLetter, isDigit)\nimport Data.Maybe (isJust, isNothing)\nimport Data.Either (isLeft, isRight)\nimport qualified Data.Map as Map\nimport qualified Data.Set as Set\n' test/Test/Unit/LimitedQuickCheckTests.hs

echo "精简测试文件创建完成"