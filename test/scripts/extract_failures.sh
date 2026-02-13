#!/bin/bash

# 提取失败测试的详细信息
echo "=== prop_normalize_indentation_relative ==="
grep -A 10 "prop_normalize_indentation_relative:.*FAIL" test_output.txt

echo -e "\n=== prop_is_problematic_unclosed_string ==="
grep -A 10 "prop_is_problematic_unclosed_string:.*FAIL" test_output.txt

echo -e "\n=== prop_is_complete_string_literal ==="
grep -A 10 "prop_is_complete_string_literal:.*FAIL" test_output.txt

echo -e "\n=== normalize indentation empty lines ==="
grep -A 10 "normalize indentation empty lines:.*FAIL" test_output.txt

echo -e "\n=== normalizeIndentation code block ==="
grep -A 10 "normalizeIndentation code block:.*FAIL" test_output.txt

echo -e "\n=== normalizeIndentation nested ==="
grep -A 10 "normalizeIndentation nested:.*FAIL" test_output.txt

echo -e "\n=== prop_remove_comments_single_line ==="
grep -A 10 "prop_remove_comments_single_line:.*FAIL" test_output.txt