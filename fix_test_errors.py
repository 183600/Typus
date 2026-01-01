#!/usr/bin/env python3
import re
import sys
import os

def fix_pattern_in_file(file_path, pattern, replacement):
    """Fix a pattern in a file"""
    try:
        with open(file_path, 'r') as f:
            content = f.read()
        
        if re.search(pattern, content):
            new_content = re.sub(pattern, replacement, content)
            with open(file_path, 'w') as f:
                f.write(new_content)
            print(f"Fixed {file_path}")
            return True
        return False
    except Exception as e:
        print(f"Error fixing {file_path}: {e}")
        return False

def main():
    # Common pattern fixes
    fixes = [
        # Fix runExceptT pattern matching
        (r'(\s+)result <- runExceptT \(evalStateT (.+) (.+)\)\s+case result of:\s+\[\] ->',
         r'\1result <- runExceptT (evalStateT \2 \3)\n\1case result of:\n\1  Left _ -> assertBool "Should not fail" False\n\1  Right [] ->'),
        
        # Fix evalState with TypeInference
        (r'evalState \(([^)]+)\) newTypeInferenceState', r'runTypeInference (\1)'),
        
        # Fix getDetailedAnalysisSummary call
        (r'let summary <- getDetailedAnalysisSummary ([^)]+)', r'let summary = getDetailedAnalysisSummary \1'),
        
        # Fix String to Text conversions
        (r'SVarDecl "([^"]+)"', r'SVarDecl (T.pack "\1")'),
        (r'SimpleT "([^"]+)"', r'SimpleT (T.pack "\1")'),
    ]
    
    # Find all test files
    test_dir = "/home/runner/work/Typus/Typus/test/Test/Unit"
    for filename in os.listdir(test_dir):
        if filename.endswith(".hs"):
            file_path = os.path.join(test_dir, filename)
            for pattern, replacement in fixes:
                fix_pattern_in_file(file_path, pattern, replacement)

if __name__ == "__main__":
    main()
