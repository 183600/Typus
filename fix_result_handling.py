#!/usr/bin/env python3
import os
import re

def fix_result_handling(file_path):
    """Fix result handling for Either type"""
    try:
        with open(file_path, 'r') as f:
            content = f.read()
        
        # Replace case result of patterns
        content = re.sub(r'case result of:\s*\[\] ->', r'case result of\n    Left _ -> assertBool "Should not fail with exception" False\n    Right [] ->', content)
        content = re.sub(r'\[CrossAnalyzerError', r'Right [CrossAnalyzerError', content)
        content = re.sub(r'_ -> assertBool "Should return a single error" False', r'Right _ -> assertBool "Should return a single error" False', content)
        
        with open(file_path, 'w') as f:
            f.write(content)
        
        return True
    except Exception as e:
        print(f"Error fixing {file_path}: {e}")
        return False

def main():
    file_path = "/home/runner/work/Typus/Typus/test/Test/Unit/AnalyzerCrossAnalysisSpec.hs"
    if fix_result_handling(file_path):
        print(f"Fixed {file_path}")
    else:
        print(f"Failed to fix {file_path}")

if __name__ == "__main__":
    main()