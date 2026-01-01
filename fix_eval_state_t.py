#!/usr/bin/env python3
import os
import re

def fix_eval_state_t(file_path):
    """Fix evalStateT calls to handle ExceptT"""
    try:
        with open(file_path, 'r') as f:
            content = f.read()
        
        # Replace evalStateT (runCrossAnalysis ...) with runExceptT (evalStateT (runCrossAnalysis ...))
        content = re.sub(r'result <- evalStateT \(runCrossAnalysis ([^)]+)\) ([a-zA-Z]+)', 
                        r'result <- runExceptT (evalStateT (runCrossAnalysis \1) \2)', content)
        
        with open(file_path, 'w') as f:
            f.write(content)
        
        return True
    except Exception as e:
        print(f"Error fixing {file_path}: {e}")
        return False

def main():
    file_path = "/home/runner/work/Typus/Typus/test/Test/Unit/AnalyzerCrossAnalysisSpec.hs"
    if fix_eval_state_t(file_path):
        print(f"Fixed {file_path}")
    else:
        print(f"Failed to fix {file_path}")

if __name__ == "__main__":
    main()