#!/usr/bin/env python3
import os
import re

def fix_initial_analyzer_state(file_path):
    """Fix initialAnalyzerState to newIntegratedAnalyzer True True in a file"""
    try:
        with open(file_path, 'r') as f:
            content = f.read()
        
        # Replace initialAnalyzerState with newIntegratedAnalyzer True True
        content = re.sub(r'initialAnalyzerState', 'newIntegratedAnalyzer True True', content)
        
        with open(file_path, 'w') as f:
            f.write(content)
        
        return True
    except Exception as e:
        print(f"Error fixing {file_path}: {e}")
        return False

def main():
    test_dir = "/home/runner/work/Typus/Typus/test"
    fixed_count = 0
    
    for root, dirs, files in os.walk(test_dir):
        for file in files:
            if file.endswith('.hs'):
                file_path = os.path.join(root, file)
                if fix_initial_analyzer_state(file_path):
                    fixed_count += 1
    
    print(f"Fixed {fixed_count} files")

if __name__ == "__main__":
    main()