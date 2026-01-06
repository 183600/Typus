#!/usr/bin/env python3
import os

def fix_dependent_type_errors(file_path):
    """Fix DependentType parsing errors in test files"""
    with open(file_path, 'r') as f:
        content = f.read()
    
    # Fix parseDependentType return value pattern
    content = content.replace('Right depType ->', 'Right (depType, _) ->')
    
    with open(file_path, 'w') as f:
        f.write(content)
    print(f"Fixed DependentType errors in {file_path}")

def main():
    # Fix specific files
    files_to_fix = [
        "test/Test/Unit/CabalDependentTypesQuickCheckSpec.hs",
    ]
    
    for file_path in files_to_fix:
        if os.path.exists(file_path):
            fix_dependent_type_errors(file_path)

if __name__ == "__main__":
    main()