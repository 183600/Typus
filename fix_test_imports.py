#!/usr/bin/env python3
import os
import re
import glob

def fix_test_imports(file_path):
    """Fix imports in a test file"""
    with open(file_path, 'r') as f:
        content = f.read()
    
    # Add FlexibleInstances language extension if not present
    if 'Arbitrary String where' in content and 'FlexibleInstances' not in content:
        # Find the language extensions section
        language_pattern = r'({-\#\s*LANGUAGE\s+[^}]*})'
        match = re.search(language_pattern, content)
        if match:
            # Add FlexibleInstances to existing LANGUAGE pragma
            existing_extensions = match.group(1)
            if 'FlexibleInstances' not in existing_extensions:
                new_extensions = existing_extensions.rstrip('}') + ', FlexibleInstances}'
                content = content.replace(existing_extensions, new_extensions)
        else:
            # Add new LANGUAGE pragma at the top
            module_pattern = r'(module\s+[^\s]+\s+\([^)]*\)\s+where)'
            module_match = re.search(module_pattern, content)
            if module_match:
                module_line = module_match.group(1)
                new_module_line = "{-# LANGUAGE FlexibleInstances #-}\n" + module_line
                content = content.replace(module_line, new_module_line)
    
    # Add property to QuickCheck imports if not present
    if 'import Test.Tasty.QuickCheck' in content and 'property' not in content:
        import_pattern = r'(import\s+Test\.Tasty\.QuickCheck\s+\([^)]*\))'
        match = re.search(import_pattern, content)
        if match:
            existing_import = match.group(1)
            if 'property' not in existing_import:
                new_import = existing_import.rstrip(')') + ', property)'
                content = content.replace(existing_import, new_import)
        else:
            # Add property to simple import
            content = content.replace('import Test.Tasty.QuickCheck', 'import Test.Tasty.QuickCheck (property)')
    
    with open(file_path, 'w') as f:
        f.write(content)
    print(f"Fixed imports in {file_path}")

# Get all test files
test_files = glob.glob('test/Test/Unit/*.hs')
for file_path in test_files:
    fix_test_imports(file_path)