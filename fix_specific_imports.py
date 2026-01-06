#!/usr/bin/env python3
import os
import re
import sys

def fix_import_line(file_path):
    """Fix QuickCheck import line in a Haskell file"""
    try:
        with open(file_path, 'r') as f:
            content = f.read()
        
        # Pattern to find Test.Tasty.QuickCheck imports
        import_pattern = r'import\s+Test\.Tasty\.QuickCheck\s*\(([^)]*)\)'
        
        def fix_import(match):
            import_list = match.group(1)
            
            # Remove duplicate (==>) entries
            import_list = re.sub(r'\(\s*==>\s*\)', '(==>)', import_list)
            import_list = re.sub(r'\(\s*==>\s*\),\s*\(\s*==>\s*\)', '(==>)', import_list)
            
            # Fix malformed (===, (==>)) patterns
            import_list = re.sub(r'\(\s*===,\s*\(\s*==>\s*\)\s*\)', '(===), (==>)', import_list)
            import_list = re.sub(r'\(\s*===,\s*\(\s*==>\s*\)\s*\)', '(===), (==>)', import_list)
            
            # Remove duplicate (==>) after fixing
            parts = [p.strip() for p in import_list.split(',')]
            seen = set()
            unique_parts = []
            
            for part in parts:
                # Normalize for comparison
                normalized = re.sub(r'\s+', '', part)
                if normalized == '(==>)' and '(==>)' in seen:
                    continue
                if normalized not in seen:
                    seen.add(normalized)
                    unique_parts.append(part)
            
            return f"import Test.Tasty.QuickCheck ({', '.join(unique_parts)})"
        
        content = re.sub(import_pattern, fix_import, content)
        
        # Fix any remaining malformed patterns
        content = re.sub(r'\(\s*===,\s*\(\s*==>\s*\)\s*\)', '(===), (==>)', content)
        content = re.sub(r'\(\s*==>,\s*\(\s*==>\s*\)\s*\)', '(==>)', content)
        
        with open(file_path, 'w') as f:
            f.write(content)
        
        return True
    except Exception as e:
        print(f"Error processing {file_path}: {e}")
        return False

def main():
    # Fix specific files that had errors
    files_to_fix = [
        "test/Test/Unit/AdditionalQuickCheckTests2025.hs",
        "test/Test/Unit/CompilerCoreQuickCheckTests.hs",
        "test/Test/Unit/DependentTypesCoreQuickCheckTests.hs",
        "test/Test/Unit/ErrorHandlerCoreQuickCheckTests.hs",
        "test/Test/Unit/NewAdvancedSourceLocationQuickCheckSpec.hs",
        "test/Test/Unit/NewCabalTestCasesSpec.hs"
    ]
    
    fixed_count = 0
    for file_path in files_to_fix:
        if os.path.exists(file_path):
            if fix_import_line(file_path):
                print(f"Fixed {file_path}")
                fixed_count += 1
    
    print(f"Fixed {fixed_count} files")

if __name__ == "__main__":
    main()