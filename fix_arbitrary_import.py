#!/usr/bin/env python3
import os
import re
import sys

def fix_arbitrary_import(file_path):
    """Fix Arbitrary import with malformed syntax"""
    try:
        with open(file_path, 'r') as f:
            content = f.read()
        
        # Fix Arbitrary(.., (==>)) pattern
        content = re.sub(r'Arbitrary\(\.\.\.,\s*\(\s*==>\s*\)\s*\)', 'Arbitrary(..)', content)
        
        # Fix any remaining duplicate (==>) in imports
        import_pattern = r'import\s+Test\.Tasty\.QuickCheck\s*\(([^)]*)\)'
        
        def fix_import(match):
            import_list = match.group(1)
            
            # Remove duplicate (==>) entries
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
        
        with open(file_path, 'w') as f:
            f.write(content)
        
        return True
    except Exception as e:
        print(f"Error processing {file_path}: {e}")
        return False

def main():
    test_dir = "test/Test/Unit"
    fixed_count = 0
    
    for filename in os.listdir(test_dir):
        if filename.endswith(".hs"):
            file_path = os.path.join(test_dir, filename)
            if fix_arbitrary_import(file_path):
                fixed_count += 1
    
    print(f"Processed {fixed_count} files")

if __name__ == "__main__":
    main()