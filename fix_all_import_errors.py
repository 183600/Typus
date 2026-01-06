#!/usr/bin/env python3
import os
import re
import sys

def fix_all_import_errors(file_path):
    """Fix all import errors in a Haskell file"""
    try:
        with open(file_path, 'r') as f:
            content = f.read()
        
        # Fix Arbitrary(.., (==>)) pattern
        content = re.sub(r'Arbitrary\(\.\.\.,\s*\(\s*==>\s*\)\s*\)', 'Arbitrary(..)', content)
        
        # Fix import Test.Tasty.QuickCheck lines with various issues
        import_pattern = r'import\s+Test\.Tasty\.QuickCheck\s*\(([^)]*)\)'
        
        def fix_import(match):
            import_list = match.group(1)
            
            # Split and clean parts
            parts = [p.strip() for p in import_list.split(',')]
            seen = set()
            unique_parts = []
            
            for part in parts:
                # Skip empty parts
                if not part:
                    continue
                
                # Normalize for comparison
                normalized = re.sub(r'\s+', '', part)
                
                # Skip duplicates
                if normalized in seen:
                    continue
                
                # Add to unique parts
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
            if fix_all_import_errors(file_path):
                fixed_count += 1
    
    print(f"Processed {fixed_count} files")

if __name__ == "__main__":
    main()