#!/usr/bin/env python3
import os
import re
import sys

def fix_all_remaining_errors():
    """Fix all remaining import errors"""
    test_dir = "test/Test/Unit"
    fixed_count = 0
    
    for filename in os.listdir(test_dir):
        if filename.endswith(".hs"):
            file_path = os.path.join(test_dir, filename)
            
            try:
                with open(file_path, 'r') as f:
                    content = f.read()
                
                # Fix Arbitrary(.., (==>)) pattern
                content = re.sub(r'Arbitrary\(\.\.\.,\s*\(\s*==>\s*\)\s*\)', 'Arbitrary(..)', content)
                
                # Fix (===, (==>)) pattern
                content = re.sub(r'\(\s*===,\s*\(\s*==>\s*\)\s*\)', '(===), (==>)', content)
                
                # Fix duplicate property
                content = re.sub(r'property,\s*Property', 'Property', content)
                content = re.sub(r'Property,\s*property', 'Property', content)
                
                # Fix duplicate testProperty
                content = re.sub(r'testProperty,\s*testProperty', 'testProperty', content)
                
                # Fix duplicate (==>)
                content = re.sub(r'\(\s*==>\s*\),\s*\(\s*==>\s*\)', '(==>)', content)
                
                # Fix import Test.Tasty.QuickCheck lines
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
                
                fixed_count += 1
            except Exception as e:
                print(f"Error processing {file_path}: {e}")
    
    print(f"Processed {fixed_count} files")

if __name__ == "__main__":
    fix_all_remaining_errors()