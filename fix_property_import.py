#!/usr/bin/env python3
import os
import re

def main():
    test_dir = "test/Test/Unit"
    fixed_count = 0
    
    for filename in os.listdir(test_dir):
        if filename.endswith(".hs"):
            file_path = os.path.join(test_dir, filename)
            
            try:
                with open(file_path, 'r') as f:
                    content = f.read()
                
                # Check if file uses 'property' but doesn't import it
                if 'property True' in content or 'property False' in content:
                    # Check if property is already imported
                    if 'import Test.Tasty.QuickCheck' in content:
                        if 'property' not in content:
                            # Add property to the import
                            import_pattern = r'(import\s+Test\.Tasty\.QuickCheck\s*\([^)]*))'
                            def add_property(match):
                                import_str = match.group(1)
                                if import_str.endswith('('):
                                    return import_str + 'property)'
                                else:
                                    return import_str + ', property)'
                            
                            content = re.sub(import_pattern, add_property, content)
                            
                            with open(file_path, 'w') as f:
                                f.write(content)
                            
                            print(f"Fixed {file_path}")
                            fixed_count += 1
            except Exception as e:
                print(f"Error processing {file_path}: {e}")
    
    print(f"Fixed {fixed_count} files")

if __name__ == "__main__":
    main()