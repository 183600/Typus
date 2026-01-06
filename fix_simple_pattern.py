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
                
                # Fix the problematic pattern
                if "Arbitrary(.., (==>))" in content:
                    content = content.replace("Arbitrary(.., (==>))", "Arbitrary(..)")
                    
                    with open(file_path, 'w') as f:
                        f.write(content)
                    
                    print(f"Fixed {file_path}")
                    fixed_count += 1
            except Exception as e:
                print(f"Error processing {file_path}: {e}")
    
    print(f"Fixed {fixed_count} files")

if __name__ == "__main__":
    main()