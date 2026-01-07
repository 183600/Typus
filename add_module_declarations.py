import os
import re

def fix_test_file(file_path):
    """Fix a test file with proper structure"""
    with open(file_path, 'r') as f:
        content = f.read()
    
    # Extract module name from file path
    module_name = file_path.split('/')[-1][:-3]  # Remove .hs extension
    module_name = "Test.Unit." + module_name
    
    # Check if module declaration exists
    if not content.startswith("module"):
        # Add module declaration at the beginning
        new_content = f"module {module_name} where\n\n" + content
        
        # Write back to the file
        with open(file_path, 'w') as f:
            f.write(new_content)
        
        return True
    
    return False

def fix_all_test_files():
    """Fix all test files"""
    test_dir = "/home/runner/work/Typus/Typus/test/Test/Unit"
    fixed_count = 0
    
    for filename in os.listdir(test_dir):
        if filename.endswith(".hs"):
            file_path = os.path.join(test_dir, filename)
            if fix_test_file(file_path):
                fixed_count += 1
    
    print("Added module declarations to " + str(fixed_count) + " test files")

if __name__ == "__main__":
    fix_all_test_files()