import os
import re

def fix_test_file_module(file_path):
    """Fix the module declaration in a test file"""
    with open(file_path, 'r') as f:
        content = f.read()
    
    # Extract module name from file path
    module_name = file_path.split('/')[-1][:-3]  # Remove .hs extension
    module_name = "Test.Unit." + module_name
    
    # Check if module declaration exists
    if "module " in content:
        # Replace the module declaration
        pattern = r"module\s+([^\s]+)"
        new_content = re.sub(pattern, f"module {module_name}", content)
        
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
            if fix_test_file_module(file_path):
                fixed_count += 1
    
    print("Fixed module declarations in " + str(fixed_count) + " test files")

if __name__ == "__main__":
    fix_all_test_files()