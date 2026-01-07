import os
import re

def fix_module_declaration(file_path):
    """Fix module declaration with duplicate 'where'"""
    with open(file_path, 'r') as f:
        content = f.read()
    
    # Find module declaration with duplicate 'where'
    module_pattern = r"(module\s+[^\s]+\s+where)\s+where"
    
    # Fix the duplicate 'where'
    new_content = re.sub(module_pattern, r"\1", content)
    
    # Write back to the file if changed
    if new_content != content:
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
            if fix_module_declaration(file_path):
                fixed_count += 1
    
    print("Fixed module declarations in " + str(fixed_count) + " test files")

if __name__ == "__main__":
    fix_all_test_files()