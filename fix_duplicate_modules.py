import os
import re

def fix_duplicate_modules(file_path):
    """Fix duplicate module declarations"""
    with open(file_path, 'r') as f:
        content = f.read()
    
    # Find all module declarations
    module_pattern = r"module\s+[^\s]+\s+where"
    modules = re.findall(module_pattern, content)
    
    # If there are multiple module declarations, keep only the first one
    if len(modules) > 1:
        # Split content by module declarations
        parts = re.split(module_pattern, content)
        
        # Keep the first module declaration and the rest of the file after the last module declaration
        new_content = parts[0] + modules[0] + " where\n\n" + parts[-1]
        
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
            if fix_duplicate_modules(file_path):
                fixed_count += 1
    
    print("Fixed duplicate module declarations in " + str(fixed_count) + " test files")

if __name__ == "__main__":
    fix_all_test_files()