import os
import re

def fix_property_functions(file_path):
    """Fix property functions to return Property instead of Bool"""
    with open(file_path, 'r') as f:
        content = f.read()
    
    # Skip if file doesn't have property functions
    if "prop_" not in content:
        return False
    
    # Replace property functions to return Property
    pattern = r"(prop_\w+ :: .+ -> Property\s+prop_\w+ .+ =\s+)(.+)(\s+in\s+.+)"
    
    def replacement(match):
        prefix = match.group(1)
        body = match.group(2)
        suffix = match.group(3)
        
        # If the body is a simple boolean expression, wrap it in 'property'
        if "property" not in body and "counterexample" not in body and "===" not in body:
            body = f"property $ {body}"
        
        return prefix + body + suffix
    
    new_content = re.sub(pattern, replacement, content, flags=re.DOTALL)
    
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
            if fix_property_functions(file_path):
                fixed_count += 1
    
    print("Fixed property functions in " + str(fixed_count) + " test files")

if __name__ == "__main__":
    fix_all_test_files()