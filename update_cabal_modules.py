import os
import re

def find_all_test_modules():
    """Find all test modules in the test directory"""
    test_modules = set()
    test_dir = "/home/runner/work/Typus/Typus/test/Test/Unit"
    
    for filename in os.listdir(test_dir):
        if filename.endswith(".hs"):
            # Convert filename to module name
            module_name = filename[:-3]  # Remove .hs extension
            test_modules.add(f"Test.Unit.{module_name}")
    
    return sorted(test_modules)

def update_cabal_file():
    """Update the cabal file with all test modules"""
    cabal_path = "/home/runner/work/Typus/Typus/typus.cabal"
    
    # Find all test modules
    test_modules = find_all_test_modules()
    
    # Read the cabal file
    with open(cabal_path, 'r') as f:
        content = f.read()
    
    # Find the test suite section
    test_suite_match = re.search(r'test-suite typus-test.*?other-modules:(.*?)(?=\n\n|\n    [a-z]|\Z)', content, re.DOTALL)
    
    if test_suite_match:
        # Replace the other-modules section
        new_other_modules = "other-modules:\n"
        for module in test_modules:
            new_other_modules += f"            {module},\n"
        
        # Replace in the content
        new_content = content[:test_suite_match.start(1)] + new_other_modules + content[test_suite_match.end(1):]
        
        # Write back to the file
        with open(cabal_path, 'w') as f:
            f.write(new_content)
        
        print(f"Updated cabal file with {len(test_modules)} test modules")
    else:
        print("Could not find test-suite section in cabal file")

if __name__ == "__main__":
    update_cabal_file()