import os
import re

def add_arbitrary_instances(file_path):
    """Add Arbitrary instances for SourcePos and SourceSpan to test files"""
    with open(file_path, 'r') as f:
        content = f.read()
    
    # Skip if file already has Arbitrary instances
    if "instance Arbitrary SourcePos" in content:
        return False
    
    # Find the import section
    import_end = content.find("\n\n")
    if import_end == -1:
        import_end = content.find("\n--")
    
    if import_end == -1:
        return False
    
    # Add Arbitrary instances after imports
    instances = "\n-- Arbitrary instance for SourcePos\n"
    instances += "instance Arbitrary SourcePos where\n"
    instances += "  arbitrary = do\n"
    instances += "    line <- choose (1, 100)\n"
    instances += "    column <- choose (1, 100)\n"
    instances += "    return $ SourcePos line column\n\n"
    instances += "-- Arbitrary instance for SourceSpan\n"
    instances += "instance Arbitrary SourceSpan where\n"
    instances += "  arbitrary = do\n"
    instances += "    start <- arbitrary\n"
    instances += "    end <- arbitrary\n"
    instances += "    return $ SourceSpan start end\n"
    
    # Insert the instances
    new_content = content[:import_end] + instances + content[import_end:]
    
    # Write back to the file
    with open(file_path, 'w') as f:
        f.write(new_content)
    
    return True

def fix_all_test_files():
    """Fix all test files"""
    test_dir = "/home/runner/work/Typus/Typus/test/Test/Unit"
    fixed_count = 0
    
    for filename in os.listdir(test_dir):
        if filename.endswith(".hs"):
            file_path = os.path.join(test_dir, filename)
            if add_arbitrary_instances(file_path):
                fixed_count += 1
    
    print("Added Arbitrary instances to " + str(fixed_count) + " test files")

if __name__ == "__main__":
    fix_all_test_files()