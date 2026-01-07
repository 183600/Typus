import os
import re

def fix_arbitrary_instances(file_path):
    """Fix Arbitrary instances for SourcePos and SourceSpan"""
    with open(file_path, 'r') as f:
        content = f.read()
    
    # Skip if file doesn't have Arbitrary instances
    if "instance Arbitrary SourcePos" not in content:
        return False
    
    # Replace the Arbitrary instance for SourcePos
    old_instance = """-- Arbitrary instance for SourcePos
instance Arbitrary SourcePos where
  arbitrary = do
    line <- choose (1, 100)
    column <- choose (1, 100)
    return $ SourcePos line column"""
    
    new_instance = """-- Arbitrary instance for SourcePos
instance Arbitrary SourcePos where
  arbitrary = do
    line <- choose (1, 100)
    column <- choose (1, 100)
    offset <- choose (0, 1000)
    return $ SourcePos line column offset"""
    
    if old_instance in content:
        content = content.replace(old_instance, new_instance)
        
        # Write back to the file
        with open(file_path, 'w') as f:
            f.write(content)
        
        return True
    
    return False

def fix_all_test_files():
    """Fix all test files"""
    test_dir = "/home/runner/work/Typus/Typus/test/Test/Unit"
    fixed_count = 0
    
    for filename in os.listdir(test_dir):
        if filename.endswith(".hs"):
            file_path = os.path.join(test_dir, filename)
            if fix_arbitrary_instances(file_path):
                fixed_count += 1
    
    print("Fixed Arbitrary instances in " + str(fixed_count) + " test files")

if __name__ == "__main__":
    fix_all_test_files()