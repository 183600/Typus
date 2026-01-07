#!/usr/bin/env python3
import os
import re

def fix_specific_errors(file_path):
    """修复特定的语法错误"""
    try:
        with open(file_path, 'r') as f:
            content = f.read()
        
        original_content = content
        
        # 修复 AdvancedOwnershipQuickCheckSpec.hs 中的缩进错误
        if file_path.endswith('AdvancedOwnershipQuickCheckSpec.hs'):
            content = re.sub(r'^    , CrossFunctionMove', r'      , CrossFunctionMove', content, flags=re.MULTILINE)
        
        # 修复 AdvancedCompilerQuickCheckSpec.hs 中的 tests :: TestTree 缩进错误
        if file_path.endswith('AdvancedCompilerQuickCheckSpec.hs'):
            content = re.sub(r'^tests :: TestTree', r'tests :: TestTree', content, flags=re.MULTILINE)
        
        # 修复 AdditionalIntegratedCompilerQuickCheckSpec.hs 中的 result <- run 缩进错误
        if file_path.endswith('AdditionalIntegratedCompilerQuickCheckSpec.hs'):
            content = re.sub(r'^  result <- run', r'        result <- run', content, flags=re.MULTILINE)
        
        # 修复 VerbositySpec.hs 中的 restore original 缩进错误
        if file_path.endswith('VerbositySpec.hs'):
            content = re.sub(r'^        restore original =', r'          restore original =', content, flags=re.MULTILINE)
        
        if content != original_content:
            with open(file_path, 'w') as f:
                f.write(content)
            print(f"Fixed specific errors in {file_path}")
            return True
        return False
    except Exception as e:
        print(f"Error processing {file_path}: {e}")
        return False

def main():
    test_dir = "/home/runner/work/Typus/Typus/test/Test/Unit"
    fixed_count = 0
    
    # 修复特定的文件
    specific_files = [
        "AdvancedOwnershipQuickCheckSpec.hs",
        "AdvancedCompilerQuickCheckSpec.hs",
        "AdditionalIntegratedCompilerQuickCheckSpec.hs",
        "VerbositySpec.hs"
    ]
    
    for file_name in specific_files:
        file_path = os.path.join(test_dir, file_name)
        if os.path.exists(file_path):
            if fix_specific_errors(file_path):
                fixed_count += 1
    
    print(f"Fixed specific errors in {fixed_count} files")

if __name__ == "__main__":
    main()