#!/usr/bin/env python3
import os
import re
import glob

def fix_import_errors():
    """修复导入语句中的多余逗号错误"""
    test_dir = "/home/runner/work/Typus/Typus/test/Test/Unit"
    pattern = os.path.join(test_dir, "*.hs")
    
    files_modified = 0
    
    for file_path in glob.glob(pattern):
        try:
            with open(file_path, 'r', encoding='utf-8') as f:
                content = f.read()
            
            # 修复导入语句中的多余逗号
            # 匹配模式: import Module (function1), function2)
            old_pattern = r'import\s+([^\s]+(?:\.[^\s]+)*)\s+\(([^)]+)\),\s*\)'
            new_pattern = r'import \1 (\2)'
            
            new_content = re.sub(old_pattern, new_pattern, content)
            
            if new_content != content:
                with open(file_path, 'w', encoding='utf-8') as f:
                    f.write(new_content)
                print(f"Fixed import in {file_path}")
                files_modified += 1
                
        except Exception as e:
            print(f"Error processing {file_path}: {e}")
    
    print(f"Total files modified: {files_modified}")

if __name__ == "__main__":
    fix_import_errors()