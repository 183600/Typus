#!/usr/bin/env python3
"""
修复剩余的导入和语法错误
"""

import os
import re
import sys

def fix_specific_errors_in_file(file_path):
    """修复特定文件中的错误"""
    try:
        with open(file_path, 'r', encoding='utf-8') as f:
            content = f.read()
        
        original_content = content
        
        # 1. 修复 T.L.isInfixOf 错误
        content = re.sub(r'T\.L\.isInfixOf', r'L.isInfixOf', content)
        
        # 2. 修复 L.isInfixOf 在绑定位置的问题
        #    L.isInfixOf needle haystack = ...
        #    改为：
        #    isInfixOf needle haystack = ...
        content = re.sub(r'^(\s*)L\.(\w+)\s+::', r'\1\2 ::', content, flags=re.MULTILINE)
        content = re.sub(r'^(\s*)L\.(\w+)\s+', r'\1\2 ', content, flags=re.MULTILINE)
        
        # 3. 修复 infix 4 `L.isInfixOf` 的问题
        content = re.sub(r'infix\s+\d+\s+`L\.(\w+)`', r'infix \1', content)
        
        # 4. 修复 import Data.List (sort, L.length) 的问题
        #    改为两行：
        #    import qualified Data.List as L
        #    import Data.List (sort)
        pattern = r'import Data\.List \(([^)]*L\.\w+[^)]*)\)'
        matches = re.findall(pattern, content)
        if matches:
            for match in matches:
                # 分割函数列表
                functions = [f.strip() for f in match.split(',')]
                l_functions = []  # 带L.前缀的函数
                regular_functions = []  # 不带L.前缀的函数
                
                for func in functions:
                    if func.startswith('L.'):
                        # 移除L.前缀
                        l_functions.append(func[2:])
                    else:
                        regular_functions.append(func)
                
                # 构建新的导入语句
                old_import = f'import Data.List ({match})'
                
                # 创建新的导入语句
                new_imports = []
                if l_functions:
                    # 检查是否已经有qualified import
                    if not re.search(r'import qualified Data\.List as L', content):
                        new_imports.append('import qualified Data.List as L')
                    new_imports.append(f'import Data.List ({", ".join(l_functions)})')
                if regular_functions:
                    new_imports.append(f'import Data.List ({", ".join(regular_functions)})')
                
                new_import_text = '\n'.join(new_imports)
                
                # 替换旧的导入语句
                content = content.replace(old_import, new_import_text)
        
        # 5. 修复 case 表达式的语法错误
        #    case map getErrorSeverity errors of
        #    改为：
        #    case map getErrorSeverity errors of
        
        # 6. 修复 parse error on input 'case' 的问题
        #    这通常是因为缩进问题导致的
        
        # 只有内容发生变化时才写入文件
        if content != original_content:
            with open(file_path, 'w', encoding='utf-8') as f:
                f.write(content)
            print(f"Fixed errors in: {file_path}")
            return True
        return False
    except Exception as e:
        print(f"Error processing {file_path}: {e}")
        return False

def find_files_to_fix(directory):
    """查找需要修复的文件"""
    files_to_fix = []
    for root, dirs, files in os.walk(directory):
        for file in files:
            if file.endswith('.hs'):
                file_path = os.path.join(root, file)
                try:
                    with open(file_path, 'r', encoding='utf-8') as f:
                        content = f.read()
                        # 检查是否包含需要修复的模式
                        if (re.search(r'T\.L\.isInfixOf', content) or
                            re.search(r'^\s*L\.\w+\s+::', content, flags=re.MULTILINE) or
                            re.search(r'infix\s+\d+\s+`L\.\w+`', content) or
                            re.search(r'import Data\.List \([^)]*L\.\w+[^)]*\)', content)):
                            files_to_fix.append(file_path)
                except Exception as e:
                    print(f"Error reading {file_path}: {e}")
    return files_to_fix

def main():
    """主函数"""
    test_dir = "test"
    if not os.path.exists(test_dir):
        print(f"Directory {test_dir} does not exist")
        sys.exit(1)
    
    files_to_fix = find_files_to_fix(test_dir)
    print(f"Found {len(files_to_fix)} files to fix")
    
    fixed_count = 0
    for file_path in files_to_fix:
        if fix_specific_errors_in_file(file_path):
            fixed_count += 1
    
    print(f"Fixed {fixed_count} files")

if __name__ == "__main__":
    main()