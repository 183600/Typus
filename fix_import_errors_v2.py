#!/usr/bin/env python3
"""
修复测试文件中的Data.List导入问题
将错误的 `import Data.List (L.function)` 修复为正确的导入方式
"""

import os
import re
import sys

def fix_imports_in_file(file_path):
    """修复单个文件中的导入问题"""
    try:
        with open(file_path, 'r', encoding='utf-8') as f:
            content = f.read()
        
        original_content = content
        
        # 1. 修复错误的导入语句：import Data.List (L.function) 
        #    改为两行：
        #    import qualified Data.List as L
        #    import Data.List (function)
        
        # 查找所有包含L.的Data.List导入
        pattern = r'import Data\.List \(([^)]+)\)'
        matches = re.findall(pattern, content)
        
        if matches:
            for match in matches:
                # 检查是否包含L.开头的函数
                if 'L.' in match:
                    # 提取所有函数名
                    all_functions = [f.strip() for f in match.split(',')]
                    l_functions = []  # 带L.前缀的函数
                    regular_functions = []  # 不带L.前缀的函数
                    
                    for func in all_functions:
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
                        new_imports.append('import qualified Data.List as L')
                        new_imports.append(f'import Data.List ({", ".join(l_functions)})')
                    if regular_functions:
                        new_imports.append(f'import Data.List ({", ".join(regular_functions)})')
                    
                    new_import_text = '\n'.join(new_imports)
                    
                    # 替换旧的导入语句
                    content = content.replace(old_import, new_import_text)
        
        # 2. 处理 qualified Data.Text as T 中的错误导入
        #    import qualified Data.Text as T (pack, L.isInfixOf)
        pattern2 = r'import qualified Data\.Text as T \(([^)]+)\)'
        matches2 = re.findall(pattern2, content)
        if matches2:
            for match in matches2:
                # 检查是否包含 L. 开头的函数
                if 'L.' in match:
                    # 移除 L. 前缀
                    fixed_functions = re.sub(r'L\.(\w+)', r'\1', match)
                    old_import = f'import qualified Data.Text as T ({match})'
                    new_import = f'import qualified Data.Text as T ({fixed_functions})'
                    content = content.replace(old_import, new_import)
        
        # 3. 处理类型签名中的错误限定符
        #    L.isInfixOf :: String -> String -> Bool
        #    改为：
        #    isInfixOf :: String -> String -> Bool
        content = re.sub(r'^L\.(\w+) ::', r'\1 ::', content, flags=re.MULTILINE)
        
        # 只有内容发生变化时才写入文件
        if content != original_content:
            with open(file_path, 'w', encoding='utf-8') as f:
                f.write(content)
            print(f"Fixed imports in: {file_path}")
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
                        # 检查是否包含错误的导入模式
                        if re.search(r'import Data\.List \(L\.\w+', content):
                            files_to_fix.append(file_path)
                        elif re.search(r'import qualified Data\.Text as T \([^)]*L\.', content):
                            files_to_fix.append(file_path)
                        elif re.search(r'^L\.\w+ ::', content, flags=re.MULTILINE):
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
        if fix_imports_in_file(file_path):
            fixed_count += 1
    
    print(f"Fixed {fixed_count} files")

if __name__ == "__main__":
    main()