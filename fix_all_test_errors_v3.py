#!/usr/bin/env python3
"""
修复测试文件中的所有错误
"""

import os
import re
import glob

def fix_all_test_errors():
    """修复测试文件中的所有错误"""
    
    # 查找所有测试文件
    test_files = []
    for root, dirs, files in os.walk('.'):
        for file in files:
            if file.endswith('.hs') and 'Test' in root:
                test_files.append(os.path.join(root, file))
    
    # 修复所有文件
    for file_path in test_files:
        print(f"处理文件: {file_path}")
        try:
            with open(file_path, 'r', encoding='utf-8') as f:
                content = f.read()
            
            old_content = content
            
            # 错误1: parseTypusFile 不存在，替换为 parseTypus
            content = content.replace('parseTypusFile', 'parseTypus')
            
            # 错误2: 修复导入语句中的 parseTypusFile
            content = content.replace('import Parser (parseTypusFile)', 'import Parser (parseTypus)')
            
            # 错误3: TypusFile defaultFileDirectives [] [] [] 后面有多余的 ]
            # 使用更精确的正则表达式
            pattern3 = r'TypusFile defaultFileDirectives \[\] \[\] \[\](\s*\[\])+(\s*\[\])+'
            replacement3 = 'TypusFile defaultFileDirectives [] [] []'
            content = re.sub(pattern3, replacement3, content, flags=re.MULTILINE)
            
            # 错误4: 修复 T.null 与 String 类型不匹配
            # 如果变量是 String 类型，使用 null 而不是 T.null
            # 先查找变量声明
            var_declarations = re.findall(r'(\w+)\s*::\s*String', content)
            for var in var_declarations:
                pattern4 = f'T\\.null\\s*{var}'
                replacement4 = f'null {var}'
                content = re.sub(pattern4, replacement4, content)
            
            # 错误5: 修复 T.null 与普通变量不匹配
            # 查找格式化函数返回 String 的情况
            pattern5 = r'(\w+)\s*=\s*formatErrorMessages.*\n.*T\\.null\\s*\1'
            replacement5 = r'\1 = formatErrorMessages\1\n  in property (not (null \1))'
            content = re.sub(pattern5, replacement5, content, flags=re.MULTILINE | re.DOTALL)
            
            # 错误6: 修复简单的 T.null 用法
            # 如果没有导入 Data.Text，使用 null
            if 'import qualified Data.Text as T' not in content:
                content = content.replace('T.null', 'null')
            
            # 如果内容有变化，写回文件
            if content != old_content:
                with open(file_path, 'w', encoding='utf-8') as f:
                    f.write(content)
                
        except Exception as e:
            print(f"  错误: {e}")

if __name__ == "__main__":
    fix_all_test_errors()
    print("修复完成!")
