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
            
            # 错误1: TypusFile defaultFileDirectives [] [] [] 后面有多余的 ]
            # 匹配 TypusFile defaultFileDirectives [] [] [] 后面跟着任意数量的 [] ]
            pattern1 = r'TypusFile defaultFileDirectives \[\] \[\] \[\](\s*\[\])*\]'
            replacement1 = 'TypusFile defaultFileDirectives [] [] []'
            content = re.sub(pattern1, replacement1, content, flags=re.MULTILINE)
            
            # 错误2: 修复 T.null 未导入的问题
            # 检查是否有 import qualified Data.Text as T
            if 'import qualified Data.Text as T' not in content and 'T.null' in content:
                # 在导入部分添加
                import_pattern = r'(import\s+.*\n)+'
                if re.search(import_pattern, content):
                    # 在第一个导入后添加
                    content = re.sub(
                        r'(import\s+.*\n)',
                        r'\1import qualified Data.Text as T\n',
                        content,
                        count=1
                    )
                    print(f"  添加了 Data.Text 导入")
            
            # 错误3: 修复 T.null 与 String 类型不匹配
            # 如果 formatted 是 String 类型，使用 null 而不是 T.null
            pattern3 = r'T\.null\s+(\w+)\s*(?=--.*String|::\s*String)'
            replacement3 = r'null \1'
            content = re.sub(pattern3, replacement3, content, flags=re.MULTILINE)
            
            # 错误4: 修复 parseTypus 函数不存在的问题
            # 检查是否有 parseTypus 函数调用
            if 'parseTypus' in content and 'parseTypusFile' not in content:
                content = content.replace('parseTypus', 'parseTypusFile')
                print(f"  将 parseTypus 替换为 parseTypusFile")
            
            # 错误5: 修复 hasLocation 函数类型不匹配
            pattern5 = r'property\s*\(\s*all\s+hasLocation\s+(\w+)\s*\)'
            replacement5 = r'property (all (hasLocation . unLoc) \1)'
            content = re.sub(pattern5, replacement5, content, flags=re.MULTILINE)
            
            # 错误6: 修复 TypeError 与 CompilerError 类型不匹配
            # 在所有需要 CompilerError 的地方，将 TypeError 转换为 CompilerError
            pattern6a = r'calculateErrorStatistics\s+(\w+)'
            replacement6a = r'calculateErrorStatistics (map TypeErrorToCompilerError \1)'
            content = re.sub(pattern6a, replacement6a, content, flags=re.MULTILINE)
            
            pattern6b = r'filterErrorsBySeverity\s+(\w+)\s+(\d+)'
            replacement6b = r'filterErrorsBySeverity (map TypeErrorToCompilerError \1) \2'
            content = re.sub(pattern6b, replacement6b, content, flags=re.MULTILINE)
            
            pattern6c = r'sortErrorsByLocation\s+(\w+)'
            replacement6c = r'sortErrorsByLocation (map TypeErrorToCompilerError \1)'
            content = re.sub(pattern6c, replacement6c, content, flags=re.MULTILINE)
            
            pattern6d = r'highlightErrorInSource\s+(\w+)\s+(\w+)'
            replacement6d = r'highlightErrorInSource \1 (TypeErrorToCompilerError \2)'
            content = re.sub(pattern6d, replacement6d, content, flags=re.MULTILINE)
            
            # 添加辅助函数
            if 'TypeErrorToCompilerError' in content and 'TypeErrorToCompilerError' not in content.replace('TypeErrorToCompilerError', ''):
                # 在文件中添加辅助函数
                helper_function = """
-- 辅助函数：将 TypeError 转换为 CompilerError
TypeErrorToCompilerError :: TypeError -> CompilerError
TypeErrorToCompilerError (TypeError loc msg) = CompilerError loc msg TypeErrorSeverity
"""
                # 在最后一个导入后添加
                import_section = re.findall(r'(import\s+.*\n)+', content)
                if import_section:
                    last_import = import_section[-1]
                    end_pos = content.rfind(last_import) + len(last_import)
                    content = content[:end_pos] + helper_function + "\n" + content[end_pos:]
                    print(f"  添加了 TypeErrorToCompilerError 辅助函数")
            
            # 如果内容有变化，写回文件
            if content != old_content:
                with open(file_path, 'w', encoding='utf-8') as f:
                    f.write(content)
                
        except Exception as e:
            print(f"  错误: {e}")

if __name__ == "__main__":
    fix_all_test_errors()
    print("修复完成!")