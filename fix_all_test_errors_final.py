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
            # 使用更精确的正则表达式
            pattern1 = r'TypusFile defaultFileDirectives \[\] \[\] \[\]\s*\]'
            replacement1 = 'TypusFile defaultFileDirectives [] [] []'
            content = re.sub(pattern1, replacement1, content, flags=re.MULTILINE)
            
            # 错误2: 修复 T.null 与 String 类型不匹配
            # 查找所有 T.null 的使用并替换为 null
            content = content.replace('T.null', 'null')
            
            # 错误3: 修复 TypeErrorToCompilerError 类型签名
            # 移除错误的辅助函数定义
            pattern3 = r'-- 辅助函数：将 TypeError 转换为 CompilerError\nTypeErrorToCompilerError :: TypeError -> CompilerError\nTypeErrorToCompilerError \(TypeError loc msg\) = CompilerError loc msg TypeErrorSeverity\n'
            content = re.sub(pattern3, '', content, flags=re.MULTILINE)
            
            # 错误4: 修复 parseTypus 返回类型不匹配
            # 查找 parseTypus 的使用并修复
            pattern4 = r'result = case parseTypus code of'
            replacement4 = 'result = case parseTypus (T.pack code) of'
            content = re.sub(pattern4, replacement4, content, flags=re.MULTILINE)
            
            # 错误5: 修复 formatCompilerErrors 参数类型不匹配
            # 查找 formatCompilerErrors 的使用并修复
            pattern5 = r'formatCompilerErrors \[parseErr\]'
            replacement5 = 'formatCompilerErrors [SyntaxError (Located (SourcePos "" 0 0)) parseErr]'
            content = re.sub(pattern5, replacement5, content, flags=re.MULTILINE)
            
            # 错误6: 修复 parse error on input '<-' 
            # 查找可能的 do 块缺少问题
            pattern6 = r'(\w+)\s*=\s*(\w+)\s*<-'
            replacement6 = r'\1 = do\n    \2 <-'
            content = re.sub(pattern6, replacement6, content, flags=re.MULTILINE)
            
            # 如果内容有变化，写回文件
            if content != old_content:
                with open(file_path, 'w', encoding='utf-8') as f:
                    f.write(content)
                
        except Exception as e:
            print(f"  错误: {e}")

if __name__ == "__main__":
    fix_all_test_errors()
    print("修复完成!")