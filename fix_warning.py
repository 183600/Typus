#!/usr/bin/env python3
"""
批量修复ErrorHandlerBasicFunctionsSpec.hs中的Warning引用
"""

def fix_warning_references():
    # 读取文件内容
    with open('/home/runner/work/Typus/Typus/test/Test/Unit/ErrorHandlerBasicFunctionsSpec.hs', 'r') as f:
        content = f.read()
    
    # 替换所有的Warning为C.Warning，但保留createWarning函数名
    import re
    
    # 使用正则表达式替换 Warning 为 C.Warning，但不匹配 createWarning
    pattern = r'\bWarning\b'
    replacement = 'C.Warning'
    
    # 先处理createWarning，临时替换它
    content = content.replace('createWarning', 'TEMP_createWarning')
    
    # 然后替换所有Warning
    content = re.sub(pattern, replacement, content)
    
    # 最后恢复createWarning
    content = content.replace('TEMP_createWarning', 'createWarning')
    
    # 写回文件
    with open('/home/runner/work/Typus/Typus/test/Test/Unit/ErrorHandlerBasicFunctionsSpec.hs', 'w') as f:
        f.write(content)
    
    print("修复完成！")

if __name__ == "__main__":
    fix_warning_references()