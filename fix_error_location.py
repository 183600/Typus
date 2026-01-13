#!/usr/bin/env python3
"""
批量修复ErrorHandlerBasicFunctionsSpec.hs中的ErrorLocation引用
"""

def fix_error_location_references():
    # 读取文件内容
    with open('/home/runner/work/Typus/Typus/test/Test/Unit/ErrorHandlerBasicFunctionsSpec.hs', 'r') as f:
        content = f.read()
    
    # 替换所有的ErrorLocation为C.ErrorLocation
    import re
    
    # 替换所有ErrorLocation为C.ErrorLocation
    content = re.sub(r'\bErrorLocation\b', 'C.ErrorLocation', content)
    
    # 写回文件
    with open('/home/runner/work/Typus/Typus/test/Test/Unit/ErrorHandlerBasicFunctionsSpec.hs', 'w') as f:
        f.write(content)
    
    print("修复完成！")

if __name__ == "__main__":
    fix_error_location_references()