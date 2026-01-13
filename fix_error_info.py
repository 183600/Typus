#!/usr/bin/env python3
"""
批量修复ErrorHandlerBasicFunctionsSpec.hs中的Error和Info引用
"""

def fix_error_info_references():
    # 读取文件内容
    with open('/home/runner/work/Typus/Typus/test/Test/Unit/ErrorHandlerBasicFunctionsSpec.hs', 'r') as f:
        content = f.read()
    
    # 替换所有的Error为C.Error，但保留createError函数名
    import re
    
    # 先处理createError，临时替换它
    content = content.replace('createError', 'TEMP_createError')
    
    # 替换所有Error为C.Error
    content = re.sub(r'\bError\b', 'C.Error', content)
    
    # 恢复createError
    content = content.replace('TEMP_createError', 'createError')
    
    # 替换所有Info为C.Info
    content = re.sub(r'\bInfo\b', 'C.Info', content)
    
    # 写回文件
    with open('/home/runner/work/Typus/Typus/test/Test/Unit/ErrorHandlerBasicFunctionsSpec.hs', 'w') as f:
        f.write(content)
    
    print("修复完成！")

if __name__ == "__main__":
    fix_error_info_references()