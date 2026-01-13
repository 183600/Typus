#!/usr/bin/env python3
"""
批量修复CliBasicFunctionsSpec.hs中的parseArgs引用
"""

def fix_parse_args_references():
    # 读取文件内容
    with open('/home/runner/work/Typus/Typus/test/Test/Unit/CliBasicFunctionsSpec.hs', 'r') as f:
        content = f.read()
    
    # 替换所有的parseArgs为parseArgsTest
    content = content.replace('parseArgs args', 'parseArgsTest args')
    
    # 写回文件
    with open('/home/runner/work/Typus/Typus/test/Test/Unit/CliBasicFunctionsSpec.hs', 'w') as f:
        f.write(content)
    
    print("修复完成！")

if __name__ == "__main__":
    fix_parse_args_references()