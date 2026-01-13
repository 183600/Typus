#!/usr/bin/env python3
"""
批量修复ConcurrentSafetyQuickCheckSpec.hs中的whenFail类型错误
"""

def fix_when_fail_errors():
    # 读取文件内容
    with open('/home/runner/work/Typus/Typus/test/Test/Unit/ConcurrentSafetyQuickCheckSpec.hs', 'r') as f:
        content = f.read()
    
    # 替换所有的whenFail调用
    import re
    
    # 使用正则表达式替换 whenFail ("...") 为 whenFail (print ("..."))
    pattern = r'whenFail \("([^"]+)"\)'
    replacement = r'whenFail (print ("\1"))'
    
    new_content = re.sub(pattern, replacement, content)
    
    # 写回文件
    with open('/home/runner/work/Typus/Typus/test/Test/Unit/ConcurrentSafetyQuickCheckSpec.hs', 'w') as f:
        f.write(new_content)
    
    print("修复完成！")

if __name__ == "__main__":
    fix_when_fail_errors()