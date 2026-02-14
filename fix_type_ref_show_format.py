#!/usr/bin/env python3
import re
import os

# 需要修复的测试文件
test_files = [
    "test/Test/Unit/NewDependentTypesTestSuite.hs",
    "test/Test/Unit/NewOwnershipTestSuite.hs",
    "test/Test/Unit/NewCompilerIntegrationTestSuite.hs",
    "test/Test/Unit/NewQuickCheckPropertiesTestSuite.hs"
]

# 读取文件内容
def read_file(filepath):
    with open(filepath, 'r') as f:
        return f.read()

# 写入文件内容
def write_file(filepath, content):
    with open(filepath, 'w') as f:
        f.write(content)

# 修复TypeRef的Show实例格式
def fix_type_ref_show_format(content):
    # 匹配 "TypeRef {refName = \"([^\"]+)\", refArgs = []}" 并替换为 "\1"
    content = re.sub(r'TypeRef \{refName = "([^"]+)", refArgs = \[\]\}', r'\1', content)
    
    # 匹配 "TypeRef {refName = \"([^\"]+)\", refArgs = \[([^\]]+)\]}" 并替换为 "\1[\2]"
    content = re.sub(r'TypeRef \{refName = "([^"]+)", refArgs = \[([^\]]+)\]\}', r'\1[\2]', content)
    
    # 处理嵌套的TypeRef
    # 匹配 "TypeRef {refName = \"([^\"]+)\", refArgs = \[TypeRef \{refName = \"([^\"]+)\", refArgs = \[\]\}\]}" 并替换为 "\1[\2]"
    content = re.sub(r'TypeRef \{refName = "([^"]+)", refArgs = \[TypeRef \{refName = "([^"]+)", refArgs = \[\]\}\]\}', r'\1[\2]', content)
    
    # 处理多个参数的TypeRef
    # 匹配 "TypeRef {refName = \"([^\"]+)\", refArgs = \[TypeRef \{refName = \"([^\"]+)\", refArgs = \[\]\},TypeRef \{refName = \"([^\"]+)\", refArgs = \[\]\}\]}" 并替换为 "\1[\2,\3]"
    content = re.sub(r'TypeRef \{refName = "([^"]+)", refArgs = \[TypeRef \{refName = "([^"]+)", refArgs = \[\]\},TypeRef \{refName = "([^"]+)", refArgs = \[\]\}\]\}', r'\1[\2,\3]', content)
    
    return content

# 主函数
def main():
    for filepath in test_files:
        if os.path.exists(filepath):
            print(f"修复文件: {filepath}")
            content = read_file(filepath)
            content = fix_type_ref_show_format(content)
            write_file(filepath, content)
        else:
            print(f"文件不存在: {filepath}")

if __name__ == "__main__":
    main()