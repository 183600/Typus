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

# 修复测试中的TypeRef Show实例格式
def fix_type_ref_show_format_in_tests(content):
    # 匹配 "assertEqual \"([^\"]+)\" \"TypeRef \{refName = \"([^\"]+)\", refArgs = \\\[\\\]\}\" ty" 并替换为 "assertEqual \"\\1\" \"\\2\" (show ty)"
    content = re.sub(r'assertEqual "([^"]+)" "TypeRef \{refName = "([^"]+)", refArgs = \\\[\\\]\}" ty', r'assertEqual "\1" "\2" (show ty)', content)
    
    # 匹配 "assertEqual \"([^\"]+)\" \"TypeRef \{refName = \"([^\"]+)\", refArgs = \\\[([^\\\]]+)\\\]\}\" ty" 并替换为 "assertEqual \"\\1\" \"\\2[\\3]\" (show ty)"
    content = re.sub(r'assertEqual "([^"]+)" "TypeRef \{refName = "([^"]+)", refArgs = \\\[([^\\\]]+)\\\]\}" ty', r'assertEqual "\1" "\2[\3]" (show ty)', content)
    
    return content

# 主函数
def main():
    for filepath in test_files:
        if os.path.exists(filepath):
            print(f"修复文件: {filepath}")
            content = read_file(filepath)
            content = fix_type_ref_show_format_in_tests(content)
            write_file(filepath, content)
        else:
            print(f"文件不存在: {filepath}")

if __name__ == "__main__":
    main()
