#!/usr/bin/env python3
import re
import os
import sys

def fix_all_test_errors():
    """修复所有测试文件中的错误"""
    test_dir = "/home/runner/work/Typus/Typus/test/Test/Unit"
    
    # 遍历所有测试文件
    for root, dirs, files in os.walk(test_dir):
        for file_name in files:
            if file_name.endswith(".hs"):
                file_path = os.path.join(root, file_name)
                try:
                    with open(file_path, 'r') as f:
                        content = f.read()
                    
                    modified = False
                    
                    # 1. 修复布尔表达式到属性的转换
                    if "Property" in content and "True" in content:
                        # 修复各种情况下的 True -> property True
                        patterns = [
                            (r'(\s+)Left _ -> True\s*$', r'\1Left _ -> property True'),
                            (r'(\s+)Right _ -> True\s*$', r'\1Right _ -> property True'),
                            (r'(\s+)else True\s*$', r'\1else property True'),
                            (r'(\s+)then True\s*$', r'\1then property True'),
                            (r'(\s+)if (.+) then True else True\s*$', r'\1if \2 then property True else property True'),
                            (r'(\s+)in (.+?) && (.+?)$', r'\1in property $ \2 && \3'),
                            (r'(\s+)in (.+?) >= (.+?)$', r'\1in property $ \2 >= \3'),
                            (r'(\s+)in (.+?) <= (.+?)$', r'\1in property $ \2 <= \3'),
                        ]
                        
                        for pattern, replacement in patterns:
                            if re.search(pattern, content):
                                content = re.sub(pattern, replacement, content)
                                modified = True
                    
                    # 2. 修复比较运算符
                    if "===" in content:
                        # 在条件语句中修复 ===
                        content = re.sub(r'(\s+)(then|else|if) (.+?) === (.+?)\s*$', r'\1\2 \3 == \4', content)
                        modified = True
                    
                    # 3. 修复CodeBlock构造函数
                    if "CodeBlock" in content:
                        # 修复只有两个参数的CodeBlock调用
                        content = re.sub(r'CodeBlock ([^,]+) ([^)]+)\)', r'CodeBlock \1 \2 (SourceSpan (SourcePos 1 1 0) (SourcePos 1 1 0))', content)
                        modified = True
                    
                    # 4. 修复TypusFile构造函数
                    if "TypusFile" in content:
                        # 修复只有三个参数的TypusFile调用
                        content = re.sub(r'TypusFile ([^,]+) ([^,]+) ([^)]+)\)', r'TypusFile \1 \2 \3 []', content)
                        modified = True
                    
                    # 5. 修复语法错误
                    if ",," in content:
                        # 修复多余的逗号
                        content = re.sub(r',,\)', r')', content)
                        content = re.sub(r',,\s*\)', r')', content)
                        modified = True
                    
                    # 6. 修复where子句的缩进
                    if "where" in content:
                        # 修复where子句的缩进
                        content = re.sub(r'(\s+)where\s*\n(\s+)(\S)', r'\1where\n\2  \3', content)
                        modified = True
                    
                    # 7. 添加必要的导入
                    if "Property" in content and "property" not in content:
                        # 添加property函数的导入
                        if "import Test.Tasty.QuickCheck" in content:
                            content = re.sub(r'(import Test.Tasty.QuickCheck \([^(]+\))', r'\1, property)', content)
                            modified = True
                    
                    if modified:
                        with open(file_path, 'w') as f:
                            f.write(content)
                        print(f"Fixed errors in {file_path}")
                
                except Exception as e:
                    print(f"Error processing {file_path}: {e}")

if __name__ == "__main__":
    fix_all_test_errors()
