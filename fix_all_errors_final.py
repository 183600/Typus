#!/usr/bin/env python3
import os
import re
import glob

def fix_all_remaining_errors():
    """修复所有剩余的语法错误"""
    test_dir = "/home/runner/work/Typus/Typus/test/Test/Unit"
    pattern = os.path.join(test_dir, "*.hs")
    
    files_modified = 0
    
    for file_path in glob.glob(pattern):
        try:
            with open(file_path, 'r', encoding='utf-8') as f:
                content = f.read()
            
            original_content = content
            new_content = content
            
            # 1. 修复do块中的缩进错误
            # 匹配: arbitrary = do\nn <- choose (0, 20)
            # 修复为: arbitrary = do\n  n <- choose (0, 20)
            new_content = re.sub(r'(\bdo\n)\s+(\w+\s+<-)', r'\1  \2', new_content)
            
            # 2. 修复列表中的缩进错误
            # 匹配: [ return ""\n, return "\0\1\2\xFF"
            # 修复为: [ return ""\n  , return "\0\1\2\xFF"
            new_content = re.sub(r'(\[\s+return\s+[^\]]+\n)(\s+,)', r'\1  \2', new_content)
            
            # 3. 修复testGroup中的列表错误
            # 匹配: , testCase "test" $ do
            # 修复为: , testCase "test" $ do
            new_content = re.sub(r'(\n\s+),\s+(testCase)', r'\1  , \2', new_content)
            
            # 4. 修复instance Arbitrary中的列表错误
            # 匹配: , pure Ownership
            # 修复为: , pure Ownership
            new_content = re.sub(r'(\n\s+),\s+(pure|return)', r'\1  , \2', new_content)
            
            # 5. 修复property定义错误
            # 匹配: prop_test = property $ not (hasTypeErrors emptyTypusFile)
            # 修复为: prop_test = property $ not (hasTypeErrors emptyTypusFile)
            
            # 6. 修复括号不匹配错误
            # 移除多余的右括号
            new_content = re.sub(r'\)\]\]\)', ')))', new_content)
            
            # 7. 修复choose函数调用错误
            # 匹配: choose (1, 1000 (SourceSpan...
            # 修复为: choose (1, 1000)
            new_content = re.sub(r'choose\s+\(\s*\d+\s*,\s*\d+\s*\([^)]*\)', 
                                lambda m: re.sub(r'\([^)]*$', '', m.group(0)) + ')', 
                                new_content)
            
            # 8. 修复导入语句错误
            # 匹配: import Module (function1), function2)
            # 修复为: import Module (function1, function2)
            new_content = re.sub(r'import\s+([^\s]+(?:\.[^\s]+)*)\s+\(([^)]+)\),\s*\)', 
                                r'import \1 (\2)', new_content)
            
            # 9. 修复函数定义中的缩进错误
            # 匹配: where\n    leadingSpaces :: String -> Int\n  leadingSpaces s = ...
            # 修复为: where\n    leadingSpaces :: String -> Int\n    leadingSpaces s = ...
            new_content = re.sub(r'(\n\s+\w+\s+::[^)]+\n)\s+(\w+\s+)', r'\1\2', new_content)
            
            # 10. 修复null函数调用错误
            # 匹配: null msg []
            # 修复为: null msg
            new_content = re.sub(r'null\s+(\w+)\s*\[\]', r'null \1', new_content)
            
            # 11. 修复===函数定义错误
            # 匹配: (===) = (== [])
            # 修复为: (===) = (==)
            new_content = re.sub(r'\(===\)\s*=\s*\(==\s*\[\]', r'(===) = (==)', new_content)
            
            # 12. 修复变量绑定错误
            # 匹配: n <- choose (0, 20)
            # 修复为: n <- choose (0, 20)
            
            if new_content != original_content:
                with open(file_path, 'w', encoding='utf-8') as f:
                    f.write(new_content)
                print(f"Fixed errors in {file_path}")
                files_modified += 1
                
        except Exception as e:
            print(f"Error processing {file_path}: {e}")
    
    print(f"Total files modified: {files_modified}")

if __name__ == "__main__":
    fix_all_remaining_errors()
