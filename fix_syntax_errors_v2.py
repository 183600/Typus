#!/usr/bin/env python3
import os
import re
import glob

def fix_do_indentation_errors():
    """修复do关键字后的缩进错误"""
    test_dir = "/home/runner/work/Typus/Typus/test/Test/Unit"
    pattern = os.path.join(test_dir, "*.hs")
    
    files_modified = 0
    
    for file_path in glob.glob(pattern):
        try:
            with open(file_path, 'r', encoding='utf-8') as f:
                content = f.read()
            
            new_content = content
            
            # 修复do关键字后的缩进错误
            # 匹配模式: 
            # arbitrary = do
            # n <- choose (0, 20)
            # 
            # 修复为:
            # arbitrary = do
            #   n <- choose (0, 20)
            
            # 使用正则表达式匹配并修复缩进
            pattern1 = r'(arbitrary = do\n)\s+(\w+\s+<-)'
            new_content = re.sub(pattern1, r'\1  \2', new_content)
            
            # 修复其他类似情况
            pattern2 = r'(\bdo\n)\s+(\w+\s+<-)'
            new_content = re.sub(pattern2, r'\1  \2', new_content)
            
            if new_content != content:
                with open(file_path, 'w', encoding='utf-8') as f:
                    f.write(new_content)
                print(f"Fixed do indentation in {file_path}")
                files_modified += 1
                
        except Exception as e:
            print(f"Error processing {file_path}: {e}")
    
    print(f"Total files modified: {files_modified}")

def fix_bracket_mismatch_errors():
    """修复括号不匹配的错误"""
    test_dir = "/home/runner/work/Typus/Typus/test/Test/Unit"
    pattern = os.path.join(test_dir, "*.hs")
    
    files_modified = 0
    
    for file_path in glob.glob(pattern):
        try:
            with open(file_path, 'r', encoding='utf-8') as f:
                content = f.read()
            
            new_content = content
            
            # 修复特定的括号不匹配错误
            # 例如: hasContent target typusFile = L.any (isInfixOf target . cbContent) (tfBlocks typusFile [])))
            if 'L.any (isInfixOf target . cbContent) (tfBlocks typusFile [])))' in new_content:
                new_content = new_content.replace(
                    'L.any (isInfixOf target . cbContent) (tfBlocks typusFile [])))',
                    'L.any (isInfixOf target . cbContent) (tfBlocks typusFile []))'
                )
            
            if new_content != content:
                with open(file_path, 'w', encoding='utf-8') as f:
                    f.write(new_content)
                print(f"Fixed bracket mismatch in {file_path}")
                files_modified += 1
                
        except Exception as e:
            print(f"Error processing {file_path}: {e}")
    
    print(f"Total files modified: {files_modified}")

def fix_comma_in_list_errors():
    """修复列表中的逗号错误"""
    test_dir = "/home/runner/work/Typus/Typus/test/Test/Unit"
    pattern = os.path.join(test_dir, "*.hs")
    
    files_modified = 0
    
    for file_path in glob.glob(pattern):
        try:
            with open(file_path, 'r', encoding='utf-8') as f:
                content = f.read()
            
            new_content = content
            
            # 修复列表中的逗号错误
            # 例如: , return "\0\1\2\xFF"  -- Invalid characters
            # 修复为: , return "\0\1\2\xFF"  -- Invalid characters
            # 这可能需要根据具体情况调整
            
            if new_content != content:
                with open(file_path, 'w', encoding='utf-8') as f:
                    f.write(new_content)
                print(f"Fixed comma in list in {file_path}")
                files_modified += 1
                
        except Exception as e:
            print(f"Error processing {file_path}: {e}")
    
    print(f"Total files modified: {files_modified}")

if __name__ == "__main__":
    fix_do_indentation_errors()
    fix_bracket_mismatch_errors()
    fix_comma_in_list_errors()