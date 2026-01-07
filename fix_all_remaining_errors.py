#!/usr/bin/env python3
import os
import re

def fix_all_remaining_errors(file_path):
    """修复所有剩余的语法错误"""
    try:
        with open(file_path, 'r') as f:
            content = f.read()
        
        original_content = content
        
        # 修复 BoundaryConditionSpec.hs 中的 case 语句
        if file_path.endswith('BoundaryConditionSpec.hs'):
            # 查找并修复 case 语句的缩进
            content = re.sub(r'(\s+)case result of', r'\1  case result of', content)
        
        # 修复 BasicPropertiesQuickCheckSpec.hs 中的 l1 语句
        if file_path.endswith('BasicPropertiesQuickCheckSpec.hs'):
            # 查找并修复 l1 语句的缩进
            content = re.sub(r'(\s+)l1 <- choose \(1, 100\)', r'\1    l1 <- choose (1, 100)', content)
        
        # 修复 AnalyzerQuickCheckSpec.hs 中的 convertDepTypeVarToExpr 函数
        if file_path.endswith('AnalyzerQuickCheckSpec.hs'):
            # 查找并修复 convertDepTypeVarToExpr 函数的缩进
            content = re.sub(r'^(\s*)convertDepTypeVarToExpr \(DepTS\.TVVar name\) =', r'convertDepTypeVarToExpr (DepTS.TVVar name) =', content, flags=re.MULTILINE)
            content = re.sub(r'^(\s*)convertDepTypeVarToExpr \(DepTS\.TVApp name args\) =', r'  convertDepTypeVarToExpr (DepTS.TVApp name args) =', content, flags=re.MULTILINE)
        
        # 修复 AdvancedSyntaxValidatorQuickCheckSpec.hs 中的 instance Arbitrary
        if file_path.endswith('AdvancedSyntaxValidatorQuickCheckSpec.hs'):
            # 查找并修复 instance Arbitrary 的缩进
            content = re.sub(r'^(\s*)instance Arbitrary SyntaxError where', r'instance Arbitrary SyntaxError where', content, flags=re.MULTILINE)
        
        # 修复 AdvancedSourceLocationTestSpec.hs 中的 spanStart
        if file_path.endswith('AdvancedSourceLocationTestSpec.hs'):
            # 查找并修复 spanStart 的缩进
            content = re.sub(r'(\s+)spanStart merged @\?= SourcePos 1 1 0', r'\1    spanStart merged @?= SourcePos 1 1 0', content)
        
        # 修复 AdvancedCompilerQuickCheckSpec.hs 中的 tests 函数
        if file_path.endswith('AdvancedCompilerQuickCheckSpec.hs'):
            # 查找并修复 tests 函数的缩进
            content = re.sub(r'^(\s*)tests = testGroup', r'  tests = testGroup', content, flags=re.MULTILINE)
        
        # 修复 VerbositySpec.hs 中的 restore 语句
        if file_path.endswith('VerbositySpec.hs'):
            # 查找并修复 restore 语句的缩进
            content = re.sub(r'(\s+)restore oldOriginal =', r'\1    restore oldOriginal =', content)
        
        if content != original_content:
            with open(file_path, 'w') as f:
                f.write(content)
            print(f"Fixed remaining errors in {file_path}")
            return True
        return False
    except Exception as e:
        print(f"Error processing {file_path}: {e}")
        return False

def main():
    test_dir = "/home/runner/work/Typus/Typus/test/Test/Unit"
    fixed_count = 0
    
    # 修复特定的文件
    specific_files = [
        "BoundaryConditionSpec.hs",
        "BasicPropertiesQuickCheckSpec.hs",
        "AnalyzerQuickCheckSpec.hs",
        "AdvancedSyntaxValidatorQuickCheckSpec.hs",
        "AdvancedSourceLocationTestSpec.hs",
        "AdvancedCompilerQuickCheckSpec.hs",
        "VerbositySpec.hs"
    ]
    
    for file_name in specific_files:
        file_path = os.path.join(test_dir, file_name)
        if os.path.exists(file_path):
            if fix_all_remaining_errors(file_path):
                fixed_count += 1
    
    print(f"Fixed remaining errors in {fixed_count} files")

if __name__ == "__main__":
    main()