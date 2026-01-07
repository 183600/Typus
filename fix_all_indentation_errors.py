#!/usr/bin/env python3
import os
import re

def fix_all_indentation_errors(file_path):
    """修复所有缩进错误"""
    try:
        with open(file_path, 'r') as f:
            content = f.read()
        
        original_content = content
        
        # 修复 AnalyzerSymbolTableQuickCheckTestSpec.hs 中的 isJust 函数
        if file_path.endswith('AnalyzerSymbolTableQuickCheckTestSpec.hs'):
            content = re.sub(r'^  isJust \(Just _\) = True', r'  isJust (Just _) = True', content, flags=re.MULTILINE)
        
        # 修复 AnalyzerQuickCheckSpec.hs 中的 convertDepTypeVarToExpr 函数
        if file_path.endswith('AnalyzerQuickCheckSpec.hs'):
            content = re.sub(r'^  convertDepTypeVarToExpr \(DepTS\.TVVar name\) =', r'  convertDepTypeVarToExpr (DepTS.TVVar name) =', content, flags=re.MULTILINE)
        
        # 修复 AdvancedSyntaxValidatorQuickCheckSpec.hs 中的 instance Arbitrary
        if file_path.endswith('AdvancedSyntaxValidatorQuickCheckSpec.hs'):
            content = re.sub(r'^  instance Arbitrary SyntaxError where', r'  instance Arbitrary SyntaxError where', content, flags=re.MULTILINE)
        
        # 修复 AdvancedSourceLocationTestSpec.hs 中的 spanStart
        if file_path.endswith('AdvancedSourceLocationTestSpec.hs'):
            content = re.sub(r'^        spanStart merged @\?= SourcePos 1 1 0', r'        spanStart merged @?= SourcePos 1 1 0', content, flags=re.MULTILINE)
        
        # 修复 AdvancedOwnershipQuickCheckSpec.hs 中的列表项
        if file_path.endswith('AdvancedOwnershipQuickCheckSpec.hs'):
            content = re.sub(r'^    , LoopOwnershipError', r'      , LoopOwnershipError', content, flags=re.MULTILINE)
        
        # 修复 AdvancedCompilerQuickCheckSpec.hs 中的 tests 函数
        if file_path.endswith('AdvancedCompilerQuickCheckSpec.hs'):
            content = re.sub(r'^tests = testGroup', r'  tests = testGroup', content, flags=re.MULTILINE)
        
        # 修复 VerbositySpec.hs 中的 case original of
        if file_path.endswith('VerbositySpec.hs'):
            content = re.sub(r'^            case original of', r'            case original of', content, flags=re.MULTILINE)
        
        if content != original_content:
            with open(file_path, 'w') as f:
                f.write(content)
            print(f"Fixed indentation errors in {file_path}")
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
        "AnalyzerSymbolTableQuickCheckTestSpec.hs",
        "AnalyzerQuickCheckSpec.hs",
        "AdvancedSyntaxValidatorQuickCheckSpec.hs",
        "AdvancedSourceLocationTestSpec.hs",
        "AdvancedOwnershipQuickCheckSpec.hs",
        "AdvancedCompilerQuickCheckSpec.hs",
        "VerbositySpec.hs"
    ]
    
    for file_name in specific_files:
        file_path = os.path.join(test_dir, file_name)
        if os.path.exists(file_path):
            if fix_all_indentation_errors(file_path):
                fixed_count += 1
    
    print(f"Fixed indentation errors in {fixed_count} files")

if __name__ == "__main__":
    main()