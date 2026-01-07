#!/usr/bin/env python3
"""
修复LANGUAGE和OPTIONS pragma位置问题的脚本
"""
import os
import re
from pathlib import Path

def fix_pragmas_in_file(file_path):
    """修复单个文件中的pragma位置问题"""
    try:
        with open(file_path, 'r', encoding='utf-8') as f:
            content = f.read()
        
        original_content = content
        
        # 查找模块声明位置
        module_match = re.search(r'^module\s+\w+(\.\w+)*\s+', content, re.MULTILINE)
        if not module_match:
            return False  # 没有找到模块声明，跳过
            
        module_start = module_match.start()
        
        # 提取所有LANGUAGE和OPTIONS pragma
        language_pragmas = re.findall(r'({-#\s+LANGUAGE\s+[^#]+#-})', content)
        options_pragmas = re.findall(r'({-#\s+OPTIONS_GHC\s+[^#]+#-})', content)
        
        if not language_pragmas and not options_pragmas:
            return False  # 没有pragma需要修复
            
        # 移除所有的LANGUAGE和OPTIONS pragma
        content = re.sub(r'{-#\s+LANGUAGE\s+[^#]+#-}\s*\n?', '', content)
        content = re.sub(r'{-#\s+OPTIONS_GHC\s+[^#]+#-}\s*\n?', '', content)
        
        # 在模块声明前插入pragma
        pragmas_to_insert = []
        if language_pragmas:
            pragmas_to_insert.extend(language_pragmas)
        if options_pragmas:
            pragmas_to_insert.extend(options_pragmas)
            
        if pragmas_to_insert:
            pragma_section = '\n'.join(pragmas_to_insert) + '\n\n'
            content = content[:module_start] + pragma_section + content[module_start:]
        
        # 如果内容有变化，写回文件
        if content != original_content:
            with open(file_path, 'w', encoding='utf-8') as f:
                f.write(content)
            print(f"修复了 {file_path}")
            return True
            
    except Exception as e:
        print(f"处理 {file_path} 时出错: {e}")
        
    return False

def main():
    """主函数"""
    # 获取所有需要修复的文件
    test_dir = Path("test/Test/Unit")
    fixed_files = []
    
    # 从错误信息中获取需要修复的文件列表
    files_to_fix = [
        "test/Test/Unit/UtilsCorePropertiesQuickCheckSpec.hs",
        "test/Test/Unit/NewSourceLocationMathCoreQuickCheckSpec.hs",
        "test/Test/Unit/NewParserBoundaryCoreQuickCheckSpec.hs",
        "test/Test/Unit/NewOwnershipBasicCoreQuickCheckSpec.hs",
        "test/Test/Unit/NewDependenciesInferenceCoreQuickCheckSpec.hs",
        "test/Test/Unit/NewFreshDependentTypesQuickCheckSpec.hs",
        "test/Test/Unit/NewCompilerIRCoreQuickCheckSpec.hs",
        "test/Test/Unit/CabalQuickCheckPropertiesSpec.hs",
        "test/Test/Unit/NewUtilsQuickCheckPropertySpec.hs",
        "test/Test/Unit/NewSourceLocationQuickCheckPropertySpec.hs",
        "test/Test/Unit/NewParserQuickCheckPropertySpec.hs",
        "test/Test/Unit/NewErrorHandlerQuickCheckPropertySpec.hs",
        "test/Test/Unit/NewOwnershipQuickCheckPropertySpec.hs",
        "test/Test/Unit/NewDependenciesQuickCheckPropertySpec.hs"
    ]
    
    for file_path in files_to_fix:
        full_path = Path(file_path)
        if full_path.exists():
            if fix_pragmas_in_file(full_path):
                fixed_files.append(str(full_path))
        else:
            print(f"文件不存在: {file_path}")
    
    print(f"\n总共修复了 {len(fixed_files)} 个文件")

if __name__ == "__main__":
    main()