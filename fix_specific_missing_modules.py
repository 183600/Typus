#!/usr/bin/env python3
"""
从cabal文件中移除特定的缺失模块
"""
import os
import re
from pathlib import Path

def update_cabal_file():
    """更新cabal文件，移除特定的缺失模块"""
    cabal_path = Path("typus.cabal")
    
    with open(cabal_path, 'r', encoding='utf-8') as f:
        content = f.read()
    
    # 从错误信息中提取的缺失模块列表
    missing_modules = [
        'Test.Unit.AdditionalQuickCheckTests2025',
        'Test.Unit.AdditionalCabalQuickCheckSpec',
        'Test.Unit.AdditionalUtilsSpec',
        'Test.Unit.AdditionalSourceLocationSpec',
        'Test.Unit.AdditionalParserSpec',
        'Test.Unit.AdditionalErrorHandlerSpec',
        'Test.Unit.AdditionalSyntaxValidatorSpec',
        'Test.Unit.AdditionalUtilsQuickCheckSpec',
        'Test.Unit.AdditionalSourceLocationQuickCheckSpec',
        'Test.Unit.AdditionalPropertyTestsQuickCheckSpec',
        'Test.Unit.AdditionalOwnershipAnalysisQuickCheckSpec',
        'Test.Unit.AdditionalCoreQuickCheckSpec',
        'Test.Unit.AdditionalCoreTestsSpec',
        'Test.Unit.AdditionalQuickCheckTests',
        'Test.Unit.AdditionalQuickCheckSpec',
        'Test.Unit.AdditionalTypusSpec',
        'Test.Unit.AdditionalCabalQuickCheckTestSpec',
        'Test.Unit.AdditionalCabalTestsSpec',
        'Test.Unit.AdditionalDependencyAnalysisQuickCheckSpec',
        'Test.Unit.AdditionalCorePropertiesSpec',
        'Test.Unit.AdditionalCabalQuickCheckTestSuite'
    ]
    
    # 移除这些模块
    for module in missing_modules:
        # 匹配模块行（可能带有前导空格和逗号）
        pattern = rf'(\s*){re.escape(module)}\s*,?\s*\n'
        content = re.sub(pattern, '', content)
        print(f"移除模块: {module}")
    
    # 清理多余的空行
    content = re.sub(r'\n\s*\n\s*\n', '\n\n', content)
    
    # 写回文件
    with open(cabal_path, 'w', encoding='utf-8') as f:
        f.write(content)
    
    print("已更新cabal文件，移除了所有缺失的模块")

def main():
    """主函数"""
    update_cabal_file()

if __name__ == "__main__":
    main()