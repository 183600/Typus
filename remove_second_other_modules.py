#!/usr/bin/env python3
"""
删除第二个other-modules部分
"""
from pathlib import Path

def remove_second_other_modules():
    """删除第二个other-modules部分"""
    cabal_path = Path("typus.cabal")
    
    with open(cabal_path, 'r', encoding='utf-8') as f:
        lines = f.readlines()
    
    # 找到第二个other-modules行（行号1155，索引1154）
    # 找到build-depends行（行号1192，索引1191）
    # 删除第1155行到第1191行
    
    # 修复第一个other-modules部分的最后一个模块
    # 第1153行是Test.Unit.NewCabalEdgeCaseSpec,
    # 需要删除逗号
    if lines[1152].strip().endswith(','):
        lines[1152] = lines[1152][:-1] + '
'
    
    # 删除第二个other-modules部分
    del lines[1154:1191]
    
    # 写回文件
    with open(cabal_path, 'w', encoding='utf-8') as f:
        f.writelines(lines)
    
    print("已删除第二个other-modules部分")

def main():
    """主函数"""
    remove_second_other_modules()

if __name__ == "__main__":
    main()