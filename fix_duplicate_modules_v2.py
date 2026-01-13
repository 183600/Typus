#!/usr/bin/env python3
"""
修复typus.cabal文件中测试套件的重复模块问题
"""

def fix_duplicate_modules():
    # 读取文件内容
    with open('/home/runner/work/Typus/Typus/typus.cabal', 'r') as f:
        content = f.read()
    
    # 定义需要去重的模块列表
    modules_to_dedup = [
        'Test.Unit.DataStructuresQuickCheckSpec',
        'Test.Unit.MemorySafetyQuickCheckSpec',
        'Test.Unit.ParserErrorRecoveryQuickCheckSpec',
        'Test.Unit.PerformanceBoundaryQuickCheckSpec',
        'Test.Unit.StringProcessingQuickCheckSpec'
    ]
    
    # 将内容按行分割
    lines = content.split('\n')
    
    # 找出typus-test测试套件的other-modules部分
    in_typus_test = False
    in_other_modules = False
    other_modules_start = -1
    other_modules_end = -1
    
    for i, line in enumerate(lines):
        if 'test-suite typus-test' in line:
            in_typus_test = True
        elif in_typus_test and 'other-modules:' in line:
            in_other_modules = True
            other_modules_start = i + 1
        elif in_other_modules and 'build-depends:' in line:
            other_modules_end = i
            break
    
    if other_modules_start == -1 or other_modules_end == -1:
        print("无法找到typus-test测试套件的other-modules部分")
        return
    
    # 提取other-modules部分
    other_modules_lines = lines[other_modules_start:other_modules_end]
    
    # 记录每个模块是否已经出现过
    seen_modules = set()
    fixed_lines = []
    
    for line in other_modules_lines:
        # 检查行中是否包含需要去重的模块
        module_found = False
        for module in modules_to_dedup:
            if module in line:
                module_found = True
                if module not in seen_modules:
                    fixed_lines.append(line)
                    seen_modules.add(module)
                    print(f"保留模块: {module}")
                else:
                    print(f"删除重复模块: {module}")
                break
        
        if not module_found:
            fixed_lines.append(line)
    
    # 替换原文件中的other-modules部分
    new_lines = lines[:other_modules_start] + fixed_lines + lines[other_modules_end:]
    
    # 写回文件
    with open('/home/runner/work/Typus/Typus/typus.cabal', 'w') as f:
        f.write('\n'.join(new_lines))
    
    print("修复完成！")

if __name__ == "__main__":
    fix_duplicate_modules()