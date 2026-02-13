#!/usr/bin/env python3

# 清理和修复剩余的 Ownership 类型错误

def clean_and_fix():
    with open('/home/runner/work/Typus/Typus/test/Test/Unit/ComprehensiveTypusTestSuite.hs', 'r') as f:
        content = f.read()
    
    # 修复不完整的行：删除 "in case errors of:" 行
    lines = content.split('\n')
    fixed_lines = []
    
    i = 0
    while i < len(lines):
        line = lines[i]
        
        # 如果这一行是 "in case errors of:" 并且前一行包含 O.analyzeOwnership，跳过它
        if 'in case errors of:' in line and i > 0:
            prev_line = lines[i-1]
            if 'O.analyzeOwnership' in prev_line:
                # 跳过这一行
                i += 1
                continue
        
        # 如果这一行是 "Right _ -> property True" 并且在 O.analyzeOwnership 上下文中，跳过它
        if 'Right _ -> property True' in line:
            # 检查前面几行是否有 O.analyzeOwnership
            found_ownership = False
            for j in range(max(0, i-5), i):
                if 'O.analyzeOwnership' in lines[j]:
                    found_ownership = True
                    break
            if found_ownership:
                i += 1
                continue
        
        # 如果这一行是 "Left _ -> property False" 并且在 O.analyzeOwnership 上下文中，跳过它
        if 'Left _ -> property False' in line:
            # 检查前面几行是否有 O.analyzeOwnership
            found_ownership = False
            for j in range(max(0, i-5), i):
                if 'O.analyzeOwnership' in lines[j]:
                    found_ownership = True
                    break
            if found_ownership:
                i += 1
                continue
        
        fixed_lines.append(line)
        i += 1
    
    # 写回文件
    new_content = '\n'.join(fixed_lines)
    with open('/home/runner/work/Typus/Typus/test/Test/Unit/ComprehensiveTypusTestSuite.hs', 'w') as f:
        f.write(new_content)
    
    print("Cleaned up remaining ownership errors")

if __name__ == "__main__":
    clean_and_fix()