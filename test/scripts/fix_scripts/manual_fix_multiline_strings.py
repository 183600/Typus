#!/usr/bin/env python3
"""
手动修复所有多行字符串问题
"""

def manual_fix_multiline_strings():
    file_path = "/home/runner/work/Typus/Typus/test/Test/Unit/ComprehensiveTypusTestSuite.hs"
    
    # 读取文件
    with open(file_path, 'r', encoding='utf-8') as f:
        lines = f.readlines()
    
    # 手动修复特定的多行字符串
    fixes = [
        # 修复 moveStr (1287-1290)
        (1287, 
         '  then let moveStr = "{//! ownership: on
s := NewMyString(\"" ++ s ++ "\")
t := s
// s 已被移动}"
'),
        
        # 修复 mutableStr (1307-1310) 
        (1307,
         '  then let mutableStr = "{//! ownership: on\ns := NewMyString(\"" ++ s ++ "\")\nm := &mut s\nm.data = \"world\"}"
'),
         
        # 修复 lifetimeStr (1336-1339)
        (1336,
         '  then let lifetimeStr = "{//! ownership: on\nfunc test() {\n  s := NewMyString(\"" ++ s ++ "\")\n  r := &s\n  // r 生命周期不超过 s}"
'),
         
        # 修复 scopeStr (1406-1409)
        (1406,
         '  then let scopeStr = "{//! ownership: on\nfunc test() {\n  s := NewMyString(\"" ++ s ++ "\")\n  // s 作用域在此函数内\n}"
'),
         
        # 修复 closureStr (1485-1488)
        (1485,
         '  then let closureStr = "{//! ownership: on\ns := NewMyString(\"" ++ s ++ "\")\nf := func() { /* 使用 s */ }\n// s 被闭包捕获}"
'),
         
        # 修复 recoveryStr (1590-1593)
        (1590,
         '  then let recoveryStr = "{//! ownership: on\ns := NewMyString(\"" ++ s ++ "\")\nt := s\n// 错误：s 已被移动\n// 编译器提供错误恢复信息}"
'),
    ]
    
    # 应用修复
    for line_num, new_content in fixes:
        # 找到多行字符串的结束行
        end_line = line_num
        while end_line < len(lines) and not lines[end_line].strip().endswith('}"'):
            end_line += 1
        
        # 替换这些行
        lines[line_num-1:end_line] = [new_content]
    
    # 写回文件
    with open(file_path, 'w', encoding='utf-8') as f:
        f.writelines(lines)
    
    print("手动多行字符串修复完成!")

if __name__ == "__main__":
    manual_fix_multiline_strings()