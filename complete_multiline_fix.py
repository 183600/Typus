#!/usr/bin/env python3
"""
修复ComprehensiveTypusTestSuite.hs中所有的多行字符串问题
"""

import re

def fix_all_multiline_strings_complete():
    file_path = "/home/runner/work/Typus/Typus/test/Test/Unit/ComprehensiveTypusTestSuite.hs"
    
    # 读取文件
    with open(file_path, 'r', encoding='utf-8') as f:
        content = f.read()
    
    # 定义所有需要修复的多行字符串映射
    multiline_fixes = {
        # interactionStr (1079)
        'then let interactionStr = "{//! ownership: on\n//! dependent_types: on\ns := NewMyString(\"" ++ s ++ "\")}"':
        'then let interactionStr = "{//! ownership: on\\n//! dependent_types: on\\ns := NewMyString(\\\"" ++ s ++ "\\\")}"',
        
        # blockStr (1089)
        'then let blockStr = "func main() {\n  {//! ownership: on\n    " ++ s ++ "\n  }\n  {//! dependent_types: on\n    " ++ s ++ "\n  }\n}"':
        'then let blockStr = "func main() {\\n  {//! ownership: on\\n    " ++ s ++ "\\n  }\\n  {//! dependent_types: on\\n    " ++ s ++ "\\n  }\\n}"',
        
        # ownershipStr (1279)
        'then let ownershipStr = "{//! ownership: on\ns := NewMyString(\"" ++ s ++ "\")\nt := s}"':
        'then let ownershipStr = "{//! ownership: on\\ns := NewMyString(\\\"" ++ s ++ "\\\")\\nt := s}"',
        
        # borrowStr (1295)
        'then let borrowStr = "{//! ownership: on\ns := NewMyString(\"" ++ s ++ "\")\nr := &s\nfmt.Println(r.data)}"':
        'then let borrowStr = "{//! ownership: on\\ns := NewMyString(\\\"" ++ s ++ "\\\")\\nr := &s\\nfmt.Println(r.data)}"',
        
        # mutableStr (1303)
        'then let mutableStr = "{//! ownership: on\ns := NewMyString(\"" ++ s ++ "\")\nm := &mut s\nm.data = \"world\"}"':
        'then let mutableStr = "{//! ownership: on\\ns := NewMyString(\\\"" ++ s ++ "\\\")\\nm := &mut s\\nm.data = \\"world\\"}"',
        
        # rulesStr (1312)
        'then let rulesStr = "{//! ownership: on\ns := NewMyString(\"" ++ s ++ "\")\nr1 := &s\nr2 := &s\n// 多个不可变借用允许}"':
        'then let rulesStr = "{//! ownership: on\\ns := NewMyString(\\\"" ++ s ++ "\\\")\\nr1 := &s\\nr2 := &s\\n// 多个不可变借用允许}"',
        
        # conflictStr (1320)
        'then let conflictStr = "{//! ownership: on\ns := NewMyString(\"" ++ s ++ "\")\nr := &s\nm := &mut s\n// 借用冲突}"':
        'then let conflictStr = "{//! ownership: on\\ns := NewMyString(\\\"" ++ s ++ "\\\")\\nr := &s\\nm := &mut s\\n// 借用冲突}"',
        
        # lifetimeStr (1329)
        'then let lifetimeStr = "{//! ownership: on\nfunc test() {\n  s := NewMyString(\"" ++ s ++ "\")\n  r := &s\n  // r 生命周期不超过 s}"':
        'then let lifetimeStr = "{//! ownership: on\\nfunc test() {\\n  s := NewMyString(\\\"" ++ s ++ "\\\")\\n  r := &s\\n  // r 生命周期不超过 s}"',
        
        # gcStr (1335)
        'then let gcStr = "{//! ownership: on\ns := NewMyString(\"" ++ s ++ "\")\n// 所有权检查是编译期的，GC仍是运行时的}"':
        'then let gcStr = "{//! ownership: on\\ns := NewMyString(\\\"" ++ s ++ "\\\")\\n// 所有权检查是编译期的，GC仍是运行时的}"',
        
        # goroutineStr (1344)
        'then let goroutineStr = "{//! ownership: on\ns := NewMyString(\"" ++ s ++ "\")\ngo func() {\n  // 使用 s\n}()\n// s 不能再使用}"':
        'then let goroutineStr = "{//! ownership: on\\ns := NewMyString(\\\"" ++ s ++ "\\\")\\ngo func() {\\n  // 使用 s\\n}()\\n// s 不能再使用}"',
        
        # errorStr (1361)
        'then let errorStr = "{//! ownership: on\ns := NewMyString(\"" ++ s ++ "\")\nt := s\nfmt.Println(s.data) // 错误：s 已被移动}"':
        'then let errorStr = "{//! ownership: on\\ns := NewMyString(\\\"" ++ s ++ "\\\")\\nt := s\\nfmt.Println(s.data) // 错误：s 已被移动}"',
        
        # overheadStr (1369)
        'then let overheadStr = "{//! ownership: on\ns := NewMyString(\"" ++ s ++ "\")\nt := s\n// 所有权检查在编译期完成}"':
        'then let overheadStr = "{//! ownership: on\\ns := NewMyString(\\\"" ++ s ++ "\\\")\\nt := s\\n// 所有权检查在编译期完成}"',
        
        # raceStr (1378)
        'then let raceStr = "{//! ownership: on\ns := NewMyString(\"" ++ s ++ "\")\nm := &mut s\n// 同一时刻只能有一个可变借用}"':
        'then let raceStr = "{//! ownership: on\\ns := NewMyString(\\\"" ++ s ++ "\\\")\\nm := &mut s\\n// 同一时刻只能有一个可变借用}"',
        
        # correctnessStr (1386)
        'then let correctnessStr = "{//! ownership: on\ns := NewMyString(\"" ++ s ++ "\")\n// 所有权确保逻辑正确性}"':
        'then let correctnessStr = "{//! ownership: on\\ns := NewMyString(\\\"" ++ s ++ "\\\")\\n// 所有权确保逻辑正确性}"',
        
        # scopeStr (1395)
        'then let scopeStr = "{//! ownership: on\nfunc test() {\n  s := NewMyString(\"" ++ s ++ "\")\n  // s 作用域在此函数内\n}"':
        'then let scopeStr = "{//! ownership: on\\nfunc test() {\\n  s := NewMyString(\\\"" ++ s ++ "\\\")\\n  // s 作用域在此函数内\\n}"',
        
        # closureStr (1470)
        'then let closureStr = "{//! ownership: on\ns := NewMyString(\"" ++ s ++ "\")\nf := func() { /* 使用 s */ }\n// s 被闭包捕获}"':
        'then let closureStr = "{//! ownership: on\\ns := NewMyString(\\\"" ++ s ++ "\\\")\\nf := func() { /* 使用 s */ }\\n// s 被闭包捕获}"',
        
        # recoveryStr (1572)
        'then let recoveryStr = "{//! ownership: on\ns := NewMyString(\"" ++ s ++ "\")\nt := s\n// 错误：s 已被移动\n// 编译器提供错误恢复信息}"':
        'then let recoveryStr = "{//! ownership: on\\ns := NewMyString(\\\"" ++ s ++ "\\\")\\nt := s\\n// 错误：s 已被移动\\n// 编译器提供错误恢复信息}"',
        
        # safetyStr (1521)
        'then let safetyStr = "{//! ownership: on\ns := NewMyString(\"" ++ s ++ "\")\nt := s\n// 所有权确保内存安全}"':
        'then let safetyStr = "{//! ownership: on\\ns := NewMyString(\\\"" ++ s ++ "\\\")\\nt := s\\n// 所有权确保内存安全}"',
        
        # resourceStr (1530)
        'then let resourceStr = "{//! ownership: on\ns := NewMyString(\"" ++ s ++ "\")\n// 所有权确保资源正确释放}"':
        'then let resourceStr = "{//! ownership: on\\ns := NewMyString(\\\"" ++ s ++ "\\\")\\n// 所有权确保资源正确释放}"',
        
        # concurrentStr (1538)
        'then let concurrentStr = "{//! ownership: on\ns := NewMyString(\"" ++ s ++ "\")\n// 所有权确保并发安全}"':
        'then let concurrentStr = "{//! ownership: on\\ns := NewMyString(\\\"" ++ s ++ "\\\")\\n// 所有权确保并发安全}"',
        
        # optimizationStr (1555)
        'then let optimizationStr = "{//! ownership: on\ns := NewMyString(\"" ++ s ++ "\")\nt := s\n// 编译器可以优化所有权检查}"':
        'then let optimizationStr = "{//! ownership: on\\ns := NewMyString(\\\"" ++ s ++ "\\\")\\nt := s\\n// 编译器可以优化所有权检查}"',
        
        # debugStr (1564)
        'then let debugStr = "{//! ownership: on\n//! debug: ownership\ns := NewMyString(\"" ++ s ++ "\")\n// 调试所有权信息}"':
        'then let debugStr = "{//! ownership: on\\n//! debug: ownership\\ns := NewMyString(\\\"" ++ s ++ "\\\")\\n// 调试所有权信息}"',
        
        # performanceStr (1581)
        'then let performanceStr = "{//! ownership: on\ns := NewMyString(\"" ++ s ++ "\")\nt := s\n// 所有权检查零运行时开销}"':
        'then let performanceStr = "{//! ownership: on\\ns := NewMyString(\\\"" ++ s ++ "\\\")\\nt := s\\n// 所有权检查零运行时开销}"',
        
        # interactionStr (1598)
        'then let interactionStr = "{//! ownership: on\n//! dependent_types: on\ns := NewMyString(\"" ++ s ++ "\")\n// 所有权与依赖类型交互}"':
        'then let interactionStr = "{//! ownership: on\\n//! dependent_types: on\\ns := NewMyString(\\\"" ++ s ++ "\\\")\\n// 所有权与依赖类型交互}"',
        
        # practicesStr (1606)
        'then let practicesStr = "{//! ownership: on\ns := NewMyString(\"" ++ s ++ "\")\n// 所有权最佳实践示例}"':
        'then let practicesStr = "{//! ownership: on\\ns := NewMyString(\\\"" ++ s ++ "\\\")\\n// 所有权最佳实践示例}"',
    }
    
    # 应用所有修复
    for old_pattern, new_pattern in multiline_fixes.items():
        content = content.replace(old_pattern, new_pattern)
    
    # 写回文件
    with open(file_path, 'w', encoding='utf-8') as f:
        f.write(content)
    
    print("所有多行字符串修复完成!")

if __name__ == "__main__":
    fix_all_multiline_strings_complete()