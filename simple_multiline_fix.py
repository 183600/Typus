#!/usr/bin/env python3
"""
简单直接修复多行字符串问题
"""

def simple_fix():
    file_path = "/home/runner/work/Typus/Typus/test/Test/Unit/ComprehensiveTypusTestSuite.hs"
    
    # 读取文件内容
    with open(file_path, 'r', encoding='utf-8') as f:
        content = f.read()
    
    # 直接替换多行字符串
    replacements = [
        # moveStr
        ('then let moveStr = "{//! ownership: on\ns := NewMyString(\"" ++ s ++ "\")\nt := s\n// s 已被移动}"',
         'then let moveStr = "{//! ownership: on\\ns := NewMyString(\\"" ++ s ++ "\\")\\nt := s\\n// s 已被移动}"'),
         
        # mutableStr  
        ('then let mutableStr = "{//! ownership: on\ns := NewMyString(\"" ++ s ++ "\")\nm := &mut s\nm.data = \"world\"}"',
         'then let mutableStr = "{//! ownership: on\\ns := NewMyString(\\"" ++ s ++ "\\")\\nm := &mut s\\nm.data = \\"world\\"}"'),
         
        # lifetimeStr
        ('then let lifetimeStr = "{//! ownership: on\nfunc test() {\n  s := NewMyString(\"" ++ s ++ "\")\n  r := &s\n  // r 生命周期不超过 s}"',
         'then let lifetimeStr = "{//! ownership: on\\nfunc test() {\\n  s := NewMyString(\\"" ++ s ++ "\\")\\n  r := &s\\n  // r 生命周期不超过 s}"'),
         
        # scopeStr
        ('then let scopeStr = "{//! ownership: on\nfunc test() {\n  s := NewMyString(\"" ++ s ++ "\")\n  // s 作用域在此函数内\n}"',
         'then let scopeStr = "{//! ownership: on\\nfunc test() {\\n  s := NewMyString(\\"" ++ s ++ "\\")\\n  // s 作用域在此函数内\\n}"'),
         
        # closureStr
        ('then let closureStr = "{//! ownership: on\ns := NewMyString(\"" ++ s ++ "\")\nf := func() { /* 使用 s */ }\n// s 被闭包捕获}"',
         'then let closureStr = "{//! ownership: on\\ns := NewMyString(\\"" ++ s ++ "\\")\\nf := func() { /* 使用 s */ }\\n// s 被闭包捕获}"'),
    ]
    
    # 应用替换
    for old_text, new_text in replacements:
        content = content.replace(old_text, new_text)
    
    # 写回文件
    with open(file_path, 'w', encoding='utf-8') as f:
        f.write(content)
    
    print("简单多行字符串修复完成!")

if __name__ == "__main__":
    simple_fix()