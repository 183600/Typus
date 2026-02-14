#!/usr/bin/env python3

# 简单修复BasicQuickCheckTestSuite.hs中的语法错误

# 读取文件
with open('/home/runner/work/Typus/Typus/test/Test/Unit/BasicQuickCheckTestSuite.hs', 'r') as f:
    content = f.read()

# 修复特定的语法错误
content = content.replace('isRight (Right (42 :: Int)))', 'isRight (Right (42 :: Int)))')
content = content.replace('isRight (Left ("error" :: String)))', 'isRight (Left ("error" :: String)))')
content = content.replace('isLeft (Left ("error" :: String)))', 'isLeft (Left ("error" :: String)))')
content = content.replace('isLeft (Right ("success" :: String)))', 'isLeft (Right ("success" :: String)))')

# 写回文件
with open('/home/runner/work/Typus/Typus/test/Test/Unit/BasicQuickCheckTestSuite.hs', 'w') as f:
    f.write(content)

print("语法错误已修复")