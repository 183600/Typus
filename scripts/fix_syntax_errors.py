#!/usr/bin/env python3

# 修复BasicQuickCheckTestSuite.hs中的语法错误

with open('/home/runner/work/Typus/Typus/test/Test/Unit/BasicQuickCheckTestSuite.hs', 'r') as f:
    lines = f.readlines()

# 修复第271行
if len(lines) >= 271:
    lines[270] = '  assertBool "Right value is right" (isRight (Right (42 :: Int)))\n'

# 修复第272行
if len(lines) >= 272:
    lines[271] = '  assertBool "Left value is not right" (not $ isRight (Left ("error" :: String)))\n'

# 修复第278行
if len(lines) >= 278:
    lines[277] = '  assertBool "Right value is not left" (not $ isLeft (Right ("success" :: String)))\n'

with open('/home/runner/work/Typus/Typus/test/Test/Unit/BasicQuickCheckTestSuite.hs', 'w') as f:
    f.writelines(lines)

print("语法错误已修复")