#!/bin/bash
# 批量修复测试文件中的undefined

set -e

# 修复返回Bool的undefined函数
fix_bool_undefined() {
    local file=$1
    sed -i 's/^\([a-zA-Z_][a-zA-Z0-9_]*\) = undefined$/\1 _ = False/g' "$file"
    sed -i 's/^\([a-zA-Z_][a-zA-Z0-9_]*\) _ = undefined$/\1 _ _ = False/g' "$file"
}

# 处理所有QuickCheck测试文件
for file in /home/runner/work/Typus/Typus/test/Test/Unit/*QuickCheckSpec.hs; do
    if [ -f "$file" ]; then
        echo "Processing $file..."
        
        # 简单替换：对于简单的undefined，根据类型签名替换
        # Bool -> False
        # Maybe -> Nothing  
        # [] -> []
        # Int -> 0
        # String -> ""
        
        # 这个脚本只做基本的替换，更复杂的需要手动处理
        sed -i '/= undefined$/s/= undefined$/= False -- Auto-fixed/g' "$file" || true
    fi
done

echo "Done!"
