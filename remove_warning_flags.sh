#!/bin/bash
# 批量移除-Wno-x-partial标志的脚本

find test -name "*.hs" -type f | while read file; do
    if grep -q "\-Wno-x-partial" "$file"; then
        echo "Processing $file..."
        # 使用sed移除-Wno-x-partial标志
        sed -i 's/{-# OPTIONS_GHC -Wno-x-partial #-}//g' "$file"
        # 处理可能在一行中有多个OPTIONS_GHC的情况
        sed -i 's/{-# OPTIONS_GHC \([^}]*\) \-Wno-x-partial \([^}]*\) #-}/{-# OPTIONS_GHC \1 \2 #-}/g' "$file"
        sed -i 's/{-# OPTIONS_GHC \-Wno-x-partial \([^}]*\) #-}/{-# OPTIONS_GHC \1 #-}/g' "$file"
        sed -i 's/{-# OPTIONS_GHC \([^}]*\) \-Wno-x-partial #-}/{-# OPTIONS_GHC \1 #-}/g' "$file"
        echo "Fixed $file"
    fi
done

echo "All files processed."