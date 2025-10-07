#!/bin/bash

# 测试覆盖率报告脚本
# 生成详细的测试覆盖率报告并进行分析

set -e

echo "生成测试覆盖率报告..."
echo "======================"

# 检查是否启用了覆盖率
if [ ! -f "typus-test.tix" ]; then
    echo "错误: 未找到覆盖率数据文件 typus-test.tix"
    echo "请使用 --enable-coverage 重新构建项目"
    exit 1
fi

# 创建覆盖率报告目录
COVERAGE_DIR="coverage-report"
mkdir -p "$COVERAGE_DIR"

echo "1. 生成基本覆盖率报告..."
hpc report typus-test.tix --include=src --exclude=Main > "$COVERAGE_DIR/basic-report.txt"

echo "2. 生成详细覆盖率报告..."
hpc markup typus-test.tix --include=src --exclude=Main --destdir="$COVERAGE_DIR/hpc-html/"

echo "3. 生成模块覆盖率报告..."
# 获取所有模块
MODULES=$(find src -name "*.hs" -type f | sed 's|src/||' | sed 's|\.hs$||' | tr '/' '.')

echo "模块覆盖率分析:" > "$COVERAGE_DIR/module-report.txt"
for module in $MODULES; do
    if [ "$module" != "Main" ]; then
        echo "----------------------------------------" >> "$COVERAGE_DIR/module-report.txt"
        echo "模块: $module" >> "$COVERAGE_DIR/module-report.txt"
        hpc report typus-test.tix --include="src" --include="$module" --exclude="Main" >> "$COVERAGE_DIR/module-report.txt" 2>/dev/null || echo "  模块无覆盖率数据" >> "$COVERAGE_DIR/module-report.txt"
    fi
done

echo "4. 分析覆盖率质量..."
TOTAL_COVERAGE=$(grep "expression" "$COVERAGE_DIR/basic-report.txt" | awk '{print $7}' | tr -d '%')

echo ""
echo "覆盖率分析结果:"
echo "================"

if [ -n "$TOTAL_COVERAGE" ]; then
    echo "总覆盖率: ${TOTAL_COVERAGE}%"

    # 覆盖率等级评估
    if [ "$TOTAL_COVERAGE" -ge 90 ]; then
        echo "等级: 优秀 (>=90%)"
        QUALITY="优秀"
    elif [ "$TOTAL_COVERAGE" -ge 80 ]; then
        echo "等级: 良好 (80-89%)"
        QUALITY="良好"
    elif [ "$TOTAL_COVERAGE" -ge 70 ]; then
        echo "等级: 一般 (70-79%)"
        QUALITY="一般"
    elif [ "$TOTAL_COVERAGE" -ge 60 ]; then
        echo "等级: 及格 (60-69%)"
        QUALITY="及格"
    else
        echo "等级: 不及格 (<60%)"
        QUALITY="不及格"
    fi

    echo "质量评级: $QUALITY"

    # 检查覆盖率是否达到生产标准
    if [ "$TOTAL_COVERAGE" -ge 80 ]; then
        echo "✓ 覆盖率满足生产环境要求 (>=80%)"
    else
        echo "⚠ 覆盖率不满足生产环境要求 (<80%)"
        echo "建议: 增加测试用例以提高覆盖率"
    fi
else
    echo "警告: 无法解析总覆盖率数据"
fi

echo ""
echo "5. 识别未覆盖的代码区域..."
echo "未覆盖的函数/代码行:" > "$COVERAGE_DIR/uncovered.txt"

# 检查每个源文件的覆盖率
find src -name "*.hs" -type f | while read file; do
    module_name=$(echo "$file" | sed 's|src/||' | sed 's|\.hs$||' | tr '/' '.')
    if [ "$module_name" != "Main" ]; then
        coverage=$(hpc report typus-test.tix --include="src" --include="$module_name" --exclude="Main" 2>/dev/null | grep "expression" | awk '{print $7}' | tr -d '%' || echo "0")
        if [ "$coverage" != "0" ]; then
            echo "  $module_name: ${coverage}% 覆盖率"
            if [ "$coverage" -lt 80 ]; then
                echo "    ⚠ 覆盖率较低，建议增加测试"
            fi
        fi
    fi
done

echo ""
echo "6. 生成HTML报告..."
if [ -d "$COVERAGE_DIR/hpc-html/" ]; then
    echo "✓ HTML覆盖率报告已生成"
    echo "   报告位置: $COVERAGE_DIR/hpc-html/hpc_index.html"
    echo "   使用以下命令查看: python3 -m webbrowser $COVERAGE_DIR/hpc-html/hpc_index.html"
else
    echo "警告: HTML报告生成失败"
fi

echo ""
echo "7. 生成覆盖率摘要..."
cat > "$COVERAGE_DIR/summary.json" << EOF
{
  "generated_at": "$(date -Iseconds)",
  "total_coverage": "${TOTAL_COVERAGE:-0}%",
  "quality_grade": "${QUALITY:-unknown}",
  "production_ready": $([ "${TOTAL_COVERAGE:-0}" -ge 80 ] && echo "true" || echo "false"),
  "recommendations": [
    $([ "${TOTAL_COVERAGE:-0}" -lt 80 ] && echo "\"增加单元测试以提高覆盖率\"," || echo "")
    "定期运行覆盖率检查",
    "对关键模块进行重点测试覆盖"
  ]
}
EOF

echo "✓ 覆盖率摘要已生成: $COVERAGE_DIR/summary.json"

echo ""
echo "=================="
echo "覆盖率报告完成"
echo "=================="
echo "报告文件位置:"
echo "  - 基本报告: $COVERAGE_DIR/basic-report.txt"
echo "  - 模块报告: $COVERAGE_DIR/module-report.txt"
echo "  - HTML报告: $COVERAGE_DIR/hpc-html/hpc_index.html"
echo "  - 未覆盖代码: $COVERAGE_DIR/uncovered.txt"
echo "  - JSON摘要: $COVERAGE_DIR/summary.json"