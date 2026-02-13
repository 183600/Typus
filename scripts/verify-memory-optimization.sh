#!/bin/bash

# 内存优化效果验证脚本
# 这个脚本用于验证内存优化措施的有效性

set -e

# 颜色输出
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# 配置
LOG_DIR="memory-verification-logs"
TIMESTAMP=$(date +"%Y%m%d_%H%M%S")
BASELINE_LOG="$LOG_DIR/baseline_$TIMESTAMP.log"
OPTIMIZED_LOG="$LOG_DIR/optimized_$TIMESTAMP.log"
MEMORY_REPORT="$LOG_DIR/memory_report_$TIMESTAMP.txt"

# 创建日志目录
mkdir -p "$LOG_DIR"

echo -e "${GREEN}=== 内存优化效果验证 ===${NC}"
echo -e "${BLUE}时间戳: $TIMESTAMP${NC}"
echo ""

# 检测系统信息
detect_system_info() {
    echo -e "${BLUE}检测系统信息...${NC}"
    
    echo "=== 系统信息 ===" > "$MEMORY_REPORT"
    echo "时间: $(date)" >> "$MEMORY_REPORT"
    echo "操作系统: $(uname -s)" >> "$MEMORY_REPORT"
    echo "内核版本: $(uname -r)" >> "$MEMORY_REPORT"
    
    if command -v free >/dev/null 2>&1; then
        echo "内存信息:" >> "$MEMORY_REPORT"
        free -h >> "$MEMORY_REPORT"
    fi
    
    if command -v lscpu >/dev/null 2>&1; then
        echo "CPU信息:" >> "$MEMORY_REPORT"
        lscpu | grep "Model name\|CPU(s):\|Thread(s)" >> "$MEMORY_REPORT"
    fi
    
    echo "" >> "$MEMORY_REPORT"
    echo -e "${GREEN}系统信息已记录${NC}"
}

# 运行基准测试
run_baseline_tests() {
    echo -e "${BLUE}运行基准测试...${NC}"
    
    echo "=== 基准测试 ===" >> "$MEMORY_REPORT"
    echo "开始时间: $(date)" >> "$MEMORY_REPORT"
    
    # 运行标准测试
    if command -v /usr/bin/time >/dev/null 2>&1; then
        /usr/bin/time -v cabal test --test-show-details=always 2>&1 | tee "$BASELINE_LOG" || true
    else
        cabal test --test-show-details=always 2>&1 | tee "$BASELINE_LOG" || true
    fi
    
    echo "结束时间: $(date)" >> "$MEMORY_REPORT"
    
    # 提取内存使用信息
    if grep -q "Maximum resident set size" "$BASELINE_LOG"; then
        BASELINE_MEMORY=$(grep "Maximum resident set size" "$BASELINE_LOG" | awk '{print $6}')
        echo "基准测试峰值内存: ${BASELINE_MEMORY}KB" >> "$MEMORY_REPORT"
    fi
    
    echo -e "${GREEN}基准测试完成${NC}"
}

# 运行优化测试
run_optimized_tests() {
    echo -e "${BLUE}运行优化测试...${NC}"
    
    echo "=== 优化测试 ===" >> "$MEMORY_REPORT"
    echo "开始时间: $(date)" >> "$MEMORY_REPORT"
    
    # 运行内存优化测试
    if command -v /usr/bin/time >/dev/null 2>&1; then
        /usr/bin/time -v ./scripts/enhanced-memory-test-config.sh --environment standard 2>&1 | tee "$OPTIMIZED_LOG" || true
    else
        ./scripts/enhanced-memory-test-config.sh --environment standard 2>&1 | tee "$OPTIMIZED_LOG" || true
    fi
    
    echo "结束时间: $(date)" >> "$MEMORY_REPORT"
    
    # 提取内存使用信息
    if grep -q "Maximum resident set size" "$OPTIMIZED_LOG"; then
        OPTIMIZED_MEMORY=$(grep "Maximum resident set size" "$OPTIMIZED_LOG" | awk '{print $6}')
        echo "优化测试峰值内存: ${OPTIMIZED_MEMORY}KB" >> "$MEMORY_REPORT"
    fi
    
    echo -e "${GREEN}优化测试完成${NC}"
}

# 分析测试结果
analyze_results() {
    echo -e "${BLUE}分析测试结果...${NC}"
    
    echo "=== 结果分析 ===" >> "$MEMORY_REPORT"
    
    # 分析测试数量
    if grep -q "tests:" "$BASELINE_LOG"; then
        BASELINE_TESTS=$(grep -o "[0-9]\+ tests" "$BASELINE_LOG" | head -1 | awk '{print $1}')
        echo "基准测试数量: $BASELINE_TESTS" >> "$MEMORY_REPORT"
    fi
    
    if grep -q "tests:" "$OPTIMIZED_LOG"; then
        OPTIMIZED_TESTS=$(grep -o "[0-9]\+ tests" "$OPTIMIZED_LOG" | head -1 | awk '{print $1}')
        echo "优化测试数量: $OPTIMIZED_TESTS" >> "$MEMORY_REPORT"
    fi
    
    # 分析执行时间
    if grep -q "Elapsed (wall clock) time" "$BASELINE_LOG"; then
        BASELINE_TIME=$(grep "Elapsed (wall clock) time" "$BASELINE_LOG" | awk '{print $5}')
        echo "基准测试时间: $BASELINE_TIME" >> "$MEMORY_REPORT"
    fi
    
    if grep -q "Elapsed (wall clock) time" "$OPTIMIZED_LOG"; then
        OPTIMIZED_TIME=$(grep "Elapsed (wall clock) time" "$OPTIMIZED_LOG" | awk '{print $5}')
        echo "优化测试时间: $OPTIMIZED_TIME" >> "$MEMORY_REPORT"
    fi
    
    # 计算内存改进
    if [ -n "$BASELINE_MEMORY" ] && [ -n "$OPTIMIZED_MEMORY" ]; then
        MEMORY_IMPROVEMENT=$((BASELINE_MEMORY - OPTIMIZED_MEMORY))
        MEMORY_IMPROVEMENT_PERCENT=$((MEMORY_IMPROVEMENT * 100 / BASELINE_MEMORY))
        echo "内存改进: ${MEMORY_IMPROVEMENT}KB (${MEMORY_IMPROVEMENT_PERCENT}%)" >> "$MEMORY_REPORT"
        
        echo -e "${GREEN}内存改进: ${MEMORY_IMPROVEMENT}KB (${MEMORY_IMPROVEMENT_PERCENT}%)${NC}"
    fi
    
    # 计算测试覆盖率保持率
    if [ -n "$BASELINE_TESTS" ] && [ -n "$OPTIMIZED_TESTS" ]; then
        COVERAGE_RETENTION=$((OPTIMIZED_TESTS * 100 / BASELINE_TESTS))
        echo "测试覆盖率保持: ${COVERAGE_RETENTION}%" >> "$MEMORY_REPORT"
        
        echo -e "${GREEN}测试覆盖率保持: ${COVERAGE_RETENTION}%${NC}"
    fi
    
    echo -e "${GREEN}结果分析完成${NC}"
}

# 生成详细报告
generate_detailed_report() {
    echo -e "${BLUE}生成详细报告...${NC}"
    
    local detailed_report="$LOG_DIR/detailed_report_$TIMESTAMP.html"
    
    cat > "$detailed_report" << EOF
<!DOCTYPE html>
<html>
<head>
    <title>内存优化验证报告 - $TIMESTAMP</title>
    <style>
        body { font-family: Arial, sans-serif; margin: 20px; }
        .header { background-color: #f0f0f0; padding: 10px; border-radius: 5px; }
        .section { margin: 20px 0; }
        .success { color: green; }
        .warning { color: orange; }
        .error { color: red; }
        table { border-collapse: collapse; width: 100%; }
        th, td { border: 1px solid #ddd; padding: 8px; text-align: left; }
        th { background-color: #f2f2f2; }
    </style>
</head>
<body>
    <div class="header">
        <h1>内存优化验证报告</h1>
        <p>生成时间: $(date)</p>
    </div>
    
    <div class="section">
        <h2>系统信息</h2>
        <pre>$(cat "$MEMORY_REPORT" | sed -n '/=== 系统信息 ===/,/=== 基准测试 ===/p')</pre>
    </div>
    
    <div class="section">
        <h2>测试结果对比</h2>
        <table>
            <tr>
                <th>指标</th>
                <th>基准测试</th>
                <th>优化测试</th>
                <th>改进</th>
            </tr>
EOF

    # 添加内存使用对比
    if [ -n "$BASELINE_MEMORY" ] && [ -n "$OPTIMIZED_MEMORY" ]; then
        cat >> "$detailed_report" << EOF
            <tr>
                <td>峰值内存使用 (KB)</td>
                <td>$BASELINE_MEMORY</td>
                <td>$OPTIMIZED_MEMORY</td>
                <td class="success">${MEMORY_IMPROVEMENT}KB (${MEMORY_IMPROVEMENT_PERCENT}%)</td>
            </tr>
EOF
    fi

    # 添加测试数量对比
    if [ -n "$BASELINE_TESTS" ] && [ -n "$OPTIMIZED_TESTS" ]; then
        cat >> "$detailed_report" << EOF
            <tr>
                <td>测试数量</td>
                <td>$BASELINE_TESTS</td>
                <td>$OPTIMIZED_TESTS</td>
                <td class="success">${COVERAGE_RETENTION}% 覆盖率保持</td>
            </tr>
EOF
    fi

    cat >> "$detailed_report" << EOF
        </table>
    </div>
    
    <div class="section">
        <h2>结论</h2>
EOF

    # 添加结论
    if [ -n "$MEMORY_IMPROVEMENT_PERCENT" ] && [ "$MEMORY_IMPROVEMENT_PERCENT" -gt 20 ]; then
        echo "<p class='success'>内存优化效果显著，内存使用减少超过20%</p>" >> "$detailed_report"
    elif [ -n "$MEMORY_IMPROVEMENT_PERCENT" ] && [ "$MEMORY_IMPROVEMENT_PERCENT" -gt 10 ]; then
        echo "<p class='warning'>内存优化效果中等，内存使用减少超过10%</p>" >> "$detailed_report"
    else
        echo "<p class='error'>内存优化效果不明显，需要进一步优化</p>" >> "$detailed_report"
    fi

    if [ -n "$COVERAGE_RETENTION" ] && [ "$COVERAGE_RETENTION" -gt 80 ]; then
        echo "<p class='success'>测试覆盖率保持良好，超过80%</p>" >> "$detailed_report"
    else
        echo "<p class='warning'>测试覆盖率较低，可能影响测试质量</p>" >> "$detailed_report"
    fi

    cat >> "$detailed_report" << EOF
    </div>
    
    <div class="section">
        <h2>详细日志</h2>
        <p><a href="baseline_$TIMESTAMP.log">基准测试日志</a></p>
        <p><a href="optimized_$TIMESTAMP.log">优化测试日志</a></p>
    </div>
</body>
</html>
EOF

    echo -e "${GREEN}详细报告已生成: $detailed_report${NC}"
}

# 运行多次测试以获得更准确的结果
run_multiple_iterations() {
    echo -e "${BLUE}运行多次测试以获得更准确的结果...${NC}"
    
    local iterations=3
    local total_baseline_memory=0
    local total_optimized_memory=0
    
    for i in $(seq 1 $iterations); do
        echo -e "${YELLOW}第 $i 次迭代...${NC}"
        
        # 运行基准测试
        if command -v /usr/bin/time >/dev/null 2>&1; then
            local baseline_iter_log="$LOG_DIR/baseline_iter_${i}_$TIMESTAMP.log"
            /usr/bin/time -v cabal test --test-show-details=always > "$baseline_iter_log" 2>&1 || true
            
            if grep -q "Maximum resident set size" "$baseline_iter_log"; then
                local iter_baseline_memory=$(grep "Maximum resident set size" "$baseline_iter_log" | awk '{print $6}')
                total_baseline_memory=$((total_baseline_memory + iter_baseline_memory))
            fi
        fi
        
        # 运行优化测试
        if command -v /usr/bin/time >/dev/null 2>&1; then
            local optimized_iter_log="$LOG_DIR/optimized_iter_${i}_$TIMESTAMP.log"
            /usr/bin/time -v ./scripts/enhanced-memory-test-config.sh --environment standard > "$optimized_iter_log" 2>&1 || true
            
            if grep -q "Maximum resident set size" "$optimized_iter_log"; then
                local iter_optimized_memory=$(grep "Maximum resident set size" "$optimized_iter_log" | awk '{print $6}')
                total_optimized_memory=$((total_optimized_memory + iter_optimized_memory))
            fi
        fi
    done
    
    # 计算平均值
    if [ $total_baseline_memory -gt 0 ] && [ $total_optimized_memory -gt 0 ]; then
        local avg_baseline=$((total_baseline_memory / iterations))
        local avg_optimized=$((total_optimized_memory / iterations))
        local avg_improvement=$((avg_baseline - avg_optimized))
        local avg_improvement_percent=$((avg_improvement * 100 / avg_baseline))
        
        echo "=== 多次迭代结果 ===" >> "$MEMORY_REPORT"
        echo "迭代次数: $iterations" >> "$MEMORY_REPORT"
        echo "平均基准内存: ${avg_baseline}KB" >> "$MEMORY_REPORT"
        echo "平均优化内存: ${avg_optimized}KB" >> "$MEMORY_REPORT"
        echo "平均内存改进: ${avg_improvement}KB (${avg_improvement_percent}%)" >> "$MEMORY_REPORT"
        
        echo -e "${GREEN}平均内存改进: ${avg_improvement}KB (${avg_improvement_percent}%)${NC}"
    fi
}

# 显示总结
show_summary() {
    echo -e "${BLUE}=== 验证总结 ===${NC}"
    echo -e "${GREEN}日志目录: $LOG_DIR${NC}"
    echo -e "${GREEN}内存报告: $MEMORY_REPORT${NC}"
    
    if [ -f "$MEMORY_REPORT" ]; then
        echo -e "${BLUE}关键指标:${NC}"
        grep -E "(内存改进|测试覆盖率保持)" "$MEMORY_REPORT" || echo "无法提取关键指标"
    fi
    
    echo ""
    echo -e "${GREEN}验证完成！${NC}"
}

# 主函数
main() {
    echo -e "${GREEN}开始内存优化效果验证${NC}"
    echo ""
    
    # 检测系统信息
    detect_system_info
    
    # 运行基准测试
    run_baseline_tests
    
    # 运行优化测试
    run_optimized_tests
    
    # 分析结果
    analyze_results
    
    # 运行多次迭代（可选）
    if [ "$1" = "--multiple" ]; then
        run_multiple_iterations
    fi
    
    # 生成详细报告
    generate_detailed_report
    
    # 显示总结
    show_summary
}

# 运行主函数
main "$@"