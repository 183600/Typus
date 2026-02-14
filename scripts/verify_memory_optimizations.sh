#!/bin/bash
# 内存优化验证脚本
# 比较优化前后的内存使用情况

set -e

# 颜色代码
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
PURPLE='\033[0;35m'
CYAN='\033[0;36m'
NC='\033[0m'

print_header() {
    echo -e "${PURPLE}========================================${NC}"
    echo -e "${PURPLE}内存优化验证脚本${NC}"
    echo -e "${PURPLE}========================================${NC}"
    echo ""
}

print_status() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

print_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

print_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1"
}

print_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

print_config() {
    echo -e "${CYAN}[CONFIG]${NC} $1"
}

# 帮助函数
show_help() {
    echo "内存优化验证脚本 for Typus"
    echo ""
    echo "用法: $0 [选项]"
    echo ""
    echo "选项:"
    echo "  --verbose, -v     启用详细输出"
    echo "  --help, -h        显示此帮助信息"
    echo "  --baseline        建立基准测试（优化前）"
    echo "  --compare         比较优化效果"
    echo "  --quick           快速验证（仅运行关键测试）"
    echo ""
    echo "环境变量:"
    echo "  TYPUS_VERBOSE     启用详细输出"
    echo "  TYPUS_TIMEOUT     测试超时时间（秒）"
    echo ""
    echo "内存级别:"
    echo "  nano        - 极度受限环境 (<64MB)"
    echo "  micro       - 微型环境 (64-128MB)"
    echo "  tiny        - 超轻量环境 (128-256MB)"
    echo "  lightweight - 轻量环境 (256-512MB)"
    echo "  standard    - 标准环境 (>512MB)"
}

# 获取进程内存使用情况
get_process_memory() {
    local pid=$1
    if command -v ps >/dev/null 2>&1; then
        ps -p $pid -o rss= 2>/dev/null | tr -d ' ' || echo "0"
    else
        echo "0"
    fi
}

# 获取系统可用内存
get_available_memory() {
    if command -v free >/dev/null 2>&1; then
        free -m | awk 'NR==2{printf "%.0f", $7}'
    elif command -v vm_stat >/dev/null 2>&1; then
        local page_size=$(vm_stat | head -1 | sed 's/.*page size of \([0-9]*\).*/\1/')
        local free_pages=$(vm_stat | awk '/free/ {gsub(/\./, ""); print $3}')
        echo $((free_pages * page_size / 1024 / 1024))
    else
        echo "1024"
    fi
}

# 运行测试并监控内存使用
run_test_with_memory_monitoring() {
    local test_name="$1"
    local test_command="$2"
    local timeout="${3:-120}"
    
    print_status "运行测试: $test_name"
    print_config "命令: $test_command"
    print_config "超时: ${timeout}秒"
    
    # 记录测试前的内存状态
    local before_memory=$(get_available_memory)
    print_config "测试前可用内存: ${before_memory}MB"
    
    # 运行测试并监控内存
    local temp_file=$(mktemp)
    local start_time=$(date +%s)
    
    # 在后台运行测试并监控内存
    eval "$test_command" > "$temp_file" 2>&1 &
    local test_pid=$!
    
    local max_memory=0
    local current_time=$start_time
    local end_time=$((start_time + timeout))
    
    # 监控内存使用
    while [ $current_time -lt $end_time ]; do
        if kill -0 $test_pid 2>/dev/null; then
            local current_memory=$(get_process_memory $test_pid)
            if [ "$current_memory" -gt "$max_memory" ]; then
                max_memory=$current_memory
            fi
            sleep 1
            current_time=$(date +%s)
        else
            break
        fi
    done
    
    # 检查测试是否仍在运行
    if kill -0 $test_pid 2>/dev/null; then
        print_warning "测试超时，终止进程"
        kill -TERM $test_pid 2>/dev/null || true
        sleep 2
        kill -KILL $test_pid 2>/dev/null || true
    fi
    
    # 等待进程结束并获取退出码
    wait $test_pid 2>/dev/null
    local exit_code=$?
    
    # 记录测试后的内存状态
    local after_memory=$(get_available_memory)
    local end_time=$(date +%s)
    local duration=$((end_time - start_time))
    
    # 计算内存使用
    local max_memory_mb=$((max_memory / 1024))
    local memory_diff=$((before_memory - after_memory))
    
    # 输出结果
    echo "测试结果: $test_name"
    echo "  退出码: $exit_code"
    echo "  持续时间: ${duration}秒"
    echo "  最大内存使用: ${max_memory_mb}MB"
    echo "  内存差异: ${memory_diff}MB"
    
    if [ $exit_code -eq 0 ]; then
        print_success "测试通过: $test_name"
    else
        print_error "测试失败: $test_name"
        echo "错误输出:"
        cat "$temp_file" | head -20
    fi
    
    # 清理
    rm -f "$temp_file"
    
    # 返回结果
    echo "$exit_code,${max_memory_mb},${duration},${memory_diff}"
}

# 运行基准测试
run_baseline_tests() {
    print_status "运行基准测试（优化前）..."
    
    # 构建标准测试套件
    print_status "构建标准测试套件..."
    if cabal build typus-test; then
        print_success "构建成功"
    else
        print_error "构建失败"
        return 1
    fi
    
    # 运行标准测试并记录内存使用
    print_status "运行标准测试..."
    local baseline_result=$(run_test_with_memory_monitoring "标准测试" "cabal test typus-test" 300)
    
    # 保存基准结果
    echo "$baseline_result" > /tmp/typus_memory_baseline.txt
    print_success "基准测试完成，结果已保存"
}

# 运行优化测试
run_optimized_tests() {
    print_status "运行优化测试..."
    
    # 构建适应性测试运行器
    print_status "构建适应性测试运行器..."
    if cabal build adaptive-memory-test-runner; then
        print_success "构建成功"
    else
        print_error "构建失败"
        return 1
    fi
    
    # 运行优化测试并记录内存使用
    print_status "运行适应性内存测试..."
    local optimized_result=$(run_test_with_memory_monitoring "适应性测试" "cabal run adaptive-memory-test-runner" 180)
    
    # 保存优化结果
    echo "$optimized_result" > /tmp/typus_memory_optimized.txt
    print_success "优化测试完成，结果已保存"
}

# 比较测试结果
compare_results() {
    print_status "比较测试结果..."
    
    if [ ! -f /tmp/typus_memory_baseline.txt ]; then
        print_error "未找到基准测试结果，请先运行 --baseline"
        return 1
    fi
    
    if [ ! -f /tmp/typus_memory_optimized.txt ]; then
        print_error "未找到优化测试结果，请先运行优化测试"
        return 1
    fi
    
    # 读取结果
    local baseline=$(cat /tmp/typus_memory_baseline.txt)
    local optimized=$(cat /tmp/typus_memory_optimized.txt)
    
    # 解析结果
    IFS=',' read -r baseline_exit baseline_memory baseline_duration baseline_diff <<< "$baseline"
    IFS=',' read -r optimized_exit optimized_memory optimized_duration optimized_diff <<< "$optimized"
    
    # 输出比较结果
    echo ""
    echo "=== 内存优化效果比较 ==="
    echo ""
    echo "基准测试（优化前）:"
    echo "  退出码: $baseline_exit"
    echo "  最大内存使用: ${baseline_memory}MB"
    echo "  持续时间: ${baseline_duration}秒"
    echo "  内存差异: ${baseline_diff}MB"
    echo ""
    echo "优化测试（优化后）:"
    echo "  退出码: $optimized_exit"
    echo "  最大内存使用: ${optimized_memory}MB"
    echo "  持续时间: ${optimized_duration}秒"
    echo "  内存差异: ${optimized_diff}MB"
    echo ""
    
    # 计算改进
    local memory_improvement=$((baseline_memory - optimized_memory))
    local memory_improvement_percent=0
    if [ "$baseline_memory" -gt 0 ]; then
        memory_improvement_percent=$((memory_improvement * 100 / baseline_memory))
    fi
    
    local duration_improvement=$((baseline_duration - optimized_duration))
    local duration_improvement_percent=0
    if [ "$baseline_duration" -gt 0 ]; then
        duration_improvement_percent=$((duration_improvement * 100 / baseline_duration))
    fi
    
    echo "改进效果:"
    echo "  内存使用减少: ${memory_improvement}MB (${memory_improvement_percent}%)"
    echo "  持续时间减少: ${duration_improvement}秒 (${duration_improvement_percent}%)"
    
    # 判断优化是否成功
    if [ "$optimized_exit" -eq 0 ] && [ "$memory_improvement" -gt 0 ]; then
        print_success "内存优化成功！"
    elif [ "$optimized_exit" -eq 0 ]; then
        print_warning "测试通过但内存使用未减少"
    else
        print_error "优化测试失败"
    fi
}

# 快速验证
run_quick_verification() {
    print_status "运行快速验证..."
    
    # 检查基本构建
    print_status "检查适应性测试运行器构建..."
    if cabal build adaptive-memory-test-runner; then
        print_success "构建成功"
    else
        print_error "构建失败"
        return 1
    fi
    
    # 运行简单的内存测试
    print_status "运行简单内存测试..."
    local test_result=$(run_test_with_memory_monitoring "快速测试" "cabal run adaptive-memory-test-runner -- --force-level nano" 60)
    
    # 解析结果
    IFS=',' read -r exit_code memory duration diff <<< "$test_result"
    
    if [ "$exit_code" -eq 0 ]; then
        print_success "快速验证通过！"
        print_config "内存使用: ${memory}MB"
        print_config "持续时间: ${duration}秒"
    else
        print_error "快速验证失败"
        return 1
    fi
}

# 主执行逻辑
main() {
    local baseline="false"
    local compare="false"
    local quick="false"
    local verbose="false"
    
    # 解析命令行参数
    while [[ $# -gt 0 ]]; do
        case $1 in
            --help|-h)
                show_help
                exit 0
                ;;
            --verbose|-v)
                verbose="true"
                shift
                ;;
            --baseline)
                baseline="true"
                shift
                ;;
            --compare)
                compare="true"
                shift
                ;;
            --quick)
                quick="true"
                shift
                ;;
            *)
                print_error "未知选项: $1"
                echo "使用 --help 查看使用信息"
                exit 1
                ;;
        esac
    done
    
    # 检查环境变量
    if [ "$TYPUS_VERBOSE" = "true" ]; then
        verbose="true"
    fi
    
    # 打印标题
    print_header
    
    # 显示系统信息
    print_status "系统信息:"
    print_config "可用内存: $(get_available_memory)MB"
    print_config "当前工作目录: $(pwd)"
    echo ""
    
    # 执行操作
    if [ "$quick" = "true" ]; then
        run_quick_verification
    elif [ "$baseline" = "true" ]; then
        run_baseline_tests
    elif [ "$compare" = "true" ]; then
        compare_results
    else
        # 默认执行完整验证流程
        print_status "执行完整验证流程..."
        run_baseline_tests
        echo ""
        run_optimized_tests
        echo ""
        compare_results
    fi
}

# 运行主函数
main "$@"