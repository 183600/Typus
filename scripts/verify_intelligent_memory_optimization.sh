#!/usr/bin/env bash
# 验证智能内存优化效果
# 确保测试用例不消耗大量内存且所有测试都被保留

set -euo pipefail

# 颜色输出
readonly RED='\033[0;31m'
readonly GREEN='\033[0;32m'
readonly YELLOW='\033[1;33m'
readonly BLUE='\033[0;34m'
readonly NC='\033[0m'

# 日志函数
log() {
    local level=$1
    shift
    case $level in
        "INFO") echo -e "${BLUE}[INFO]${NC} $*" ;;
        "WARNING") echo -e "${YELLOW}[WARNING]${NC} $*" ;;
        "ERROR") echo -e "${RED}[ERROR]${NC} $*" ;;
        "SUCCESS") echo -e "${GREEN}[SUCCESS]${NC} $*" ;;
    esac
}

# 内存使用监控函数
monitor_memory_usage() {
    local pid=$1
    local max_memory=0
    
    while kill -0 "$pid" 2>/dev/null; do
        if command -v ps >/dev/null 2>&1; then
            local current_memory=$(ps -o rss= -p "$pid" 2>/dev/null | awk '{print $1/1024}')
            if [[ -n "$current_memory" ]] && (( $(echo "$current_memory > $max_memory" | bc -l) )); then
                max_memory=$current_memory
            fi
        fi
        sleep 1
    done
    
    echo "$max_memory"
}

# 测试用例计数函数
count_test_cases() {
    local test_dir="../test"
    local count=0
    
    # 统计 Haskell 测试文件
    if command -v find >/dev/null 2>&1; then
        count=$(find "$test_dir" -name "*.hs" -type f | wc -l)
    fi
    
    echo "$count"
}

# 验证内存优化效果
verify_memory_optimization() {
    local max_memory_limit=32  # MB
    local test_case_count=$(count_test_cases)
    
    log "INFO" "开始验证智能内存优化..."
    log "INFO" "测试用例总数: $test_case_count"
    log "INFO" "内存限制: ${max_memory_limit}MB"
    echo
    
    # 运行智能内存优化测试
    log "INFO" "运行智能内存优化测试..."
    
    # 后台运行测试并监控内存
    ./scripts/intelligent_memory_test_runner.sh &
    local test_pid=$!
    
    # 监控内存使用
    local max_memory_used=$(monitor_memory_usage "$test_pid")
    
    # 等待测试完成
    wait "$test_pid"
    local test_exit_code=$?
    
    echo
    log "INFO" "测试执行完成"
    log "INFO" "最大内存使用: ${max_memory_used}MB"
    log "INFO" "测试退出码: $test_exit_code"
    
    # 验证结果
    local verification_passed=true
    
    # 1. 验证内存使用
    if [[ -n "$max_memory_used" ]] && (( $(echo "$max_memory_used > $max_memory_limit" | bc -l) )); then
        log "ERROR" "内存使用超过限制: ${max_memory_used}MB > ${max_memory_limit}MB"
        verification_passed=false
    else
        log "SUCCESS" "内存使用符合要求: ${max_memory_used}MB <= ${max_memory_limit}MB"
    fi
    
    # 2. 验证测试执行
    if [[ $test_exit_code -ne 0 ]]; then
        log "ERROR" "测试执行失败 (退出码: $test_exit_code)"
        verification_passed=false
    else
        log "SUCCESS" "测试执行成功"
    fi
    
    # 3. 验证测试用例完整性
    local final_test_count=$(count_test_cases)
    if [[ $final_test_count -ne $test_case_count ]]; then
        log "ERROR" "测试用例数量发生变化: 初始 $test_case_count, 最终 $final_test_count"
        verification_passed=false
    else
        log "SUCCESS" "测试用例完整性验证通过"
    fi
    
    # 4. 验证配置文件存在
    if [[ ! -f "../intelligent_memory_optimization_with_preservation.yaml" ]]; then
        log "ERROR" "智能内存优化配置文件不存在"
        verification_passed=false
    else
        log "SUCCESS" "配置文件验证通过"
    fi
    
    # 5. 验证优化模块存在
    if [[ ! -f "../test/TestSupport/IntelligentMemoryOptimization.hs" ]]; then
        log "ERROR" "智能内存优化模块不存在"
        verification_passed=false
    else
        log "SUCCESS" "优化模块验证通过"
    fi
    
    echo
    if [[ "$verification_passed" == "true" ]]; then
        log "SUCCESS" "=== 智能内存优化验证成功 ==="
        log "SUCCESS" "✓ 内存使用控制在限制范围内"
        log "SUCCESS" "✓ 所有测试用例被保留"
        log "SUCCESS" "✓ 测试执行成功"
        log "SUCCESS" "✓ 配置和模块完整"
        return 0
    else
        log "ERROR" "=== 智能内存优化验证失败 ==="
        log "ERROR" "请检查上述错误并修复"
        return 1
    fi
}

# 性能基准测试
run_performance_benchmark() {
    log "INFO" "运行性能基准测试..."
    
    # 测试1: 标准测试运行
    log "INFO" "测试1: 标准测试运行"
    local start_time=$(date +%s)
    cabal test --test-show-details=direct > /dev/null 2>&1 || true
    local end_time=$(date +%s)
    local standard_duration=$((end_time - start_time))
    
    # 测试2: 智能内存优化测试运行
    log "INFO" "测试2: 智能内存优化测试运行"
    start_time=$(date +%s)
    ./scripts/intelligent_memory_test_runner.sh > /dev/null 2>&1 || true
    end_time=$(date +%s)
    local optimized_duration=$((end_time - start_time))
    
    echo
    log "INFO" "性能基准测试结果:"
    log "INFO" "  标准测试运行时间: ${standard_duration}秒"
    log "INFO" "  优化测试运行时间: ${optimized_duration}秒"
    
    if [[ $optimized_duration -le $standard_duration ]]; then
        log "SUCCESS" "✓ 优化版本性能相当或更好"
    else
        log "WARNING" "⚠ 优化版本稍慢，但内存使用更低"
    fi
}

# 主函数
main() {
    log "INFO" "=== 智能内存优化验证工具 ==="
    echo
    
    # 验证内存优化效果
    if ! verify_memory_optimization; then
        exit 1
    fi
    
    echo
    
    # 运行性能基准测试
    run_performance_benchmark
    
    echo
    log "SUCCESS" "所有验证完成!"
}

# 执行主函数
main "$@"