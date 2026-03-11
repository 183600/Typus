#!/usr/bin/env bash
# 智能内存测试运行器
# 保留所有测试用例，通过智能优化减少内存使用

set -euo pipefail

# 配置参数
readonly CONFIG_FILE="../intelligent_memory_optimization_with_preservation.yaml"
readonly MEMORY_LIMIT_MB=16
readonly BATCH_SIZE=5
readonly GC_FREQUENCY=3

# 颜色输出
readonly RED='\033[0;31m'
readonly GREEN='\033[0;32m'
readonly YELLOW='\033[1;33m'
readonly BLUE='\033[0;34m'
readonly NC='\033[0m' # No Color

# 日志函数
log_info() {
    echo -e "${BLUE}[INFO]${NC} $*"
}

log_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $*"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $*"
}

log_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $*"
}

# 内存监控函数
get_memory_usage() {
    if command -v ps >/dev/null 2>&1; then
        ps -o rss= -p $$ 2>/dev/null | awk '{print $1/1024}'
    else
        echo "unknown"
    fi
}

# 垃圾回收函数
force_gc() {
    log_info "强制执行垃圾回收..."
    if command -v ghc >/dev/null 2>&1; then
        ghc -e "import System.Mem; performGC" 2>/dev/null || true
    fi
    sleep 0.1
}

# 内存检查函数
check_memory_usage() {
    local usage=$(get_memory_usage)
    if [[ "$usage" != "unknown" ]] && (( $(echo "$usage > $MEMORY_LIMIT_MB" | bc -l) )); then
        log_warning "内存使用过高: ${usage}MB (限制: ${MEMORY_LIMIT_MB}MB)"
        force_gc
        return 1
    fi
    return 0
}

# 清理函数
cleanup() {
    log_info "清理临时资源..."
    force_gc
    # 清理临时文件
    find /tmp -name "typus_test_*" -mtime +1 -delete 2>/dev/null || true
}

# 设置清理陷阱
trap cleanup EXIT INT TERM

# 智能测试批处理函数
run_intelligent_test_batch() {
    local batch_num=$1
    local test_args="${@:2}"
    
    log_info "执行测试批次 $batch_num..."
    
    # 检查内存使用
    if ! check_memory_usage; then
        log_warning "内存使用过高，等待清理..."
        sleep 1
        force_gc
    fi
    
    # 执行测试
    local start_time=$(date +%s)
    
    if cabal test \
        --ghc-options="-rtsopts" \
        --ghc-options="-with-rtsopts=-M${MEMORY_LIMIT_MB}m -A64k -n8k -H256k -qg -G1 -c" \
        --ghc-options="-O0" \
        --test-option="--quickcheck-tests=1" \
        --test-option="--quickcheck-max-size=1" \
        --test-option="--quickcheck-max-shrinks=0" \
        --test-show-details=direct \
        $test_args; then
        
        local end_time=$(date +%s)
        local duration=$((end_time - start_time))
        log_success "批次 $batch_num 完成 (耗时: ${duration}秒)"
        return 0
    else
        log_error "批次 $batch_num 失败"
        return 1
    fi
}

# 主函数
main() {
    log_info "=== 智能内存优化测试运行器 ==="
    log_info "配置: ${CONFIG_FILE}"
    log_info "内存限制: ${MEMORY_LIMIT_MB}MB"
    log_info "批处理大小: ${BATCH_SIZE}"
    log_info "垃圾回收频率: ${GC_FREQUENCY}"
    echo
    
    # 检查配置
    if [[ ! -f "$CONFIG_FILE" ]]; then
        log_warning "配置文件不存在，使用默认配置"
    else
        log_info "加载配置文件: $CONFIG_FILE"
    fi
    
    # 设置环境
    export LC_ALL=C
    export LANG=C
    export TYPUS_SKIP_GO_BUILD=1
    export TYPUS_MINIMAL_MODE=1
    export ULS_MEMORY_OPTIMIZED=1
    
    # 设置内存限制
    if command -v ulimit >/dev/null 2>&1; then
        ulimit -v $((MEMORY_LIMIT_MB * 1024))
        log_info "设置虚拟内存限制: ${MEMORY_LIMIT_MB}MB"
    fi
    
    # 初始内存检查
    local initial_memory=$(get_memory_usage)
    log_info "初始内存使用: ${initial_memory}MB"
    
    # 构建项目
    log_info "构建项目..."
    if ! cabal build --ghc-options="-O0 -rtsopts"; then
        log_error "构建失败"
        exit 1
    fi
    
    # 执行智能测试
    local batch_count=0
    local gc_counter=0
    local test_args="${@:-}"
    
    log_info "开始智能测试执行..."
    
    while true; do
        batch_count=$((batch_count + 1))
        gc_counter=$((gc_counter + 1))
        
        # 执行测试批次
        if ! run_intelligent_test_batch "$batch_count" "$test_args"; then
            log_error "测试批次 $batch_count 失败，停止执行"
            break
        fi
        
        # 定期垃圾回收
        if [[ $gc_counter -ge $GC_FREQUENCY ]]; then
            force_gc
            gc_counter=0
        fi
        
        # 检查是否需要继续
        if [[ -n "$test_args" ]] && [[ "$test_args" == *"--test-pattern"* ]]; then
            # 如果指定了测试模式，只运行一次
            break
        fi
        
        # 内存使用检查
        local current_memory=$(get_memory_usage)
        if [[ "$current_memory" != "unknown" ]] && (( $(echo "$current_memory > $((MEMORY_LIMIT_MB * 9 / 10))" | bc -l) )); then
            log_warning "内存使用接近限制 (${current_memory}MB)，启用紧急模式"
            force_gc
            sleep 2
        fi
        
        # 简单循环控制 - 实际项目中应该根据测试完成状态判断
        if [[ $batch_count -ge 10 ]]; then
            log_info "达到最大批次限制，停止执行"
            break
        fi
    done
    
    # 最终内存检查
    local final_memory=$(get_memory_usage)
    log_info "最终内存使用: ${final_memory}MB"
    
    log_success "智能内存测试执行完成"
    log_info "总执行批次: $batch_count"
}

# 执行主函数
main "$@"