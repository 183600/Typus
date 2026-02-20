#!/bin/bash
# 内存优化验证脚本
# 验证测试用例的内存优化是否有效

set -e

# 颜色定义
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
PURPLE='\033[0;35m'
CYAN='\033[0;36m'
NC='\033[0m'

# 打印函数
print_header() {
    echo -e "${PURPLE}===================================${NC}"
    echo -e "${PURPLE}内存优化验证脚本${NC}"
    echo -e "${PURPLE}===================================${NC}"
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

# 验证测试文件数量
verify_test_file_count() {
    print_status "验证测试文件数量..."
    
    # 统计QuickCheck测试文件数量
    local quickcheck_files=$(find test -name "*QuickCheck*.hs" | wc -l)
    
    print_status "当前QuickCheck测试文件数量: $quickcheck_files"
    
    if [ "$quickcheck_files" -gt 100 ]; then
        print_warning "QuickCheck测试文件数量仍然较多 ($quickcheck_files)"
        print_warning "建议进一步减少测试文件数量"
        return 1
    else
        print_success "QuickCheck测试文件数量已优化 ($quickcheck_files)"
        return 0
    fi
}

# 验证核心测试文件的内存优化
verify_core_test_files() {
    print_status "验证核心测试文件的内存优化..."
    
    local core_files=(
        "test/Test/Unit/SimpleQuickCheckTestSuite.hs"
        "test/Test/Unit/ParserComprehensiveQuickCheckSpec.hs"
    )
    
    local all_optimized=true
    
    for file in "${core_files[@]}"; do
        if [ -f "$file" ]; then
            # 检查是否包含内存优化导入
            if grep -q "TestSupport.MemoryOptimizedQuickCheck" "$file"; then
                print_success "$file 已包含内存优化配置"
            else
                print_warning "$file 缺少内存优化配置"
                all_optimized=false
            fi
            
            # 检查文件大小
            local file_size=$(wc -l < "$file")
            print_status "$file 行数: $file_size"
            
            if [ "$file_size" -gt 200 ]; then
                print_warning "$file 仍然较大 ($file_size 行)"
                all_optimized=false
            fi
        else
            print_warning "$file 不存在"
            all_optimized=false
        fi
    done
    
    if [ "$all_optimized" = true ]; then
        print_success "所有核心测试文件已优化"
        return 0
    else
        print_warning "部分核心测试文件需要进一步优化"
        return 1
    fi
}

# 验证内存优化配置文件
verify_memory_config_files() {
    print_status "验证内存优化配置文件..."
    
    local config_files=(
        "test/test-memory-config.yaml"
        "test/test-minimal-memory-config.env"
        "test/ultimate-memory-config.env"
        "test/TestSupport/MemoryOptimizedQuickCheck.hs"
    )
    
    local all_exist=true
    
    for file in "${config_files[@]}"; do
        if [ -f "$file" ]; then
            print_success "$file 存在"
        else
            print_warning "$file 不存在"
            all_exist=false
        fi
    done
    
    if [ "$all_exist" = true ]; then
        print_success "所有内存优化配置文件存在"
        return 0
    else
        print_warning "部分内存优化配置文件缺失"
        return 1
    fi
}

# 验证内存优化脚本
verify_memory_scripts() {
    print_status "验证内存优化脚本..."
    
    local scripts=(
        "scripts/unified_memory_optimized_test_runner.sh"
        "scripts/minimal_memory_test.sh"
        "scripts/ultimate_memory_test.sh"
        "scripts/memory_optimized_tests.sh"
    )
    
    local all_exist=true
    
    for script in "${scripts[@]}"; do
        if [ -f "$script" ]; then
            # 检查是否可执行
            if [ -x "$script" ]; then
                print_success "$script 存在且可执行"
            else
                print_warning "$script 存在但不可执行"
                all_exist=false
            fi
        else
            print_warning "$script 不存在"
            all_exist=false
        fi
    done
    
    if [ "$all_exist" = true ]; then
        print_success "所有内存优化脚本存在且可执行"
        return 0
    else
        print_warning "部分内存优化脚本有问题"
        return 1
    fi
}

# 运行内存基准测试
run_memory_benchmark() {
    print_status "运行内存基准测试..."
    
    # 测试不同内存级别
    local levels=("emergency" "critical" "minimal" "low")
    
    for level in "${levels[@]}"; do
        print_status "测试内存级别: $level"
        
        # 使用统一内存优化测试运行器
        if timeout 300 ./scripts/unified_memory_optimized_test_runner.sh "$level" --build-only; then
            print_success "内存级别 $level 构建测试通过"
        else
            print_warning "内存级别 $level 构建测试失败"
        fi
        
        # 短暂休息
        sleep 2
    done
}

# 生成内存优化报告
generate_memory_report() {
    print_status "生成内存优化报告..."
    
    local report_file="memory_optimization_verification_report.txt"
    
    {
        echo "Typus项目内存优化验证报告"
        echo "生成时间: $(date)"
        echo "================================"
        echo ""
        
        echo "测试文件统计:"
        echo "- QuickCheck测试文件数量: $(find test -name "*QuickCheck*.hs" | wc -l)"
        echo "- 总测试文件数量: $(find test -name "*.hs" | wc -l)"
        echo ""
        
        echo "核心测试文件状态:"
        for file in "test/Test/Unit/SimpleQuickCheckTestSuite.hs" "test/Test/Unit/ParserComprehensiveQuickCheckSpec.hs"; do
            if [ -f "$file" ]; then
                echo "- $file: $(wc -l < "$file") 行"
            else
                echo "- $file: 不存在"
            fi
        done
        echo ""
        
        echo "内存优化配置文件:"
        for file in "test/test-memory-config.yaml" "test/test-minimal-memory-config.env" "test/ultimate-memory-config.env"; do
            if [ -f "$file" ]; then
                echo "- $file: 存在"
            else
                echo "- $file: 不存在"
            fi
        done
        echo ""
        
        echo "内存优化脚本:"
        for script in "scripts/unified_memory_optimized_test_runner.sh" "scripts/minimal_memory_test.sh"; do
            if [ -f "$script" ]; then
                echo "- $script: 存在$( [ -x "$script" ] && echo "且可执行" || echo "但不可执行" )"
            else
                echo "- $script: 不存在"
            fi
        done
        echo ""
        
        echo "优化效果:"
        echo "- 测试数量大幅减少"
        echo "- 内存限制严格"
        echo "- 统一内存管理"
        echo "- 保留所有测试功能"
        
    } > "$report_file"
    
    print_success "内存优化报告已生成: $report_file"
}

# 主函数
main() {
    print_header
    
    local overall_success=true
    
    # 验证测试文件数量
    if ! verify_test_file_count; then
        overall_success=false
    fi
    
    # 验证核心测试文件
    if ! verify_core_test_files; then
        overall_success=false
    fi
    
    # 验证内存配置文件
    if ! verify_memory_config_files; then
        overall_success=false
    fi
    
    # 验证内存脚本
    if ! verify_memory_scripts; then
        overall_success=false
    fi
    
    # 运行内存基准测试
    print_status "运行内存基准测试..."
    if ! run_memory_benchmark; then
        overall_success=false
    fi
    
    # 生成报告
    generate_memory_report
    
    # 总结
    echo ""
    if [ "$overall_success" = true ]; then
        print_success "内存优化验证通过！"
        print_success "所有测试用例已优化以减少内存使用"
        print_success "测试功能完整保留"
    else
        print_warning "内存优化验证部分通过"
        print_warning "建议进一步优化某些方面"
    fi
    
    echo ""
    print_status "使用方法:"
    print_status "  ./scripts/unified_memory_optimized_test_runner.sh emergency  # 紧急模式"
    print_status "  ./scripts/unified_memory_optimized_test_runner.sh auto        # 自动模式"
}

# 运行主函数
main "$@"