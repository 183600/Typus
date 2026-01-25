#!/usr/bin/env python3
import os

# 核心测试文件列表
core_test_files = [
    "test/Test/Unit/CoreDependenciesQuickCheckSpec.hs",
    "test/Test/Unit/CoreOwnershipQuickCheckSpec.hs", 
    "test/Test/Unit/CoreParserQuickCheckSpec.hs",
    "test/Test/Unit/CoreSourceLocationQuickCheckSpec.hs",
    "test/Test/Unit/CoreUtilsQuickCheckSpec.hs",
    "test/Test/Unit/DependenciesCycleDetectionQuickCheckSpec.hs"
]

def fix_core_imports(file_path):
    """修复核心测试文件的导入问题"""
    try:
        with open(file_path, 'r') as f:
            content = f.read()
        
        # 检查是否已有Test.Tasty导入
        if "import Test.Tasty" not in content:
            # 找到第一个import位置
            lines = content.split('\n')
            import_index = -1
            for i, line in enumerate(lines):
                if line.strip().startswith('import '):
                    import_index = i
                    break
            
            if import_index >= 0:
                # 在第一个import前添加Test.Tasty导入
                lines.insert(import_index, "import Test.Tasty")
                content = '\n'.join(lines)
        
        # 确保有Test.Tasty.QuickCheck导入
        if "import Test.Tasty.QuickCheck" not in content:
            lines = content.split('\n')
            import_index = -1
            for i, line in enumerate(lines):
                if line.strip().startswith('import Test.Tasty'):
                    import_index = i
                    break
            
            if import_index >= 0:
                lines.insert(import_index + 1, "import Test.Tasty.QuickCheck")
                content = '\n'.join(lines)
        
        # 添加Data.List导入（如果需要nub或sort）
        if "nub" in content or "sort" in content:
            if "import Data.List" not in content:
                lines = content.split('\n')
                import_index = -1
                for i, line in enumerate(lines):
                    if line.strip().startswith('import '):
                        import_index = i
                        break
                
                if import_index >= 0:
                    lines.insert(import_index, "import Data.List (nub, sort)")
                    content = '\n'.join(lines)
        
        with open(file_path, 'w') as f:
            f.write(content)
        print(f"Fixed core imports in {file_path}")
    except Exception as e:
        print(f"Error fixing {file_path}: {e}")

# 处理所有核心文件
for file_path in core_test_files:
    if os.path.exists(file_path):
        fix_core_imports(file_path)
    else:
        print(f"File not found: {file_path}")

print("All core fixes completed!")