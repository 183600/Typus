#!/usr/bin/env python3
import os
import re

# 需要处理的文件列表
files_to_fix = [
    "test/Test/Unit/TextProcessingAdvancedSpec.hs",
    "test/Test/Unit/ToolingErrorSpec.hs",
    "test/Test/Unit/TypeInferenceAdvancedSpec.hs",
    "test/Test/Unit/DependenciesCycleDetectionQuickCheckSpec.hs",
    "test/Test/Unit/NewAdditionalErrorHandlerQuickCheckSpec.hs",
    "test/Test/Unit/CoreUtilsQuickCheckSpec.hs",
    "test/Test/Unit/CoreSourceLocationQuickCheckSpec.hs",
    "test/Test/Unit/CoreParserQuickCheckSpec.hs",
    "test/Test/Unit/CoreOwnershipQuickCheckSpec.hs",
    "test/Test/Unit/CoreDependenciesQuickCheckSpec.hs",
    "test/Test/Unit/CoreCompilerQuickCheckSpec.hs",
    "test/Test/Unit/ComprehensiveCoreModulesQuickCheckSpec.hs"
]

def fix_unused_imports(file_path):
    """移除未使用的导入"""
    try:
        with open(file_path, 'r') as f:
            content = f.read()
        
        # 移除常见的未使用导入
        unused_imports = [
            "import Test.Tasty",
            "import Test.Tasty.QuickCheck",
            "import Test.Tasty.HUnit",
            "import Test.QuickCheck",
            "import Data.List",
            "import Data.Maybe",
            "import Data.Char",
            "import Control.Monad",
            "import qualified Data.Text as T",
            "import qualified Data.Map.Strict as Map",
            "import SourceLocation",
            "import Compiler.Errors.Core",
            "import TestSupport.QuickCheck",
            "import TestSupport.Arbitrary",
            "import ErrorHandler",
            "import Utils",
            "import Tooling.Error"
        ]
        
        for imp in unused_imports:
            # 移除导入行，包括可能的类型导入
            pattern = rf"{imp}.*?\n"
            content = re.sub(pattern, "", content, flags=re.MULTILINE)
        
        with open(file_path, 'w') as f:
            f.write(content)
        print(f"Fixed unused imports in {file_path}")
    except Exception as e:
        print(f"Error fixing {file_path}: {e}")

def fix_unused_variables(file_path):
    """修复未使用的变量"""
    try:
        with open(file_path, 'r') as f:
            content = f.read()
        
        # 将未使用的变量添加下划线前缀
        # 处理 Left err 模式
        content = re.sub(r"Left\s+(err|error|result|valid|typeExpr|constraint|ast|stmt|directives|nodes|checker|substitution|scheme|env|program)", r"Left _\1", content)
        
        # 处理 Right valid 模式
        content = re.sub(r"Right\s+(valid|err|error|result|file|typeExpr)", r"Right _\1", content)
        
        # 处理 let 绑定
        content = re.sub(r"(\s+)(err|error|result|valid|typeExpr|constraint|ast|stmt|directives|nodes|checker|substitution|scheme|env|program|cache|cachedResult|pos|span|var2|vars1|vars2|context|message|endLine|endCol|endOffset|after)\s+=", r"\1_\2 =", content)
        
        # 处理 forAll 模式
        content = re.sub(r"forAll\s+\w+\s+\$\\(\w+)\s+->", r"forAll \w+ $ \\\1 ->", content)
        
        with open(file_path, 'w') as f:
            f.write(content)
        print(f"Fixed unused variables in {file_path}")
    except Exception as e:
        print(f"Error fixing {file_path}: {e}")

def fix_name_shadowing(file_path):
    """修复名称遮蔽"""
    try:
        with open(file_path, 'r') as f:
            content = f.read()
        
        # 处理 span 变量遮蔽
        content = re.sub(r"(\\s+)(span)\s+=", r"\1span' =", content)
        content = re.sub(r"forAll\s+\w+\s+\$\\(span\)\s+->", r"forAll \w+ $ \\span' ->", content)
        
        # 处理 error 变量遮蔽
        content = re.sub(r"(\\s+)(error)\s+=", r"\1error' =", content)
        
        # 处理 context 和 message 变量遮蔽
        content = re.sub(r"prop_error_context\s+(context)\s+(message)\s+", r"prop_error_context context' message' ", content)
        
        with open(file_path, 'w') as f:
            f.write(content)
        print(f"Fixed name shadowing in {file_path}")
    except Exception as e:
        print(f"Error fixing {file_path}: {e}")

def fix_type_defaults(file_path):
    """修复类型默认化"""
    try:
        with open(file_path, 'r') as f:
            content = f.read()
        
        # 添加类型注解
        content = re.sub(r"count\s+'(\w)'\s+xs\s+==\s+count\s+'\1'\s+xs", r"count '\1' xs == (count '\1' xs :: Int)", content)
        
        with open(file_path, 'w') as f:
            f.write(content)
        print(f"Fixed type defaults in {file_path}")
    except Exception as e:
        print(f"Error fixing {file_path}: {e}")

# 处理所有文件
for file_path in files_to_fix:
    if os.path.exists(file_path):
        fix_unused_imports(file_path)
        fix_unused_variables(file_path)
        fix_name_shadowing(file_path)
        fix_type_defaults(file_path)
    else:
        print(f"File not found: {file_path}")

print("All fixes completed!")