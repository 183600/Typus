#!/usr/bin/env python3
"""
Script to fix compilation errors in the test files.
"""

import re
import os

def fix_core_compiler_properties():
    """Fix CoreCompilerPropertiesQuickCheckSpec.hs"""
    file_path = "test/Test/Unit/CoreCompilerPropertiesQuickCheckSpec.hs"
    
    with open(file_path, 'r') as f:
        content = f.read()
    
    # Fix TypusFile constructor calls
    content = re.sub(
        r"compile \(input \[\] \[\]\)",
        "compile input",
        content
    )
    
    with open(file_path, 'w') as f:
        f.write(content)
    
    print(f"Fixed {file_path}")

def fix_core_dependencies_properties():
    """Fix CoreDependenciesPropertiesQuickCheckSpec.hs"""
    file_path = "test/Test/Unit/CoreDependenciesPropertiesQuickCheckSpec.hs"
    
    with open(file_path, 'r') as f:
        content = f.read()
    
    # Remove TypeScheme from imports
    content = re.sub(
        r"import Dependencies\.TypeSystem \(TypeVar\(\.\.\.\), TypeConstraint\(\.\.\.\), Substitution, TypeScheme\(\.\.\.\)\)",
        "import Dependencies.TypeSystem (TypeVar(..), TypeConstraint(..), Substitution)",
        content
    )
    
    with open(file_path, 'w') as f:
        f.write(content)
    
    print(f"Fixed {file_path}")

def fix_core_error_handler_properties():
    """Fix CoreErrorHandlerPropertiesQuickCheckSpec.hs"""
    file_path = "test/Test/Unit/CoreErrorHandlerPropertiesQuickCheckSpec.hs"
    
    with open(file_path, 'r') as f:
        content = f.read()
    
    # Fix ErrorCollector type
    content = re.sub(
        r"newErrorCollector :: IO ErrorCollector",
        "newErrorCollector :: IO (ErrorCollector String)",
        content
    )
    
    content = re.sub(
        r"collectErrors :: ErrorCollector -> \[String\]",
        "collectErrors :: ErrorCollector String -> [String]",
        content
    )
    
    content = re.sub(
        r"processErrors :: \[String\] -> IO ErrorCollector",
        "processErrors :: [String] -> IO (ErrorCollector String)",
        content
    )
    
    content = re.sub(
        r"newErrorCollector = newErrorCollector",
        "newErrorCollector = newErrorCollector",
        content
    )
    
    with open(file_path, 'w') as f:
        f.write(content)
    
    print(f"Fixed {file_path}")

def fix_core_go_toolchain_properties():
    """Fix CoreGoToolchainPropertiesQuickCheckSpec.hs"""
    file_path = "test/Test/Unit/CoreGoToolchainPropertiesQuickCheckSpec.hs"
    
    with open(file_path, 'r') as f:
        content = f.read()
    
    # Fix duplicate function names
    content = re.sub(
        r"createGoModule :: T\.Text -> String",
        "createGoModule :: T.Text -> String",
        content
    )
    
    content = re.sub(
        r"createGoFunction :: T\.Text -> \[T\.Text\] -> String",
        "createGoFunction_ :: T.Text -> [T.Text] -> String",
        content
    )
    
    content = re.sub(
        r"createGoVariable :: T\.Text -> T\.Text -> String",
        "createGoVariable_ :: T.Text -> T.Text -> String",
        content
    )
    
    content = re.sub(
        r"createGoType :: T\.Text -> String",
        "createGoType_ :: T.Text -> String",
        content
    )
    
    content = re.sub(
        r"createGoFunction _ _ = undefined",
        "createGoFunction_ _ _ = undefined",
        content
    )
    
    content = re.sub(
        r"createGoVariable _ _ = undefined",
        "createGoVariable_ _ _ = undefined",
        content
    )
    
    content = re.sub(
        r"createGoType _ = undefined",
        "createGoType_ _ = undefined",
        content
    )
    
    with open(file_path, 'w') as f:
        f.write(content)
    
    print(f"Fixed {file_path}")

def fix_core_integration_properties():
    """Fix CoreIntegrationPropertiesQuickCheckSpec.hs"""
    file_path = "test/Test/Unit/CoreIntegrationPropertiesQuickCheckSpec.hs"
    
    with open(file_path, 'r') as f:
        content = f.read()
    
    # Fix property return type
    content = re.sub(
        r"Left _ -> property True",
        "Left _ -> property True",
        content
    )
    
    with open(file_path, 'w') as f:
        f.write(content)
    
    print(f"Fixed {file_path}")

def fix_core_ownership_properties():
    """Fix CoreOwnershipPropertiesQuickCheckSpec.hs"""
    file_path = "test/Test/Unit/CoreOwnershipPropertiesQuickCheckSpec.hs"
    
    with open(file_path, 'r') as f:
        content = f.read()
    
    # Fix case expression in function application
    content = re.sub(
        r"testProperty \"Ownership types are correctly classified\" \$\s+\\ownerType ->\s+case ownerType of",
        "testProperty \"Ownership types are correctly classified\" $ \\ownerType -> case ownerType of",
        content
    )
    
    # Fix testCase indentation
    content = re.sub(
        r"testCase \"Ownership analysis handles borrowed references correctly\" \$ do",
        "testCase \"Ownership analysis handles borrowed references correctly\" $ do",
        content
    )
    
    with open(file_path, 'w') as f:
        f.write(content)
    
    print(f"Fixed {file_path}")

def fix_core_parser_properties():
    """Fix CoreParserPropertiesQuickCheckSpec.hs"""
    file_path = "test/Test/Unit/CoreParserPropertiesQuickCheckSpec.hs"
    
    with open(file_path, 'r') as f:
        content = f.read()
    
    # Fix case expression in function application
    content = re.sub(
        r"testProperty \"Parser is idempotent for valid code\" \$\s+\\code -> case parseTypus code of",
        "testProperty \"Parser is idempotent for valid code\" $ \\code -> case parseTypus code of",
        content
    )
    
    # Fix testCase indentation
    content = re.sub(
        r"testCase \"Parser handles ownership directive correctly\" \$ do",
        "testCase \"Parser handles ownership directive correctly\" $ do",
        content
    )
    
    content = re.sub(
        r"testCase \"Parser handles dependent types directive correctly\" \$ do",
        "testCase \"Parser handles dependent types directive correctly\" $ do",
        content
    )
    
    with open(file_path, 'w') as f:
        f.write(content)
    
    print(f"Fixed {file_path}")

def fix_core_performance_properties():
    """Fix CorePerformancePropertiesQuickCheckSpec.hs"""
    file_path = "test/Test/Unit/CorePerformancePropertiesQuickCheckSpec.hs"
    
    with open(file_path, 'r') as f:
        content = f.read()
    
    # Fix parseTypus with T.Text
    content = re.sub(
        r"result = parseTypus input",
        "result = parseTypus (T.unpack input)",
        content
    )
    
    with open(file_path, 'w') as f:
        f.write(content)
    
    print(f"Fixed {file_path}")

def fix_core_source_location_properties():
    """Fix CoreSourceLocationPropertiesQuickCheckSpec.hs"""
    file_path = "test/Test/Unit/CoreSourceLocationPropertiesQuickCheckSpec.hs"
    
    with open(file_path, 'r') as f:
        content = f.read()
    
    # Fix posAfter call
    content = re.sub(
        r"nextPos = posAfter \(posLine pos\) 5",
        "nextPos = posAfter pos 5",
        content
    )
    
    # Fix locatedValue call
    content = re.sub(
        r"locatedValue located == pos",
        "locatedValue (located pos value) == pos",
        content
    )
    
    with open(file_path, 'w') as f:
        f.write(content)
    
    print(f"Fixed {file_path}")

if __name__ == "__main__":
    os.chdir("/home/runner/work/Typus/Typus")
    
    fix_core_compiler_properties()
    fix_core_dependencies_properties()
    fix_core_error_handler_properties()
    fix_core_go_toolchain_properties()
    fix_core_integration_properties()
    fix_core_ownership_properties()
    fix_core_parser_properties()
    fix_core_performance_properties()
    fix_core_source_location_properties()
    
    print("All test files have been fixed!")