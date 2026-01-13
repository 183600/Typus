#!/usr/bin/env python3
"""
Script to fix all remaining compilation errors in the test files.
"""

import re
import os

def fix_all_files():
    """Fix all remaining compilation errors"""
    
    # Fix CoreCompilerPropertiesQuickCheckSpec.hs
    file_path = "test/Test/Unit/CoreCompilerPropertiesQuickCheckSpec.hs"
    with open(file_path, 'r') as f:
        content = f.read()
    
    # Add type annotations
    content = re.sub(
        r"testProperty \"Compilation of empty file succeeds\" \$\s+\\ _ ->",
        "testProperty \"Compilation of empty file succeeds\" $ \\( _ :: () ) ->",
        content
    )
    
    content = re.sub(
        r"testProperty \"Compiler is deterministic\" \$\s+\\ code ->",
        "testProperty \"Compiler is deterministic\" $ \\( code :: String ) ->",
        content
    )
    
    content = re.sub(
        r"testProperty \"Compiler handles constant folding\" \$\s+\\ num1 num2 ->",
        "testProperty \"Compiler handles constant folding\" $ \\( num1 :: Int ) ( num2 :: Int ) ->",
        content
    )
    
    with open(file_path, 'w') as f:
        f.write(content)
    
    # Fix CoreDependenciesPropertiesQuickCheckSpec.hs
    file_path = "test/Test/Unit/CoreDependenciesPropertiesQuickCheckSpec.hs"
    with open(file_path, 'r') as f:
        content = f.read()
    
    # Add type annotations
    content = re.sub(
        r"testProperty \"Type unification is symmetric\" \$\s+\\ type1 type2 ->",
        "testProperty \"Type unification is symmetric\" $ \\( type1 :: TypeVar ) ( type2 :: TypeVar ) ->",
        content
    )
    
    with open(file_path, 'w') as f:
        f.write(content)
    
    # Fix CoreErrorHandlerPropertiesQuickCheckSpec.hs
    file_path = "test/Test/Unit/CoreErrorHandlerPropertiesQuickCheckSpec.hs"
    with open(file_path, 'r') as f:
        content = f.read()
    
    # Fix string operations
    content = re.sub(
        r"errorMsg `T\.isInfixOf` formatted",
        "T.pack errorMsg `T.isInfixOf` formatted",
        content
    )
    
    content = re.sub(
        r"formatString msg = T\.pack \"Error: \" <> msg",
        "formatString msg = T.pack \"Error: \" <> T.pack msg",
        content
    )
    
    # Fix ErrorContext
    content = re.sub(
        r"ErrorContext Nothing Nothing Nothing \[\]",
        "ErrorContext Nothing Nothing Nothing []",
        content
    )
    
    with open(file_path, 'w') as f:
        f.write(content)
    
    # Fix CoreGoToolchainPropertiesQuickCheckSpec.hs
    file_path = "test/Test/Unit/CoreGoToolchainPropertiesQuickCheckSpec.hs"
    with open(file_path, 'r') as f:
        content = f.read()
    
    # Fix function names
    content = re.sub(
        r"createString moduleName",
        "createGoModule moduleName",
        content
    )
    
    content = re.sub(
        r"createString funcName params",
        "createGoFunction_ funcName params",
        content
    )
    
    # Fix string literals
    content = re.sub(
        r"generateGoCode = \"package main",
        "generateGoCode = T.pack \"package main",
        content
    )
    
    content = re.sub(
        r"formatGoImports imports = T\.unlines \$ map",
        "formatGoImports imports = T.unlines $ map",
        content
    )
    
    content = re.sub(
        r"\\imp -> \"import \\\"" <> imp <> "\\\"\""",
        "\\imp -> T.pack \"import \\\"" <> T.pack imp <> T.pack "\\\"\"",
        content
    )
    
    content = re.sub(
        r"generateFromAST _ = \"package main\"",
        "generateFromAST _ = T.pack \"package main\"",
        content
    )
    
    content = re.sub(
        r"generateComplexType _ = \"type Complex struct",
        "generateComplexType _ = T.pack \"type Complex struct",
        content
    )
    
    content = re.sub(
        r"createNestedStructure depth = \"struct",
        "createNestedStructure depth = T.pack \"struct",
        content
    )
    
    content = re.sub(
        r"T\.replicate depth \"\\tNested",
        "T.replicate depth (T.pack \"\\tNested",
        content
    )
    
    with open(file_path, 'w') as f:
        f.write(content)
    
    # Fix CoreIntegrationPropertiesQuickCheckSpec.hs
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
    
    # Fix CoreOwnershipPropertiesQuickCheckSpec.hs
    file_path = "test/Test/Unit/CoreOwnershipPropertiesQuickCheckSpec.hs"
    with open(file_path, 'r') as f:
        content = f.read()
    
    # Remove non-existent constructors
    content = re.sub(
        r"Moved -> property True",
        "-- Moved -> property True",
        content
    )
    
    content = re.sub(
        r"Shared -> property True",
        "-- Shared -> property True",
        content
    )
    
    with open(file_path, 'w') as f:
        f.write(content)
    
    # Fix CoreParserPropertiesQuickCheckSpec.hs
    file_path = "test/Test/Unit/CoreParserPropertiesQuickCheckSpec.hs"
    with open(file_path, 'r') as f:
        content = f.read()
    
    # Add import
    content = re.sub(
        r"import Parser \(parseTypus, TypusFile\(\.\.\.\), FileDirectives\(\.\.\.\), BlockDirectives\(\.\.\.\)\)",
        "import Parser (parseTypus, TypusFile(..), FileDirectives(..), BlockDirectives(..), defaultFileDirectives)",
        content
    )
    
    # Fix Located constructor
    content = re.sub(
        r"Located _ True",
        "Located (SourcePos 0 0 0) True",
        content
    )
    
    with open(file_path, 'w') as f:
        f.write(content)
    
    # Fix CorePerformancePropertiesQuickCheckSpec.hs
    file_path = "test/Test/Unit/CorePerformancePropertiesQuickCheckSpec.hs"
    with open(file_path, 'r') as f:
        content = f.read()
    
    # Remove force
    content = re.sub(
        r"results = replicate iterations \$ force \$ parseTypus input",
        "results = replicate iterations $ parseTypus input",
        content
    )
    
    with open(file_path, 'w') as f:
        f.write(content)
    
    # Fix CoreSourceLocationPropertiesQuickCheckSpec.hs
    file_path = "test/Test/Unit/CoreSourceLocationPropertiesQuickCheckSpec.hs"
    with open(file_path, 'r') as f:
        content = f.read()
    
    # Fix posAfter call
    content = re.sub(
        r"nextPos = posAfter pos 5",
        "nextPos = posAfter (posColumn pos) 5",
        content
    )
    
    # Fix located call
    content = re.sub(
        r"locatedValue \(located \(SourceSpan pos pos\) value\) == pos",
        "locatedValue (located (SourceSpan pos pos) value) == pos",
        content
    )
    
    with open(file_path, 'w') as f:
        f.write(content)
    
    print("All files have been fixed!")

if __name__ == "__main__":
    os.chdir("/home/runner/work/Typus/Typus")
    fix_all_files()