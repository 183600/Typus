#!/usr/bin/env python3
import os
import re

def fix_compilerir_viewpattern():
    """Fix view pattern issue in CompilerIR test"""
    file_path = "/home/runner/work/Typus/Typus/test/Test/Unit/ConciseCompilerIRQuickCheckSpec.hs"
    with open(file_path, 'r') as f:
        content = f.read()
    
    # Fix the view pattern issue
    content = re.sub(
        r'testProperty\s+"[^"]+"\s*\$\s*\w+ ->',
        lambda m: m.group(0).replace(m.group(0).split('$')[1], '\\_ ->'),
        content
    )
    
    with open(file_path, 'w') as f:
        f.write(content)
    print("Fixed view pattern issue in CompilerIR test")

def fix_dependencies_ambiguous():
    """Fix ambiguous type variables in Dependencies test"""
    file_path = "/home/runner/work/Typus/Typus/test/Test/Unit/ConciseDependenciesQuickCheckSpec.hs"
    with open(file_path, 'r') as f:
        content = f.read()
    
    # Fix ambiguous type variables by adding type annotations
    content = re.sub(
        r'testProperty "Empty map has no owners" \$',
        'testProperty "Empty map has no owners" $',
        content
    )
    
    content = re.sub(
        r'\\name -> property \(Map\.null Map\.empty && Map\.lookup name Map\.empty == Nothing\)',
        '\\(_ :: Text) -> property (Map.null (Map.empty :: Map.Map Text TypeExpr) && Map.lookup (_ :: Text) (Map.empty :: Map.Map Text TypeExpr) == Nothing)',
        content
    )
    
    content = re.sub(
        r'testProperty "Single insertion creates owner" \$',
        'testProperty "Single insertion creates owner" $',
        content
    )
    
    content = re.sub(
        r'\\k v ->',
        '\\(k :: Text) (v :: TypeExpr) ->',
        content
    )
    
    with open(file_path, 'w') as f:
        f.write(content)
    print("Fixed ambiguous type variables in Dependencies test")

def main():
    """Main function to run all fixes"""
    print("Starting to fix final compilation errors...")
    
    fix_compilerir_viewpattern()
    fix_dependencies_ambiguous()
    
    print("All fixes applied.")

if __name__ == "__main__":
    main()