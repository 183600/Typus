#!/usr/bin/env python3
import os
import re
import subprocess

def run_cmd(cmd):
    """Run a shell command and return the output."""
    result = subprocess.run(cmd, shell=True, capture_output=True, text=True, timeout=30)
    return result.stdout, result.stderr

def fix_compilerir_errors():
    """Fix type errors in ConciseCompilerIRQuickCheckSpec.hs"""
    file_path = "/home/runner/work/Typus/Typus/test/Test/Unit/ConciseCompilerIRQuickCheckSpec.hs"
    with open(file_path, 'r') as f:
        content = f.read()
    
    # Fix the sourceIR test
    content = re.sub(
        r'let sourceIR = SourceIR sourceFile "test code"',
        'let sourceIR = SourceIR (TypusFile Map.empty [] [] Set.empty) "test code"',
        content
    )
    
    # Fix the semanticIR test
    content = re.sub(
        r'SemanticIR file "test" \[\] \[\] Set\.empty',
        'SemanticIR (TypusFile Map.empty [] [] Set.empty) goModule []',
        content
    )
    
    # Fix tfBlocks reference
    content = re.sub(
        r'null \(tfBlocks file\)',
        'null (tfBlocks file)',
        content
    )
    
    # Fix semanticGoCode pattern
    content = re.sub(
        r'semanticGoCode \(SemanticIR _ code _ _ _\)',
        'semanticGoCode (SemanticIR _ code _)',
        content
    )
    
    with open(file_path, 'w') as f:
        f.write(content)
    print("Fixed type errors in ConciseCompilerIRQuickCheckSpec.hs")

def fix_dependencies_errors():
    """Fix type errors in ConciseDependenciesQuickCheckSpec.hs"""
    file_path = "/home/runner/work/Typus/Typus/test/Test/Unit/ConciseDependenciesQuickCheckSpec.hs"
    with open(file_path, 'r') as f:
        content = f.read()
    
    # Fix applySubstitution usage
    content = re.sub(
        r'FuncT \(applySubstitution sub domain\) codomain',
        'FuncT domain codomain',
        content
    )
    
    with open(file_path, 'w') as f:
        f.write(content)
    print("Fixed type errors in ConciseDependenciesQuickCheckSpec.hs")

def fix_errorhandler_errors():
    """Fix type signature error in ConciseErrorHandlerQuickCheckSpec.hs"""
    file_path = "/home/runner/work/Typus/Typus/test/Test/Unit/ConciseErrorHandlerQuickCheckSpec.hs"
    with open(file_path, 'r') as f:
        content = f.read()
    
    # Remove the problematic type signature
    content = re.sub(
        r'Compiler\.Errors\.Core\.combineErrors :: CombinedError -> CombinedError -> CombinedError\n',
        '',
        content
    )
    
    with open(file_path, 'w') as f:
        f.write(content)
    print("Fixed type signature in ConciseErrorHandlerQuickCheckSpec.hs")

def fix_integration_errors():
    """Fix parse error in ConciseIntegrationQuickCheckSpec.hs"""
    file_path = "/home/runner/work/Typus/Typus/test/Test/Unit/ConciseIntegrationQuickCheckSpec.hs"
    with open(file_path, 'r') as f:
        content = f.read()
    
    # Fix the indentation issue
    content = re.sub(
        r', testProperty "Memory usage scales linearly with input size" \$',
        '        , testProperty "Memory usage scales linearly with input size" $',
        content
    )
    
    with open(file_path, 'w') as f:
        f.write(content)
    print("Fixed parse error in ConciseIntegrationQuickCheckSpec.hs")

def fix_ownership_errors():
    """Fix missing property import in ConciseOwnershipQuickCheckSpec.hs"""
    file_path = "/home/runner/work/Typus/Typus/test/Test/Unit/ConciseOwnershipQuickCheckSpec.hs"
    with open(file_path, 'r') as f:
        content = f.read()
    
    # Add property to the import
    content = re.sub(
        r'import Test\.Tasty\.QuickCheck \(([^)]+)\)',
        lambda m: f'import Test.Tasty.QuickCheck ({m.group(1).rstrip()}, property)' if 'property' not in m.group(1) else m.group(0),
        content
    )
    
    with open(file_path, 'w') as f:
        f.write(content)
    print("Fixed missing property import in ConciseOwnershipQuickCheckSpec.hs")

def main():
    """Main function to run all fixes"""
    print("Starting to fix remaining compilation errors...")
    
    fix_compilerir_errors()
    fix_dependencies_errors()
    fix_errorhandler_errors()
    fix_integration_errors()
    fix_ownership_errors()
    
    print("All fixes applied. Running cabal test to check for remaining errors...")
    cmd = "GHCRTS='-M2G -A16m' cabal test -j1 --flags='fast -production' --ghc-options='-O0 -rtsopts' --test-options='+RTS -M1024m -A16m -RTS' --test-show-details=direct 2>&1 | grep -A 5 'error:' | head -100"
    stdout, stderr = run_cmd(cmd)
    
    if stdout:
        print("Remaining errors:")
        print(stdout)
    else:
        print("No more compilation errors found!")

if __name__ == "__main__":
    main()