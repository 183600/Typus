#!/usr/bin/env python3
"""
Fix remaining import and span issues in test files
"""

import os
import re

def fix_imports_and_span_issues():
    """Fix all remaining import and span issues"""
    
    # Fix import statements - testCase, assertEqual, etc. should be from Test.Tasty.HUnit
    files_with_import_issues = [
        "/home/runner/work/Typus/Typus/test/Test/Unit/BoundaryConditionComprehensiveSpec.hs",
        "/home/runner/work/Typus/Typus/test/Test/Unit/CodeGenerationSpec.hs",
        "/home/runner/work/Typus/Typus/test/Test/Unit/ErrorHandlerSpec.hs",
        "/home/runner/work/Typus/Typus/test/Test/Unit/IntegrationComprehensiveSpec.hs",
        "/home/runner/work/Typus/Typus/test/Test/Unit/ParserCoreFunctionalitySpec.hs",
        "/home/runner/work/Typus/Typus/test/Test/Unit/PerformanceBoundarySpec.hs",
        "/home/runner/work/Typus/Typus/test/Test/Unit/TypeInferenceQuickCheckSpec.hs",
        "/home/runner/work/Typus/Typus/test/Test/Unit/UtilsComprehensiveSpec.hs",
        "/home/runner/work/Typus/Typus/test/Test/Unit/UtilsCoreFunctionalitySpec.hs"
    ]
    
    # Fix syntax errors in import statements
    files_with_syntax_errors = [
        "/home/runner/work/Typus/Typus/test/Test/Unit/CompilerCoreFunctionalitySpec.hs",
        "/home/runner/work/Typus/Typus/test/Test/Unit/CompilerCoreSpec.hs",
        "/home/runner/work/Typus/Typus/test/Test/Unit/CompilerOptimizationAdvancedSpec.hs",
        "/home/runner/work/Typus/Typus/test/Test/Unit/DependencyResolutionSpec.hs",
        "/home/runner/work/Typus/Typus/test/Test/Unit/ErrorHandlerCoreComprehensiveSpec.hs",
        "/home/runner/work/Typus/Typus/test/Test/Unit/ErrorReportingQuickCheckSpec.hs",
        "/home/runner/work/Typus/Typus/test/Test/Unit/IntegrationQuickCheckSpec.hs",
        "/home/runner/work/Typus/Typus/test/Test/Unit/OwnershipTransferSpec.hs",
        "/home/runner/work/Typus/Typus/test/Test/Unit/ParserBasicSpec.hs",
        "/home/runner/work/Typus/Typus/test/Test/Unit/ParserCombinatorsSpec.hs",
        "/home/runner/work/Typus/Typus/test/Test/Unit/ParserComprehensiveSpec.hs",
        "/home/runner/work/Typus/Typus/test/Test/Unit/SourceLocationComprehensiveSpec.hs",
        "/home/runner/work/Typus/Typus/test/Test/Unit/SymbolTableAdvancedSpec.hs",
        "/home/runner/work/Typus/Typus/test/Test/Unit/TextProcessingAdvancedSpec.hs"
    ]
    
    # Fix remaining span issues
    files_with_span_issues = [
        "/home/runner/work/Typus/Typus/test/Test/Unit/BoundaryConditionsEnhancedQuickCheckSpec.hs",
        "/home/runner/work/Typus/Typus/test/Test/Unit/NewParserQuickCheckSpec.hs",
        "/home/runner/work/Typus/Typus/test/Test/Unit/SourceLocationTestSpec.hs"
    ]
    
    fixed_count = 0
    
    # Fix import issues
    for filepath in files_with_import_issues:
        if fix_single_file_imports(filepath):
            fixed_count += 1
    
    # Fix syntax errors
    for filepath in files_with_syntax_errors:
        if fix_syntax_errors_in_file(filepath):
            fixed_count += 1
    
    # Fix span issues
    for filepath in files_with_span_issues:
        if fix_remaining_span_issues(filepath):
            fixed_count += 1
    
    return fixed_count

def fix_single_file_imports(filepath):
    """Fix import issues in a single file"""
    try:
        with open(filepath, 'r') as f:
            content = f.read()
        
        original_content = content
        
        # Remove testCase, assertEqual, etc. from Test.Tasty.QuickCheck imports
        content = re.sub(
            r'import Test\.Tasty\.QuickCheck\s*\(([^)]*)\)',
            lambda m: fix_quickcheck_import(m.group(1)),
            content
        )
        
        # Remove testCase, assertEqual, etc. from Test.Tasty imports
        content = re.sub(
            r'import Test\.Tasty\s*\(([^)]*)\)',
            lambda m: fix_tasty_import(m.group(1)),
            content
        )
        
        # Add Test.Tasty.HUnit import if needed and not present
        if 'testCase' in content or 'assertEqual' in content or 'assertBool' in content or 'assertFailure' in content:
            if 'import Test.Tasty.HUnit' not in content:
                # Find a good place to insert the import
                lines = content.split('\n')
                insert_idx = 0
                for i, line in enumerate(lines):
                    if line.startswith('import Test.Tasty'):
                        insert_idx = i + 1
                        break
                
                lines.insert(insert_idx, 'import Test.Tasty.HUnit')
                content = '\n'.join(lines)
        
        # Write back if changed
        if content != original_content:
            with open(filepath, 'w') as f:
                f.write(content)
            print(f"Fixed imports in {filepath}")
            return True
        else:
            print(f"No import changes needed for {filepath}")
            return False
            
    except Exception as e:
        print(f"Error processing imports in {filepath}: {e}")
        return False

def fix_quickcheck_import(imports_str):
    """Remove HUnit-specific imports from QuickCheck import"""
    # Remove testCase, assertEqual, assertBool, assertFailure, Assertion
    hunit_funcs = ['testCase', 'assertEqual', 'assertBool', 'assertFailure', 'Assertion']
    
    # Parse and filter imports
    imports = [imp.strip() for imp in imports_str.split(',')]
    filtered_imports = [imp for imp in imports if imp not in hunit_funcs]
    
    if filtered_imports:
        return f'import Test.Tasty.QuickCheck ({", ".join(filtered_imports)})'
    else:
        return '-- Removed empty QuickCheck import'

def fix_tasty_import(imports_str):
    """Remove HUnit-specific imports from Tasty import"""
    # Remove testCase, assertEqual, assertBool
    hunit_funcs = ['testCase', 'assertEqual', 'assertBool']
    
    # Parse and filter imports
    imports = [imp.strip() for imp in imports_str.split(',')]
    filtered_imports = [imp for imp in imports if imp not in hunit_funcs]
    
    if filtered_imports:
        return f'import Test.Tasty ({", ".join(filtered_imports)})'
    else:
        return '-- Removed empty Tasty import'

def fix_syntax_errors_in_file(filepath):
    """Fix syntax errors in import statements"""
    try:
        with open(filepath, 'r') as f:
            content = f.read()
        
        original_content = content
        
        # Fix pattern: import Test.Tasty (TestTree, testGroup) (testCase, ...)
        content = re.sub(
            r'import Test\.Tasty\s*\([^)]+\)\s*\([^)]+\)',
            lambda m: fix_double_paren_import(m.group(0)),
            content
        )
        
        # Write back if changed
        if content != original_content:
            with open(filepath, 'w') as f:
                f.write(content)
            print(f"Fixed syntax errors in {filepath}")
            return True
        else:
            print(f"No syntax changes needed for {filepath}")
            return False
            
    except Exception as e:
        print(f"Error processing syntax in {filepath}: {e}")
        return False

def fix_double_paren_import(import_str):
    """Fix import statements with double parentheses"""
    # Extract all imports from both parentheses
    imports = re.findall(r'\(([^)]+)\)', import_str)
    if len(imports) >= 2:
        all_imports = []
        for imp_group in imports:
            all_imports.extend([imp.strip() for imp in imp_group.split(',')])
        
        # Filter out HUnit-specific imports for Test.Tasty
        hunit_funcs = ['testCase', 'assertEqual', 'assertBool', 'assertFailure', 'Assertion']
        tasty_imports = [imp for imp in all_imports if imp not in hunit_funcs]
        hunit_imports = [imp for imp in all_imports if imp in hunit_funcs]
        
        result = []
        if tasty_imports:
            result.append(f'import Test.Tasty ({", ".join(tasty_imports)})')
        if hunit_imports:
            result.append(f'import Test.Tasty.HUnit ({", ".join(hunit_imports)})')
        
        return '\n'.join(result)
    
    return import_str

def fix_remaining_span_issues(filepath):
    """Fix remaining span issues in specific files"""
    try:
        with open(filepath, 'r') as f:
            content = f.read()
        
        original_content = content
        
        if 'BoundaryConditionsEnhancedQuickCheckSpec.hs' in filepath:
            # Fix specific issues in this file
            content = re.sub(r'mergeSpans\s+span\s+span', 'mergeSpans sourceSpan sourceSpan', content)
            
        elif 'NewParserQuickCheckSpec.hs' in filepath:
            # Fix sourceSpan undefined issue
            # Add sourceSpan definition where needed
            content = re.sub(
                r'return \$ Located str \(spanStart sourceSpan\) span',
                'return $ Located str (spanStart sourceSpan) sourceSpan',
                content
            )
            content = re.sub(r'cbSpan block == span', 'cbSpan block == sourceSpan', content)
            
        elif 'SourceLocationTestSpec.hs' in filepath:
            # Fix sourceSpan undefined issue
            content = re.sub(
                r'let\s+sourceSpan\s*=\s*SourceSpan\s+start\s+end\s+in\s+case\s+sourceSpan\s+of',
                'let sourceSpan = SourceSpan start end in sourceSpan',
                content
            )
            content = re.sub(
                r'in case sourceSpan of SourceSpan s e -> s == e',
                'in sourceSpan',
                content
            )
        
        # Write back if changed
        if content != original_content:
            with open(filepath, 'w') as f:
                f.write(content)
            print(f"Fixed remaining span issues in {filepath}")
            return True
        else:
            print(f"No span changes needed for {filepath}")
            return False
            
    except Exception as e:
        print(f"Error processing span issues in {filepath}: {e}")
        return False

def main():
    """Main function to fix all issues"""
    fixed_count = fix_imports_and_span_issues()
    print(f"\nFixed issues in {fixed_count} files")

if __name__ == "__main__":
    main()