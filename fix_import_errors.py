#!/usr/bin/env python3
"""
Fix import statement issues in test files
"""

import os
import re

# List of files with import issues
files_to_fix = [
    "/home/runner/work/Typus/Typus/test/Test/Unit/CompilerCoreFunctionalitySpec.hs",
    "/home/runner/work/Typus/Typus/test/Test/Unit/CompilerCoreSpec.hs",
    "/home/runner/work/Typus/Typus/test/Test/Unit/CompilerOptimizationAdvancedSpec.hs",
    "/home/runner/work/Typus/Typus/test/Test/Unit/DependencyResolutionSpec.hs",
    "/home/runner/work/Typus/Typus/test/Test/Unit/ErrorHandlerCoreComprehensiveSpec.hs",
    "/home/runner/work/Typus/Typus/test/Test/Unit/ErrorHandlerSpec.hs",
    "/home/runner/work/Typus/Typus/test/Test/Unit/ErrorReportingQuickCheckSpec.hs",
    "/home/runner/work/Typus/Typus/test/Test/Unit/IntegrationComprehensiveSpec.hs",
    "/home/runner/work/Typus/Typus/test/Test/Unit/IntegrationQuickCheckSpec.hs",
    "/home/runner/work/Typus/Typus/test/Test/Unit/OwnershipTransferSpec.hs",
    "/home/runner/work/Typus/Typus/test/Test/Unit/ParserBasicSpec.hs",
    "/home/runner/work/Typus/Typus/test/Test/Unit/ParserCombinatorsSpec.hs",
    "/home/runner/work/Typus/Typus/test/Test/Unit/ParserComprehensiveSpec.hs",
    "/home/runner/work/Typus/Typus/test/Test/Unit/ParserCoreFunctionalitySpec.hs",
    "/home/runner/work/Typus/Typus/test/Test/Unit/PerformanceBoundarySpec.hs",
    "/home/runner/work/Typus/Typus/test/Test/Unit/SourceLocationComprehensiveSpec.hs",
    "/home/runner/work/Typus/Typus/test/Test/Unit/SymbolTableAdvancedSpec.hs",
    "/home/runner/work/Typus/Typus/test/Test/Unit/TextProcessingAdvancedSpec.hs",
    "/home/runner/work/Typus/Typus/test/Test/Unit/TypeInferenceQuickCheckSpec.hs",
    "/home/runner/work/Typus/Typus/test/Test/Unit/UtilsComprehensiveSpec.hs",
    "/home/runner/work/Typus/Typus/test/Test/Unit/UtilsCoreFunctionalitySpec.hs"
]

def fix_imports_in_file(filepath):
    """Fix broken import statements in a file"""
    try:
        with open(filepath, 'r') as f:
            content = f.read()
        
        # Pattern to match broken import statements
        # Matches: import Module\n(symbol list)
        pattern = r'import\s+([^\n]+)\n\(([^)]+)\)'
        
        def replacement(match):
            module = match.group(1).strip()
            symbols = match.group(2).strip()
            return f'import {module} ({symbols})'
        
        # Apply the replacement
        fixed_content = re.sub(pattern, replacement, content)
        
        # Write back if changed
        if fixed_content != content:
            with open(filepath, 'w') as f:
                f.write(fixed_content)
            print(f"Fixed imports in {filepath}")
            return True
        else:
            print(f"No changes needed for {filepath}")
            return False
            
    except Exception as e:
        print(f"Error processing {filepath}: {e}")
        return False

def main():
    """Main function to fix all files"""
    fixed_count = 0
    for filepath in files_to_fix:
        if fix_imports_in_file(filepath):
            fixed_count += 1
    
    print(f"\nFixed imports in {fixed_count} files")

if __name__ == "__main__":
    main()