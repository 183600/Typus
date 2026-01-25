#!/usr/bin/env python3
"""
Add back Test.Tasty.HUnit import where needed
"""

import os
import re

def fix_hunit_imports(filepath):
    """Add HUnit import back if needed"""
    with open(filepath, 'r') as f:
        content = f.read()
    
    # Check if the file uses HUnit features
    uses_testCase = 'testCase' in content
    uses_assertion = '@?=' in content or 'assert' in content
    uses_Assertion = 'Assertion' in content
    
    # If HUnit is used but not imported, add it
    if (uses_testCase or uses_assertion or uses_Assertion) and not re.search(r'^import Test\.Tasty\.HUnit', content, flags=re.MULTILINE):
        # Find where to insert the import
        lines = content.split('\n')
        insert_idx = 0
        for i, line in enumerate(lines):
            if line.startswith('import '):
                insert_idx = i + 1
            elif line.startswith('module '):
                # Skip past module declaration and any exports
                while i < len(lines) and not (lines[i].startswith('import ') or lines[i].startswith('--') or lines[i].strip() == ''):
                    i += 1
                insert_idx = i
                break
        
        lines.insert(insert_idx, 'import Test.Tasty.HUnit')
        content = '\n'.join(lines)
        
        with open(filepath, 'w') as f:
            f.write(content)
        print(f"  Added Test.Tasty.HUnit import to {filepath}")
        return True
    
    return False

def main():
    test_dir = 'test/Test/Unit'
    modified_files = []
    
    # List of specific files that need HUnit
    hunit_files = [
        'AdditionalCabalQuickCheckTests.hs',
        'AdvancedTextProcessingSpec.hs',
        'BoundaryConditionSpec.hs',
        'BoundaryConditionsAdvanced2025Spec.hs',
        'CLISpec.hs',
        'CabalAnalyzerQuickCheckSpec.hs',
        'CabalBoundaryConditionsSpec.hs',
        'CabalCompilerQuickCheckSpec.hs',
        'CabalConcurrentParsingSpec.hs',
        'CabalCrossModuleIntegrationSpec.hs',
        'CabalDependentTypesQuickCheckSpec.hs',
        'CabalEndToEndSpec.hs',
        'CabalEnhancedQuickCheckSpec.hs',
        'CabalErrorHandlerQuickCheckSpec.hs',
        'CabalErrorRecoverySpec.hs',
        'CabalIntegrationQuickCheckSpec.hs',
        'CabalMemorySafetySpec.hs',
        'CabalOwnershipQuickCheckSpec.hs',
        'CabalQuickCheckTests.hs',
        'CabalConcurrentParsingSpec.hs',
        'CabalCrossModuleIntegrationSpec.hs',
        'CabalDependentTypesQuickCheckSpec.hs',
        'CabalEndToEndSpec.hs',
        'CabalEnhancedQuickCheckSpec.hs',
        'CabalErrorHandlerQuickCheckSpec.hs',
        'CabalErrorRecoverySpec.hs',
        'CabalIntegrationQuickCheckSpec.hs',
        'CabalMemorySafetySpec.hs',
        'CabalOwnershipQuickCheckSpec.hs',
        'CabalQuickCheckTests.hs',
    ]
    
    for file in hunit_files:
        filepath = os.path.join(test_dir, file)
        if os.path.exists(filepath):
            print(f"Checking {filepath}")
            if fix_hunit_imports(filepath):
                modified_files.append(filepath)
    
    print(f"\nModified {len(modified_files)} files")

if __name__ == '__main__':
    main()