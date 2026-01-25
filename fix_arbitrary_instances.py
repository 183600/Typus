#!/usr/bin/env python3
import os
import re

# List of files that need to have Arbitrary instances removed
files_to_fix = [
    "test/Test/Unit/BoundaryConditionComprehensiveSpec.hs",
    "test/Test/Unit/BoundaryConditionsEnhancedQuickCheckSpec.hs",
    "test/Test/Unit/CompilerCorePropertiesSpec.hs",
    "test/Test/Unit/CompilerIRConsistencyQuickCheckSpec.hs",
    "test/Test/Unit/ConciseCompilerQuickCheckSpec.hs",
    "test/Test/Unit/ConciseIntegrationQuickCheckSpec.hs",
    "test/Test/Unit/ConciseParserQuickCheckSpec.hs",
    "test/Test/Unit/ConciseSourceLocationQuickCheckSpec.hs",
    "test/Test/Unit/DependentTypeConstraintSpec.hs",
    "test/Test/Unit/ErrorHandlerConsistencyQuickCheckSpec.hs",
    "test/Test/Unit/ErrorRecoveryConsistencySpec.hs",
    "test/Test/Unit/IntegrationEndToEndSpec.hs",
    "test/Test/Unit/NewAdditionalSourceLocationQuickCheckTestSpec.hs",
    "test/Test/Unit/NewParserQuickCheckSpec.hs",
    "test/Test/Unit/NewSourceLocationQuickCheckSpec.hs",
    "test/Test/Unit/NewSourceLocationTestSpec.hs",
    "test/Test/Unit/OwnershipTransitivityQuickCheckSpec.hs",
    "test/Test/Unit/OwnershipTransitivitySpec.hs",
    "test/Test/Unit/ParserCoreFunctionalitySpec.hs",
    "test/Test/Unit/ParserCorePropertiesSpec.hs",
    "test/Test/Unit/ParserEnhancedQuickCheckSpec.hs",
    "test/Test/Unit/PerformanceEnhancedQuickCheckSpec.hs",
    "test/Test/Unit/SourceLocationAdvancedQuickCheckSpec.hs",
    "test/Test/Unit/SourceLocationComprehensiveSpec.hs",
    "test/Test/Unit/SourceLocationCorePropertiesSpec.hs",
    "test/Test/Unit/SourceLocationSpanQuickCheckSpec.hs",
    "test/Test/Unit/SourceLocationTestSpec.hs",
    "test/TestSupport/Arbitrary.hs"
]

def remove_arbitrary_instances(file_path):
    """Remove Arbitrary instance definitions for SourcePos and SourceSpan"""
    try:
        with open(file_path, 'r') as f:
            content = f.read()
        
        # Pattern to match instance declarations for SourcePos and SourceSpan
        # This matches multi-line instance declarations
        pattern = r'instance\s+Arbitrary\s+(SourcePos|SourceSpan)\s+where.*?(?=\n\n|\ninstance|\n--|\Z)'
        
        # Replace with a comment
        def replacement(match):
            return f"-- Arbitrary instance for {match.group(1)} is now defined in SourceLocation module\n"
        
        content = re.sub(pattern, replacement, content, flags=re.DOTALL)
        
        with open(file_path, 'w') as f:
            f.write(content)
        
        print(f"Fixed {file_path}")
        return True
    except Exception as e:
        print(f"Error fixing {file_path}: {e}")
        return False

def main():
    for file_path in files_to_fix:
        if os.path.exists(file_path):
            remove_arbitrary_instances(file_path)
        else:
            print(f"File not found: {file_path}")

if __name__ == "__main__":
    main()