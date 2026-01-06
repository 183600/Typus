#!/usr/bin/env python3
import os
import re
import sys

def fix_specific_import_pattern(file_path):
    """Fix specific import pattern in a file"""
    try:
        with open(file_path, 'r') as f:
            content = f.read()
        
        # Fix the specific pattern: Arbitrary(.., (==>))
        old_pattern = r'Arbitrary\(\.\.\.,\s*\(\s*==>\s*\)\s*\)'
        new_pattern = 'Arbitrary(..)'
        
        if re.search(old_pattern, content):
            content = re.sub(old_pattern, new_pattern, content)
            
            with open(file_path, 'w') as f:
                f.write(content)
            
            return True
        
        return False
    except Exception as e:
        print(f"Error processing {file_path}: {e}")
        return False

def main():
    test_dir = "test/Test/Unit"
    fixed_count = 0
    
    # Files that still have errors
    error_files = [
        "ComprehensiveCoreQuickCheckSpec.hs",
        "CoreQuickCheckPropertiesSpec.hs",
        "CustomCompilerQuickCheckSpec.hs",
        "CustomDependentTypesQuickCheckSpec.hs",
        "CustomErrorHandlingQuickCheckSpec.hs",
        "CustomOwnershipQuickCheckSpec.hs",
        "CustomParserQuickCheckSpec.hs",
        "CustomSourceLocationQuickCheckSpec.hs",
        "CustomSymbolTableQuickCheckSpec.hs",
        "CustomSyntaxValidatorQuickCheckSpec.hs",
        "CustomUtilsQuickCheckSpec.hs",
        "DependenciesErrorHandlingQuickCheckSpec.hs",
        "ErrorHandlingPropertySpec.hs",
        "ErrorLocationTrackingQuickCheckSpec.hs",
        "IntegrationAdditionalQuickCheckSpec.hs",
        "NewCabalCompilerQuickCheckTestSpec.hs",
        "NewCabalDependenciesQuickCheckTestSpec.hs",
        "NewCabalErrorHandlerQuickCheckTestSpec.hs",
        "NewCabalOwnershipQuickCheckTestSpec.hs",
        "NewCabalParserQuickCheckTestSpec.hs",
        "NewCabalQuickCheckPropertiesSpec.hs",
        "NewCabalQuickCheckTestSuite.hs",
        "NewCabalSourceLocationQuickCheckTestSpec.hs",
        "ParserAdditionalQuickCheckSpec.hs",
        "ParserBoundaryConditionsQuickCheckSpec.hs",
        "SyntaxValidatorBoundaryQuickCheckSpec.hs",
        "SyntaxValidatorGoToolchainQuickCheckSpec.hs",
        "UtilsAdditionalQuickCheckSpec.hs"
    ]
    
    for filename in error_files:
        file_path = os.path.join(test_dir, filename)
        if os.path.exists(file_path):
            if fix_specific_import_pattern(file_path):
                print(f"Fixed {file_path}")
                fixed_count += 1
    
    print(f"Fixed {fixed_count} files")

if __name__ == "__main__":
    main()