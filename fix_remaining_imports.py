#!/usr/bin/env python3
import os
import re

def fix_specific_files():
    """Fix specific files with import errors"""
    files_to_fix = [
        "test/Test/Unit/NewCoreFunctionalityQuickCheckSpec.hs",
        "test/Test/Unit/NewDependenciesAdvancedQuickCheckSpec.hs",
        "test/Test/Unit/NewIntegrationAdvancedQuickCheckSpec.hs",
        "test/Test/Unit/NewTextProcessingQuickCheckSpec.hs",
        "test/Test/Unit/OwnershipCoreQuickCheckTests.hs",
        "test/Test/Unit/NewParserValidationSpec.hs",
        "test/Test/Unit/ParserCoreQuickCheckTests.hs",
        "test/Test/Unit/UtilsCoreQuickCheckTests.hs"
    ]
    
    for file_path in files_to_fix:
        try:
            with open(file_path, 'r') as f:
                content = f.read()
            
            # Fix (===, (==>)) pattern
            content = re.sub(r'\(\s*===,\s*\(\s*==>\s*\)\s*\)', '(===), (==>)', content)
            
            # Fix duplicate property
            content = re.sub(r'property,\s*Property', 'Property', content)
            content = re.sub(r'Property,\s*property', 'Property', content)
            
            # Fix duplicate testProperty
            content = re.sub(r'testProperty,\s*testProperty', 'testProperty', content)
            
            # Fix duplicate (==>)
            import_pattern = r'import\s+Test\.Tasty\.QuickCheck\s*\(([^)]*)\)'
            
            def fix_import(match):
                import_list = match.group(1)
                
                # Split and clean parts
                parts = [p.strip() for p in import_list.split(',')]
                seen = set()
                unique_parts = []
                
                for part in parts:
                    # Skip empty parts
                    if not part:
                        continue
                    
                    # Normalize for comparison
                    normalized = re.sub(r'\s+', '', part)
                    
                    # Skip duplicates
                    if normalized in seen:
                        continue
                    
                    # Add to unique parts
                    seen.add(normalized)
                    unique_parts.append(part)
                
                return f"import Test.Tasty.QuickCheck ({', '.join(unique_parts)})"
            
            content = re.sub(import_pattern, fix_import, content)
            
            with open(file_path, 'w') as f:
                f.write(content)
            
            print(f"Fixed {file_path}")
        except Exception as e:
            print(f"Error processing {file_path}: {e}")

if __name__ == "__main__":
    fix_specific_files()