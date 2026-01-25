#!/usr/bin/env python3
"""
Fix the final remaining issues
"""

import os
import re

def fix_dependency_resolution_spec():
    """Fix DependencyType name conflict in DependencyResolutionSpec.hs"""
    filepath = "/home/runner/work/Typus/Typus/test/Test/Unit/DependencyResolutionSpec.hs"
    
    try:
        with open(filepath, 'r') as f:
            content = f.read()
        
        original_content = content
        
        # Fix the ambiguous DependencyType by qualifying it with the local module
        content = re.sub(
            r'dependencyType :: DependencyType',
            'dependencyType :: Test.Unit.DependencyResolutionSpec.DependencyType',
            content
        )
        
        content = re.sub(
            r'genDependencyType :: Gen DependencyType',
            'genDependencyType :: Gen Test.Unit.DependencyResolutionSpec.DependencyType',
            content)
        
        # Write back if changed
        if content != original_content:
            with open(filepath, 'w') as f:
                f.write(content)
            print(f"Fixed DependencyType conflicts in {filepath}")
            return True
        else:
            print(f"No changes needed for {filepath}")
            return False
            
    except Exception as e:
        print(f"Error processing {filepath}: {e}")
        return False

def fix_new_parser_quick_check_spec():
    """Fix sourceSpan undefined issue in NewParserQuickCheckSpec.hs"""
    filepath = "/home/runner/work/Typus/Typus/test/Test/Unit/NewParserQuickCheckSpec.hs"
    
    try:
        with open(filepath, 'r') as f:
            content = f.read()
        
        original_content = content
        
        # Find the function and add sourceSpan definition
        # Look for the pattern around line 41
        lines = content.split('\n')
        for i, line in enumerate(lines):
            if 'return $ Located str (spanStart sourceSpan) sourceSpan' in line:
                # Add sourceSpan definition before this line
                if i > 0 and 'sourceSpan' not in lines[i-1]:
                    lines.insert(i, '    let sourceSpan = SourceSpan (startPos str) (startPos str)')
                    content = '\n'.join(lines)
                    break
        
        # Write back if changed
        if content != original_content:
            with open(filepath, 'w') as f:
                f.write(content)
            print(f"Fixed sourceSpan undefined in {filepath}")
            return True
        else:
            print(f"No changes needed for {filepath}")
            return False
            
    except Exception as e:
        print(f"Error processing {filepath}: {e}")
        return False

def fix_source_location_test_spec():
    """Fix syntax error in SourceLocationTestSpec.hs"""
    filepath = "/home/runner/work/Typus/Typus/test/Test/Unit/SourceLocationTestSpec.hs"
    
    try:
        with open(filepath, 'r') as f:
            content = f.read()
        
        original_content = content
        
        # Fix the malformed case expression
        # Change: let sourceSpan = SourceSpan start end in sourceSpan SourceSpan s e -> s == start && e == end
        # To:     let sourceSpan = SourceSpan start end in case sourceSpan of SourceSpan s e -> s == start && e == end
        content = re.sub(
            r'let\s+sourceSpan\s*=\s*SourceSpan\s+start\s+end\s+in\s+sourceSpan\s+SourceSpan\s+s\s+e\s*->\s*s\s*==\s*start\s*&&\s*e\s*==\s*end',
            'let sourceSpan = SourceSpan start end in case sourceSpan of SourceSpan s e -> s == start && e == end',
            content
        )
        
        # Write back if changed
        if content != original_content:
            with open(filepath, 'w') as f:
                f.write(content)
            print(f"Fixed syntax error in {filepath}")
            return True
        else:
            print(f"No changes needed for {filepath}")
            return False
            
    except Exception as e:
        print(f"Error processing {filepath}: {e}")
        return False

def main():
    """Main function to fix all final issues"""
    fixed_count = 0
    
    if fix_dependency_resolution_spec():
        fixed_count += 1
    
    if fix_new_parser_quick_check_spec():
        fixed_count += 1
    
    if fix_source_location_test_spec():
        fixed_count += 1
    
    print(f"\nFixed issues in {fixed_count} files")

if __name__ == "__main__":
    main()