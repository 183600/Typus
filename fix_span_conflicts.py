#!/usr/bin/env python3
"""
Fix span variable name conflicts with Prelude.span function
"""

import os
import re

# List of files with span conflicts
files_to_fix = [
    "/home/runner/work/Typus/Typus/test/Test/Unit/BoundaryConditionsEnhancedQuickCheckSpec.hs",
    "/home/runner/work/Typus/Typus/test/Test/Unit/CompilerIRConsistencyQuickCheckSpec.hs",
    "/home/runner/work/Typus/Typus/test/Test/Unit/NewParserQuickCheckSpec.hs",
    "/home/runner/work/Typus/Typus/test/Test/Unit/SourceLocationSpanQuickCheckSpec.hs"
]

def fix_span_conflicts_in_file(filepath):
    """Fix span variable name conflicts in a file"""
    try:
        with open(filepath, 'r') as f:
            content = f.read()
        
        original_content = content
        
        # Fix specific patterns where 'span' variable conflicts with Prelude.span
        # Pattern 1: spanStart span -> spanStart sourceSpan
        content = re.sub(r'spanStart\s+span\b', 'spanStart sourceSpan', content)
        
        # Pattern 2: spanEnd span -> spanEnd sourceSpan  
        content = re.sub(r'spanEnd\s+span\b', 'spanEnd sourceSpan', content)
        
        # Pattern 3: isValidSpan span -> isValidSpan sourceSpan
        content = re.sub(r'isValidSpan\s+span\b', 'isValidSpan sourceSpan', content)
        
        # Pattern 4: isValidBlockSpan span -> isValidBlockSpan sourceSpan
        content = re.sub(r'isValidBlockSpan\s+span\b', 'isValidBlockSpan sourceSpan', content)
        
        # Pattern 5: In case expressions: case span of -> case sourceSpan of
        content = re.sub(r'case\s+span\s+of', 'case sourceSpan of', content)
        
        # Pattern 6: In patterns: SourceSpan s e -> where span is used as the variable
        # Need to be more careful here, let's look at specific contexts
        
        # Pattern 7: Direct usage: CodeBlock ... span -> CodeBlock ... sourceSpan
        content = re.sub(r'CodeBlock\s+([^\n]+)\s+span\b', r'CodeBlock \1 sourceSpan', content)
        
        # Pattern 8: Variable assignment: span = ... -> sourceSpan = ...
        content = re.sub(r'(\s+)span\s*=', r'\1sourceSpan =', content)
        
        # Pattern 9: In let bindings: let span = ... -> let sourceSpan = ...
        content = re.sub(r'let\s+span\s*=', 'let sourceSpan =', content)
        
        # Write back if changed
        if content != original_content:
            with open(filepath, 'w') as f:
                f.write(content)
            print(f"Fixed span conflicts in {filepath}")
            return True
        else:
            print(f"No span conflicts found in {filepath}")
            return False
            
    except Exception as e:
        print(f"Error processing {filepath}: {e}")
        return False

def fix_source_location_test_spec():
    """Fix the specific pattern matching issue in SourceLocationTestSpec.hs"""
    filepath = "/home/runner/work/Typus/Typus/test/Test/Unit/SourceLocationTestSpec.hs"
    
    try:
        with open(filepath, 'r') as f:
            content = f.read()
        
        original_content = content
        
        # Fix the specific pattern matching issue
        # Change: case span of SourceSpan s e -> s == start && e == end
        # To:     case sourceSpan of SourceSpan s e -> s == start && e == end
        content = re.sub(
            r'case\s+span\s+of\s+SourceSpan\s+s\s+e\s*->',
            'case sourceSpan of SourceSpan s e ->',
            content
        )
        
        # Also fix the variable name if needed
        content = re.sub(r'let\s+span\s*=', 'let sourceSpan =', content)
        
        # Write back if changed
        if content != original_content:
            with open(filepath, 'w') as f:
                f.write(content)
            print(f"Fixed SourceLocationTestSpec.hs pattern matching issue")
            return True
        else:
            print(f"No changes needed for SourceLocationTestSpec.hs")
            return False
            
    except Exception as e:
        print(f"Error processing SourceLocationTestSpec.hs: {e}")
        return False

def main():
    """Main function to fix all files"""
    fixed_count = 0
    for filepath in files_to_fix:
        if fix_span_conflicts_in_file(filepath):
            fixed_count += 1
    
    # Fix the specific SourceLocationTestSpec.hs issue
    if fix_source_location_test_spec():
        fixed_count += 1
    
    print(f"\nFixed span conflicts in {fixed_count} files")

if __name__ == "__main__":
    main()