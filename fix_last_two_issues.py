#!/usr/bin/env python3
"""
Fix the last two issues
"""

import os
import re

def fix_new_parser_spec():
    """Fix startPos import issue in NewParserQuickCheckSpec.hs"""
    filepath = "/home/runner/work/Typus/Typus/test/Test/Unit/NewParserQuickCheckSpec.hs"
    
    try:
        with open(filepath, 'r') as f:
            content = f.read()
        
        original_content = content
        
        # Add startPos to the SourceLocation import
        content = re.sub(
            r'import SourceLocation \(SourcePos\(\.\.\.\), SourceSpan\(\.\.\.\), spanStart, spanEnd\)',
            'import SourceLocation (SourcePos(..), SourceSpan(..), spanStart, spanEnd, startPos)',
            content
        )
        
        # Fix the sourceSpan definition to use a simpler approach
        content = re.sub(
            r'let sourceSpan = SourceSpan \(startPos str\) \(startPos str\)',
            'let sourceSpan = SourceSpan (SourcePos 0 0 0) (SourcePos 0 0 0)',
            content
        )
        
        # Write back if changed
        if content != original_content:
            with open(filepath, 'w') as f:
                f.write(content)
            print(f"Fixed startPos import in {filepath}")
            return True
        else:
            print(f"No changes needed for {filepath}")
            return False
            
    except Exception as e:
        print(f"Error processing {filepath}: {e}")
        return False

def fix_source_location_test():
    """Fix sourceSpan scope issue in SourceLocationTestSpec.hs"""
    filepath = "/home/runner/work/Typus/Typus/test/Test/Unit/SourceLocationTestSpec.hs"
    
    try:
        with open(filepath, 'r') as f:
            content = f.read()
        
        original_content = content
        
        # Fix the property function to properly return the boolean result
        content = re.sub(
            r'prop_sourceSpanPreservation :: SourcePos -> SourcePos -> Bool\s*\n\s*prop_sourceSpanPreservation start end =\s*\n\s*let sourceSpan = SourceSpan start end in case sourceSpan of SourceSpan s e -> s == start && e == end\s*\n\s*in sourceSpan',
            '''prop_sourceSpanPreservation :: SourcePos -> SourcePos -> Bool
prop_sourceSpanPreservation start end =
  let sourceSpan = SourceSpan start end
  in case sourceSpan of SourceSpan s e -> s == start && e == end''',
            content,
            flags=re.MULTILINE | re.DOTALL
        )
        
        # Write back if changed
        if content != original_content:
            with open(filepath, 'w') as f:
                f.write(content)
            print(f"Fixed sourceSpan scope in {filepath}")
            return True
        else:
            print(f"No changes needed for {filepath}")
            return False
            
    except Exception as e:
        print(f"Error processing {filepath}: {e}")
        return False

def main():
    """Main function to fix all issues"""
    fixed_count = 0
    
    if fix_new_parser_spec():
        fixed_count += 1
    
    if fix_source_location_test():
        fixed_count += 1
    
    print(f"\nFixed issues in {fixed_count} files")

if __name__ == "__main__":
    main()