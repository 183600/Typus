#!/usr/bin/env python3
import os
import re

def fix_specific_file(file_path):
    """Fix a specific file with mixed imports and function definitions"""
    try:
        with open(file_path, 'r', encoding='utf-8') as f:
            content = f.read()
        
        # Fix specific file
        if "FreshCabalQuickCheckSpec.hs" in file_path:
            content = re.sub(r'import TestSupport\.Arbitrary\s*\n\s*\(c:_\) -> property \(not \(isSpace c\) && not \(isSpace \(last t\)\)', 
                            'import TestSupport.Arbitrary ()\n\n-- Test cases', content, flags=re.DOTALL)
        
        elif "NewSimpleCabalQuickCheckSpec.hs" in file_path:
            content = re.sub(r'import TestSupport\.Arbitrary\s*\n\s*\(c:_\) -> property \(not \(isSpace c\) && not \(isSpace \(last t\)\)', 
                            'import TestSupport.Arbitrary ()\n\n-- Test cases', content, flags=re.DOTALL)
        
        elif "TypeSystemQuickCheckSpec.hs" in file_path:
            content = re.sub(r'import TestSupport\.Arbitrary\s*\n\s*\(c:_\) -> property \(not \(isSpace c\) && not \(isSpace \(last t\)\)', 
                            'import TestSupport.Arbitrary ()\n\n-- Test cases', content, flags=re.DOTALL)
        
        elif "UtilsBasicQuickCheckSpec.hs" in file_path:
            content = re.sub(r'import TestSupport\.Arbitrary\s*\n\s*\(c:_\) -> property \(not \(isSpace c\) && not \(isSpace \(last t\)\)', 
                            'import TestSupport.Arbitrary ()\n\n-- Test cases', content, flags=re.DOTALL)
        
        elif "UtilsRobustnessQuickCheckSpec.hs" in file_path:
            content = re.sub(r'import TestSupport\.Arbitrary\s*\n\s*\(c:_\) -> property \(not \(isSpace c\) && not \(isSpace \(last t\)\)', 
                            'import TestSupport.Arbitrary ()\n\n-- Test cases', content, flags=re.DOTALL)
        
        # Write back if changed
        with open(file_path, 'w', encoding='utf-8') as f:
            f.write(content)
        
        print(f"Fixed specific file {file_path}")
        return True
    except Exception as e:
        print(f"Error processing {file_path}: {e}")
        return False

def main():
    # Get list of files with errors
    result = os.popen('cabal test --test-show-details=never 2>&1 | grep -i "error:" | cut -d: -f1 | sort | uniq').read()
    error_files = [f.strip() for f in result.split('\n') if f.strip()]
    
    print(f"Found {len(error_files)} files with errors")
    
    for file_path in error_files:
        if os.path.exists(file_path):
            fix_specific_file(file_path)
    
    print("Fixed all error files")

if __name__ == "__main__":
    main()