#!/usr/bin/env python3
"""
Fix Utils references in SimpleQuickCheckTestSuite.hs
"""

import re

def fix_utils_references():
    with open('/home/runner/work/Typus/Typus/test/Test/Unit/SimpleQuickCheckTestSuite.hs', 'r') as f:
        content = f.read()
    
    # Replace all direct function calls with Utils.function
    functions = ['trim', 'splitBy', 'splitByCollapsed', 'splitByComma', 'splitByCommaCollapsed',
                 'removeLineComments', 'removeComments', 'normalizeIndentation', 'safeProcessString', 'breakOn']
    
    for func in functions:
        # Replace function calls not already qualified
        pattern = rf'\b{func}\b'
        replacement = f'Utils.{func}'
        content = re.sub(pattern, replacement, content)
    
    with open('/home/runner/work/Typus/Typus/test/Test/Unit/SimpleQuickCheckTestSuite.hs', 'w') as f:
        f.write(content)
    
    print("Fixed all Utils references!")

if __name__ == "__main__":
    fix_utils_references()