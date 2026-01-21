#!/usr/bin/env python3
"""
Fix double Utils references in SimpleQuickCheckTestSuite.hs
"""

import re

def fix_double_utils():
    with open('/home/runner/work/Typus/Typus/test/Test/Unit/SimpleQuickCheckTestSuite.hs', 'r') as f:
        content = f.read()
    
    # Replace Utils.Utils.function with Utils.function
    content = re.sub(r'Utils\.Utils\.', 'Utils.', content)
    
    with open('/home/runner/work/Typus/Typus/test/Test/Unit/SimpleQuickCheckTestSuite.hs', 'w') as f:
        f.write(content)
    
    print("Fixed double Utils references!")

if __name__ == "__main__":
    fix_double_utils()