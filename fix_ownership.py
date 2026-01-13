#!/usr/bin/env python3
"""
Fix remaining errors in CoreOwnershipPropertiesQuickCheckSpec.hs
"""

import re

def fix_file():
    file_path = "test/Test/Unit/CoreOwnershipPropertiesQuickCheckSpec.hs"
    with open(file_path, 'r') as f:
        content = f.read()
    
    # Fix string operations
    content = re.sub(
        r"not \(null \(T\.unpack f\)\) && not \(null \(T\.unpack t\)\)",
        "not (T.null f) && not (T.null t)",
        content
    )
    
    # Fix Borrowed pattern
    content = re.sub(
        r"Borrowed -> property True",
        "Borrowed _ -> property True",
        content
    )
    
    with open(file_path, 'w') as f:
        f.write(content)

if __name__ == "__main__":
    import os
    os.chdir("/home/runner/work/Typus/Typus")
    fix_file()