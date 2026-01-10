#!/usr/bin/env python3
import re

# Read the file
with open('/home/runner/work/Typus/Typus/test/Test/Unit/TestQuickCheckPropertiesSpec.hs', 'r') as f:
    content = f.read()

# Fix the invalid type signature by removing the qualified name
content = re.sub(r'Dependencies\.addType :: String -> TypeExpr -> DependentTypeChecker -> DependentTypeChecker', 
                r'addType :: String -> TypeExpr -> DependentTypeChecker -> DependentTypeChecker', content)
content = re.sub(r'Dependencies\.addType name t checker =', 
                r'addType name t checker =', content)

# Write the file back
with open('/home/runner/work/Typus/Typus/test/Test/Unit/TestQuickCheckPropertiesSpec.hs', 'w') as f:
    f.write(content)

print("Fixed invalid type signature in TestQuickCheckPropertiesSpec.hs")