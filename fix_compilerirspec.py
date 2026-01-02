#!/usr/bin/env python3

import re

# Read the file
with open('/home/runner/work/Typus/Typus/test/Test/Unit/CompilerIRSpec.hs', 'r') as f:
    content = f.read()

# Fix the SourceIR constructor - it needs TypusFile, not String
content = re.sub(r'sourceIR = SourceIR code', 'sourceIR = SourceIR (TypusFile defaultFileDirectives [] [] [])', content)

# Fix the SemanticIR constructor - it needs proper type
content = re.sub(r'semanticIR = SemanticIR typeInfo', 'semanticIR = SemanticIR typeInfo', content)

# Fix the GoIR constructor - it needs GoModule, not String
content = re.sub(r'goIR = GoIR goFunction', 'goIR = GoIR (GoModule [] [])', content)

# Write the file back
with open('/home/runner/work/Typus/Typus/test/Test/Unit/CompilerIRSpec.hs', 'w') as f:
    f.write(content)

print("Fixed CompilerIRSpec.hs")