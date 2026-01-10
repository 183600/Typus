#!/usr/bin/env python3
import re

# Read the file
with open('/home/runner/work/Typus/Typus/test/Test/Unit/TestMemorySafetySpec.hs', 'r') as f:
    content = f.read()

# Replace ambiguous references with qualified ones
content = re.sub(r'\btrim\b', 'Utils.trim', content)
content = re.sub(r'\bremoveComments\b', 'Utils.removeComments', content)
content = re.sub(r'\bnormalizeIndentation\b', 'Utils.normalizeIndentation', content)
content = re.sub(r'\bsafeProcessString\b', 'Utils.safeProcessString', content)
content = re.sub(r'\bposAt\b', 'SourceLocation.posAt', content)
content = re.sub(r'\bspanBetween\b', 'SourceLocation.spanBetween', content)
content = re.sub(r'\bmergeSpans\b', 'SourceLocation.mergeSpans', content)
content = re.sub(r'\berrorAt\b', 'ErrorHandler.errorAt', content)
content = re.sub(r'\bformatError\b', 'ErrorHandler.formatError', content)
content = re.sub(r'\bparseTypus\b', 'Parser.parseTypus', content)
content = re.sub(r'\bnewDependentTypeChecker\b', 'Dependencies.newDependentTypeChecker', content)
content = re.sub(r'\baddType\b', 'Dependencies.addType', content)
content = re.sub(r'\baddConstraint\b', 'Dependencies.addConstraint', content)
content = re.sub(r'\bsolveConstraints\b', 'Dependencies.solveConstraints', content)
content = re.sub(r'\banalyzeOwnership\b', 'Ownership.analyzeOwnership', content)
content = re.sub(r'\blocatedWithSpan\b', 'SourceLocation.locatedWithSpan', content)
content = re.sub(r'\bSourcePos\b', 'SourceLocation.SourcePos', content)
content = re.sub(r'\bTypeExpr\b', 'Dependencies.TypeExpr', content)
content = re.sub(r'\bTypeVar\b', 'Dependencies.TypeVar', content)
content = re.sub(r'\bTypeConstructor\b', 'Dependencies.TypeConstructor', content)
content = re.sub(r'\bTypeArrow\b', 'Dependencies.TypeArrow', content)
content = re.sub(r'\bEqualityConstraint\b', 'Dependencies.EqualityConstraint', content)

# Write the file back
with open('/home/runner/work/Typus/Typus/test/Test/Unit/TestMemorySafetySpec.hs', 'w') as f:
    f.write(content)

print("Fixed ambiguous references in TestMemorySafetySpec.hs")