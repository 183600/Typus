#!/usr/bin/env python3
import re

# Read the file
with open('/home/runner/work/Typus/Typus/test/Test/Unit/TestQuickCheckPropertiesSpec.hs', 'r') as f:
    content = f.read()

# Fix ambiguous references by using qualified imports
content = re.sub(r'\btrim\b', r'Utils.trim', content)
content = re.sub(r'\bsplitBy\b', r'Utils.splitBy', content)
content = re.sub(r'\bremoveComments\b', r'Utils.removeComments', content)
content = re.sub(r'\bnormalizeIndentation\b', r'Utils.normalizeIndentation', content)
content = re.sub(r'\bisValidChar\b', r'Utils.isValidChar', content)
content = re.sub(r'\bsafeProcessString\b', r'Utils.safeProcessString', content)
content = re.sub(r'\bposAfter\b', r'SourceLocation.posAfter', content)
content = re.sub(r'\bposLine\b', r'SourceLocation.posLine', content)
content = re.sub(r'\bposColumn\b', r'SourceLocation.posColumn', content)
content = re.sub(r'\bposOffset\b', r'SourceLocation.posOffset', content)
content = re.sub(r'\bmergeSpans\b', r'SourceLocation.mergeSpans', content)
content = re.sub(r'\bspanBetween\b', r'SourceLocation.spanBetween', content)
content = re.sub(r'\bspanStart\b', r'SourceLocation.spanStart', content)
content = re.sub(r'\bspanEnd\b', r'SourceLocation.spanEnd', content)
content = re.sub(r'\blocatedAt\b', r'SourceLocation.locatedAt', content)
content = re.sub(r'\blocatedWithSpan\b', r'SourceLocation.locatedWithSpan', content)
content = re.sub(r'\badvancePosBy\b', r'SourceLocation.advancePosBy', content)
content = re.sub(r'\berrorAt\b', r'ErrorHandler.errorAt', content)
content = re.sub(r'\bformatError\b', r'ErrorHandler.formatError', content)
content = re.sub(r'\bcombineErrors\b', r'ErrorHandler.combineErrors', content)
content = re.sub(r'\bunifyTypes\b', r'Dependencies.unifyTypes', content)
content = re.sub(r'\bapplyTypeSubstitution\b', r'Dependencies.applyTypeSubstitution', content)
content = re.sub(r'\baddType\b', r'Dependencies.addType', content)
content = re.sub(r'\bcheckType\b', r'Dependencies.checkType', content)
content = re.sub(r'\blookupType\b', r'Dependencies.lookupType', content)
content = re.sub(r'\bIRFunction\b', r'Compiler.IR.IRFunction', content)
content = re.sub(r'\bIRInt\b', r'Compiler.IR.IRInt', content)
content = re.sub(r'\bIRString\b', r'Compiler.IR.IRString', content)
content = re.sub(r'\bIRBool\b', r'Compiler.IR.IRBool', content)
content = re.sub(r'\bemptySpan\b', r'SourceLocation.emptySpan', content)

# Write the file back
with open('/home/runner/work/Typus/Typus/test/Test/Unit/TestQuickCheckPropertiesSpec.hs', 'w') as f:
    f.write(content)

print("Fixed ambiguous references in TestQuickCheckPropertiesSpec.hs")