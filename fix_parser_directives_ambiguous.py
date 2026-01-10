#!/usr/bin/env python3
import re

# Read the file
with open('/home/runner/work/Typus/Typus/test/Test/Unit/TestParserDirectivesSpec.hs', 'r') as f:
    content = f.read()

# Fix ambiguous references by removing duplicate type definitions
# and using Parser module versions

# First, let's remove all local type definitions and field accessors
content = re.sub(r'-- Simplified Parser types for testing\n.*?-- Helper functions\n', '-- Helper functions\n', content, flags=re.DOTALL)

# Fix function signatures to use Parser module types
content = re.sub(r'parseFileDirectives :: String -> Either String FileDirectives', 
                r'parseFileDirectives :: String -> Either String Parser.FileDirectives', content)

# Fix all ambiguous field references
content = re.sub(r'\bfdOwnership\b', r'Parser.fdOwnership', content)
content = re.sub(r'\bfdDependentTypes\b', r'Parser.fdDependentTypes', content)
content = re.sub(r'\bfdConstraints\b', r'Parser.fdConstraints', content)
content = re.sub(r'\bbdOwnership\b', r'Parser.bdOwnership', content)
content = re.sub(r'\bbdDependentTypes\b', r'Parser.bdDependentTypes', content)
content = re.sub(r'\bbdConstraints\b', r'Parser.bdConstraints', content)
content = re.sub(r'\bcbDirectives\b', r'Parser.cbDirectives', content)
content = re.sub(r'\bcbContent\b', r'Parser.cbContent', content)
content = re.sub(r'\btfDirectives\b', r'Parser.tfDirectives', content)
content = re.sub(r'\btfBlocks\b', r'Parser.tfBlocks', content)
content = re.sub(r'\b!!\b', r'Prelude.!!', content)

# Fix FileDirectives constructor references
content = re.sub(r'\$ FileDirectives \(', r'$ Parser.FileDirectives (', content)

# Write the file back
with open('/home/runner/work/Typus/Typus/test/Test/Unit/TestParserDirectivesSpec.hs', 'w') as f:
    f.write(content)

print("Fixed ambiguous references in TestParserDirectivesSpec.hs")