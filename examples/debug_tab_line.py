#!/usr/bin/env python3

# Test the exact content of the problematic line
line = '	"runtime"'
print(f"Original line: {repr(line)}")

# Test different trimming approaches
import re

# Simple strip
trimmed_simple = line.strip()
print(f"Simple strip: {repr(trimmed_simple)}")

# Remove only leading whitespace
trimmed_lstrip = line.lstrip()
print(f"Left strip: {repr(trimmed_lstrip)}")

# Remove only trailing whitespace  
trimmed_rstrip = line.rstrip()
print(f"Right strip: {repr(trimmed_rstrip)}")

# Check if any match our target patterns
patterns = [
    '"runtime"',
    'runtime',
    '	"runtime"',
    '\\t"runtime"'
]

for pattern in patterns:
    print(f"Pattern {repr(pattern)} matches simple strip: {trimmed_simple == pattern}")
    print(f"Pattern {repr(pattern)} matches left strip: {trimmed_lstrip == pattern}")
    print(f"Pattern {repr(pattern)} matches right strip: {trimmed_rstrip == pattern}")
    print(f"Pattern {repr(pattern)} in original line: {pattern in line}")
    print("---")