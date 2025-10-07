#!/usr/bin/env python3

# Simulate the exact Haskell logic
def trim(s):
    """Haskell trim function equivalent"""
    return s.strip()

def isImportLine(line):
    """Simulate the current isImportLine function"""
    trimmed = trim(line)
    return (trimmed == "fmt" or trimmed == "sync" or trimmed == "time" or 
            trimmed == "unsafe" or trimmed == "os" or trimmed == "io" or 
            trimmed == "strings" or trimmed == "math" or trimmed == "runtime" or
            trimmed == "\"fmt\"" or trimmed == "\"sync\"" or trimmed == "\"time\"" or 
            trimmed == "\"unsafe\"" or trimmed == "\"os\"" or trimmed == "\"io\"" or 
            trimmed == "\"strings\"" or trimmed == "\"math\"" or trimmed == "\"runtime\"")

# Test cases
test_lines = [
    '\t"runtime"',
    'runtime',
    '"runtime"',
    '    "runtime"',
    '\t"fmt"',
    'import "runtime"',
    'package main'
]

print("Testing isImportLine function:")
for line in test_lines:
    trimmed = trim(line)
    should_filter = isImportLine(line)
    print(f"Line: {repr(line)} -> Trimmed: {repr(trimmed)} -> Should filter: {should_filter}")

print("\nTesting cleanCodeBlocks logic:")
def cleanCodeBlocks(content):
    lines = content.split('\n')
    filtered_lines = []
    for line in lines:
        # Simulate the Haskell logic
        if not (line.startswith("package main") or 
                line.startswith("import") or
                isImportLine(line)):
            filtered_lines.append(line)
    return '\n'.join(filtered_lines)

# Test with actual content
test_content = '''package main

import (
    "fmt"
    "time"
)

\t"runtime"

func main() {
\tfmt.Println("Hello, world!")
\tfmt.Println("Runtime:", runtime.GOOS)
}'''

print("Original content:")
print(repr(test_content))
print("\nCleaned content:")
cleaned = cleanCodeBlocks(test_content)
print(repr(cleaned))
print("\nCleaned content (formatted):")
print(cleaned)