#!/usr/bin/env python3

import re

# Read the cabal file
with open('typus.cabal', 'r') as f:
    content = f.read()

# Define our modules to remove from benchmark
our_modules = [
    'Test.Unit.CoreUtilsSpec',
    'Test.Unit.CoreSourceLocationSpec',
    'Test.Unit.CoreParserSpec',
    'Test.Unit.CoreErrorHandlerSpec',
    'Test.Unit.CoreOwnershipSpec',
    'Test.Unit.CoreQuickCheckPropertiesSpec',
    'Test.Unit.CoreIntegrationSpec'
]

# Find the benchmark section and remove our modules
# Pattern to match benchmark section
benchmark_pattern = r'(benchmark typus-bench.*?other-modules:.*?)(.*?)(\n    build-depends:)'

def remove_our_modules_from_benchmark(match):
    before = match.group(1)
    modules_list = match.group(2)
    after = match.group(3)
    
    # Split modules by comma and filter out our modules
    modules = [m.strip() for m in modules_list.split(',')]
    filtered_modules = [m for m in modules if m not in our_modules]
    
    # Reconstruct the modules list
    new_modules_list = ',\n        '.join(filtered_modules) + '\n        '
    
    return before + new_modules_list + after

# Apply the replacement
new_content = re.sub(benchmark_pattern, remove_our_modules_from_benchmark, content, flags=re.DOTALL)

# Write back to file
with open('typus.cabal', 'w') as f:
    f.write(new_content)

print("Removed our modules from benchmark section")