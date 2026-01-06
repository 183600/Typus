#!/usr/bin/env python3

# Read the cabal file
with open('/home/runner/work/Typus/Typus/typus.cabal', 'r') as f:
    content = f.read()

# Find the test-suite section and replace it
lines = content.split('\n')
new_lines = []
in_test_suite = False
found_other_modules = False
modules_to_keep = [
    'Test.Unit.UtilsSpec',
    'Test.Unit.ParserSpec',
    'Test.Unit.CompilerSpec',
    'Test.Unit.ErrorHandlerCoreSpec',
    'Test.Unit.DependenciesCoreSpec',
    'Test.Unit.OwnershipSpec',
    'Test.Unit.SourceLocationSpec',
    'Test.Unit.SyntaxValidatorSpec',
    'Test.Unit.TypeSystemSpec',
    'Test.Unit.SymbolTableSpec',
    'Test.Unit.ValueAnalysisSpec',
    'Test.Unit.GoToolchainSpec',
    'Test.Unit.VerbositySpec',
    'TestSupport.Verbosity',
    'TestSupport.QuickCheck',
    'TestSupport.Arbitrary',
    'TestSupport.ExtendedArbitrary'
]

for line in lines:
    if line.startswith('test-suite typus-test'):
        in_test_suite = True
        new_lines.append(line)
    elif in_test_suite and line.strip() == '':
        # End of test-suite section
        in_test_suite = False
        new_lines.append(line)
    elif in_test_suite:
        if line.startswith('    other-modules:'):
            found_other_modules = True
            new_lines.append(line)
            for module in modules_to_keep:
                new_lines.append(f'        {module},')
        elif found_other_modules and line.startswith('        Test.Unit.'):
            # Skip all the old modules
            continue
        elif not found_other_modules:
            new_lines.append(line)
        else:
            new_lines.append(line)
    else:
        new_lines.append(line)

# Write the modified cabal file
with open('/home/runner/work/Typus/Typus/typus.cabal', 'w') as f:
    f.write('\n'.join(new_lines))

print("Cabal file modified successfully")