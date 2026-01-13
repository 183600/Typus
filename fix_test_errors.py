#!/usr/bin/env python3
"""
Script to fix compilation errors in the test files.
"""

import re
import os

def fix_core_error_handler_properties():
    """Fix CoreErrorHandlerPropertiesQuickCheckSpec.hs"""
    file_path = "test/Test/Unit/CoreErrorHandlerPropertiesQuickCheckSpec.hs"
    
    with open(file_path, 'r') as f:
        content = f.read()
    
    # Replace ErrorSeverity imports
    content = re.sub(
        r"import Compiler\.Errors\.Core \(ErrorSeverity\(\.\.\), ErrorContext\(\.\.\)\)",
        "import Compiler.Errors.Core (ErrorSeverity(..), ErrorContext(..), ErrorLocation(..))",
        content
    )
    
    # Replace Critical with Error
    content = re.sub(r"Critical", "Error", content)
    
    # Replace ErrorMessage with a simple string
    content = re.sub(r"ErrorMessage", "String", content)
    
    # Replace ErrorHandler with ErrorCollector
    content = re.sub(r"ErrorHandler", "ErrorCollector", content)
    
    # Fix ErrorMessage constructor
    content = re.sub(
        r"ErrorMessage msg _",
        '"Error: " <> msg',
        content
    )
    
    with open(file_path, 'w') as f:
        f.write(content)
    
    print(f"Fixed {file_path}")

def fix_core_go_toolchain_properties():
    """Fix CoreGoToolchainPropertiesQuickCheckSpec.hs"""
    file_path = "test/Test/Unit/CoreGoToolchainPropertiesQuickCheckSpec.hs"
    
    with open(file_path, 'r') as f:
        content = f.read()
    
    # Replace GoModule, GoFunction, GoVariable, GoType with simple types
    content = re.sub(r"GoModule", "String", content)
    content = re.sub(r"GoFunction", "String", content)
    content = re.sub(r"GoVariable", "String", content)
    content = re.sub(r"GoType", "String", content)
    
    with open(file_path, 'w') as f:
        f.write(content)
    
    print(f"Fixed {file_path}")

def fix_core_integration_properties():
    """Fix CoreIntegrationPropertiesQuickCheckSpec.hs"""
    file_path = "test/Test/Unit/CoreIntegrationPropertiesQuickCheckSpec.hs"
    
    with open(file_path, 'r') as f:
        content = f.read()
    
    # Fix Ownership.analyzeOwnership call
    content = re.sub(
        r"Ownership\.analyzeOwnership parsed",
        "Ownership.analyzeOwnership (show parsed)",
        content
    )
    
    # Fix property return type
    content = re.sub(
        r"Left _ -> property True",
        "Left _ -> property True",
        content
    )
    
    # Fix parseTypus call with T.Text
    content = re.sub(
        r"parseTypus code",
        "parseTypus (T.unpack code)",
        content
    )
    
    # Fix T.replicate with string
    content = re.sub(
        r'T\.replicate size "x"',
        'T.pack (replicate size \'x\')',
        content
    )
    
    # Fix T.replicate with string in other places
    content = re.sub(
        r'T\.replicate 10000 "x"',
        'T.pack (replicate 10000 \'x\')',
        content
    )
    
    content = re.sub(
        r'T\.replicate size "x"',
        'T.pack (replicate size \'x\')',
        content
    )
    
    content = re.sub(
        r'"func main\(\) { " <> T\.replicate size "x" <> " }"',
        'T.pack ("func main() { " ++ replicate size \'x\' ++ " }")',
        content
    )
    
    content = re.sub(
        r'T\.replicate depth "{"',
        'T.pack (replicate depth \'{\')',
        content
    )
    
    content = re.sub(
        r'"type Large struct { " <> T\.replicate size "Field int; " <> " }"',
        'T.pack ("type Large struct { " ++ replicate size "Field int; " ++ " }")',
        content
    )
    
    with open(file_path, 'w') as f:
        f.write(content)
    
    print(f"Fixed {file_path}")

def fix_core_ownership_properties():
    """Fix CoreOwnershipPropertiesQuickCheckSpec.hs"""
    file_path = "test/Test/Unit/CoreOwnershipPropertiesQuickCheckSpec.hs"
    
    with open(file_path, 'r') as f:
        content = f.read()
    
    # Fix the leading comma in testProperty
    content = re.sub(
        r", testProperty",
        "testProperty",
        content
    )
    
    with open(file_path, 'w') as f:
        f.write(content)
    
    print(f"Fixed {file_path}")

def fix_core_parser_properties():
    """Fix CoreParserPropertiesQuickCheckSpec.hs"""
    file_path = "test/Test/Unit/CoreParserPropertiesQuickCheckSpec.hs"
    
    with open(file_path, 'r') as f:
        content = f.read()
    
    # Fix the leading comma in testProperty
    content = re.sub(
        r", testProperty",
        "testProperty",
        content
    )
    
    with open(file_path, 'w') as f:
        f.write(content)
    
    print(f"Fixed {file_path}")

def fix_core_performance_properties():
    """Fix CorePerformancePropertiesQuickCheckSpec.hs"""
    file_path = "test/Test/Unit/CorePerformancePropertiesQuickCheckSpec.hs"
    
    with open(file_path, 'r') as f:
        content = f.read()
    
    # Fix T.replicate with string
    content = re.sub(
        r'T\.replicate size "x"',
        'T.pack (replicate size \'x\')',
        content
    )
    
    content = re.sub(
        r'T\.replicate 10000 "x"',
        'T.pack (replicate 10000 \'x\')',
        content
    )
    
    content = re.sub(
        r'"func main\(\) { " <> T\.replicate size "x" <> " }"',
        'T.pack ("func main() { " ++ replicate size \'x\' ++ " }")',
        content
    )
    
    content = re.sub(
        r'T\.replicate depth "{"',
        'T.pack (replicate depth \'{\')',
        content
    )
    
    content = re.sub(
        r'"type Large struct { " <> T\.replicate size "Field int; " <> " }"',
        'T.pack ("type Large struct { " ++ replicate size "Field int; " ++ " }")',
        content
    )
    
    # Fix parseTypus with T.Text
    content = re.sub(
        r"parseTypus input",
        "parseTypus (T.unpack input)",
        content
    )
    
    content = re.sub(
        r"parseTypus largeFile",
        "parseTypus (T.unpack largeFile)",
        content
    )
    
    content = re.sub(
        r"parseTypus program",
        "parseTypus (T.unpack program)",
        content
    )
    
    content = re.sub(
        r"parseTypus nested",
        "parseTypus (T.unpack nested)",
        content
    )
    
    with open(file_path, 'w') as f:
        f.write(content)
    
    print(f"Fixed {file_path}")

def fix_core_source_location_properties():
    """Fix CoreSourceLocationPropertiesQuickCheckSpec.hs"""
    file_path = "test/Test/Unit/CoreSourceLocationPropertiesQuickCheckSpec.hs"
    
    with open(file_path, 'r') as f:
        content = f.read()
    
    # Add posAfter to imports
    content = re.sub(
        r"import SourceLocation \(SourcePos\(\.\.\), SourceSpan\(\.\.\), Located\(\.\.\), startPos, emptySpan, spanFrom, mergeSpans\)",
        "import SourceLocation (SourcePos(..), SourceSpan(..), Located(..), startPos, emptySpan, spanFrom, mergeSpans, posAfter)",
        content
    )
    
    # Fix locatedPos
    content = re.sub(
        r"locatedPos located",
        "locatedValue located",
        content
    )
    
    with open(file_path, 'w') as f:
        f.write(content)
    
    print(f"Fixed {file_path}")

if __name__ == "__main__":
    os.chdir("/home/runner/work/Typus/Typus")
    
    fix_core_error_handler_properties()
    fix_core_go_toolchain_properties()
    fix_core_integration_properties()
    fix_core_ownership_properties()
    fix_core_parser_properties()
    fix_core_performance_properties()
    fix_core_source_location_properties()
    
    print("All test files have been fixed!")