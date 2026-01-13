#!/usr/bin/env python3
"""
Final comprehensive fix script.
"""

import re
import os

def fix_all_files():
    """Fix all remaining compilation errors"""
    
    # Fix CoreErrorHandlerPropertiesQuickCheckSpec.hs
    file_path = "test/Test/Unit/CoreErrorHandlerPropertiesQuickCheckSpec.hs"
    with open(file_path, 'r') as f:
        content = f.read()
    
    # Add type annotations
    content = re.sub(
        r"testProperty \"Error messages contain useful information\" \$\s+\\ errorMsg ->",
        "testProperty \"Error messages contain useful information\" $ \\(errorMsg :: T.Text) ->",
        content
    )
    
    content = re.sub(
        r"testProperty \"Error severity levels are correctly ordered\" \$\s+\\ severity1 severity2 ->",
        "testProperty \"Error severity levels are correctly ordered\" $ \\(severity1 :: ErrorSeverity) (severity2 :: ErrorSeverity) ->",
        content
    )
    
    content = re.sub(
        r"testProperty \"Error filtering preserves important errors\" \$\s+\\ errors ->",
        "testProperty \"Error filtering preserves important errors\" $ \\(errors :: [String]) ->",
        content
    )
    
    with open(file_path, 'w') as f:
        f.write(content)
    
    # Fix CoreGoToolchainPropertiesQuickCheckSpec.hs
    file_path = "test/Test/Unit/CoreGoToolchainPropertiesQuickCheckSpec.hs"
    with open(file_path, 'r') as f:
        content = f.read()
    
    # Add type annotations
    content = re.sub(
        r"testProperty \"Go module generation preserves structure\" \$\s+\\ \\(moduleName :: T\.Text\) ->",
        "testProperty \"Go module generation preserves structure\" $ \\(moduleName :: T.Text) ->",
        content
    )
    
    content = re.sub(
        r"testProperty \"Go function signatures are valid\" \$\s+\\ funcName params ->",
        "testProperty \"Go function signatures are valid\" $ \\(funcName :: T.Text) (params :: [T.Text]) ->",
        content
    )
    
    content = re.sub(
        r"testProperty \"Go module dependencies are resolved correctly\" \$\s+\\ \\(modules :: \[String]\) ->",
        "testProperty \"Go module dependencies are resolved correctly\" $ \\(modules :: [String]) ->",
        content
    )
    
    with open(file_path, 'w') as f:
        f.write(content)
    
    # Fix CoreIntegrationPropertiesQuickCheckSpec.hs
    file_path = "test/Test/Unit/CoreIntegrationPropertiesQuickCheckSpec.hs"
    with open(file_path, 'r') as f:
        content = f.read()
    
    # Fix property return type
    content = re.sub(
        r"Left _ -> property True",
        "Left _ -> property True",
        content
    )
    
    with open(file_path, 'w') as f:
        f.write(content)
    
    # Fix CoreOwnershipPropertiesQuickCheckSpec.hs
    file_path = "test/Test/Unit/CoreOwnershipPropertiesQuickCheckSpec.hs"
    with open(file_path, 'r') as f:
        content = f.read()
    
    # Fix string operations
    content = re.sub(
        r"not \(T\.null f\) && not \(T\.null t\)",
        "not (null (T.unpack f)) && not (null (T.unpack t))",
        content
    )
    
    with open(file_path, 'w') as f:
        f.write(content)
    
    # Fix CoreParserPropertiesQuickCheckSpec.hs
    file_path = "test/Test/Unit/CoreParserPropertiesQuickCheckSpec.hs"
    with open(file_path, 'r') as f:
        content = f.read()
    
    # Fix Located constructor
    content = re.sub(
        r"Located \\(SourcePos 0 0 0\) True undefined",
        "Located (SourcePos 0 0 0) True",
        content
    )
    
    # Fix property return type
    content = re.sub(
        r"Left _ -> property True",
        "Left _ -> property True",
        content
    )
    
    with open(file_path, 'w') as f:
        f.write(content)
    
    # Fix CoreSourceLocationPropertiesQuickCheckSpec.hs
    file_path = "test/Test/Unit/CoreSourceLocationPropertiesQuickCheckSpec.hs"
    with open(file_path, 'r') as f:
        content = f.read()
    
    # Fix located call
    content = re.sub(
        r"locatedValue located == pos",
        "locatedValue (Located (SourceSpan pos pos) value) == pos",
        content
    )
    
    with open(file_path, 'w') as f:
        f.write(content)
    
    print("All files have been fixed!")

if __name__ == "__main__":
    os.chdir("/home/runner/work/Typus/Typus")
    fix_all_files()