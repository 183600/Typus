#!/usr/bin/env python3
import os

def fix_indentation(file_path):
    """Fix indentation issues in test files"""
    with open(file_path, 'r') as f:
        lines = f.readlines()
    
    # Fix specific indentation issues
    new_lines = []
    for line in lines:
        # Fix the specific line with wrong indentation
        if "processed <- mapM processPositions chunked" in line:
            # Ensure proper indentation (16 spaces)
            new_lines.append("                    processed <- mapM processPositions chunked\n")
        else:
            new_lines.append(line)
    
    with open(file_path, 'w') as f:
        f.writelines(new_lines)
    print(f"Fixed indentation in {file_path}")

def main():
    # Fix specific files
    files_to_fix = [
        "test/Test/Unit/CabalConcurrentParsingSpec.hs",
    ]
    
    for file_path in files_to_fix:
        if os.path.exists(file_path):
            fix_indentation(file_path)

if __name__ == "__main__":
    main()