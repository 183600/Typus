#!/usr/bin/env python3
import re
import sys
import os

def fix_pattern_in_file(file_path, pattern, replacement):
    """Fix a pattern in a file"""
    try:
        with open(file_path, 'r') as f:
            content = f.read()
        
        if re.search(pattern, content):
            new_content = re.sub(pattern, replacement, content)
            with open(file_path, 'w') as f:
                f.write(new_content)
            print(f"Fixed {file_path}")
            return True
        return False
    except Exception as e:
        print(f"Error fixing {file_path}: {e}")
        return False

def main():
    # Common pattern fixes
    fixes = [
        # Fix evalStateT import
        (r'import Control\.Monad\.Trans \(evalStateT\)', r'import Control.Monad.Trans.State (evalStateT)'),
        
        # Fix isInfixOf
        (r'isInfixOf', r'L.isInfixOf'),
        
        # Fix isPrefixOf
        (r'isPrefixOf', r'L.isPrefixOf'),
        
        # Fix isSuffixOf
        (r'isSuffixOf', r'L.isSuffixOf'),
        
        # Fix null
        (r'null \$', r'L.null $'),
        (r'null \(', r'L.null ('),
        
        # Fix head
        (r'\bhead\b', r'L.head'),
        
        # Fix tail
        (r'\btail\b', r'L.tail'),
        
        # Fix length
        (r'\blength\b', r'L.length'),
        
        # Fix map
        (r'map\s+\(', r'L.map ('),
        
        # Fix filter
        (r'filter\s+\(', r'L.filter ('),
        
        # Fix foldl
        (r'foldl\s+\(', r'L.foldl ('),
        
        # Fix foldr
        (r'foldr\s+\(', r'L.foldr ('),
        
        # Fix concat
        (r'\bconcat\b', r'L.concat'),
        
        # Fix reverse
        (r'\breverse\b', r'L.reverse'),
        
        # Fix elem
        (r'elem\s+', r'L.elem '),
        
        # Fix notElem
        (r'notElem\s+', r'L.notElem '),
        
        # Fix and
        (r'\band\b', r'L.and'),
        
        # Fix or
        (r'\bor\b', r'L.or'),
        
        # Fix any
        (r'\bany\b', r'L.any'),
        
        # Fix all
        (r'\ball\b', r'L.all'),
        
        # Fix sum
        (r'\bsum\b', r'L.sum'),
        
        # Fix product
        (r'\bproduct\b', r'L.product'),
        
        # Fix maximum
        (r'\bmaximum\b', r'L.maximum'),
        
        # Fix minimum
        (r'\bminimum\b', r'L.minimum'),
    ]
    
    # Find all test files
    test_dir = "/home/runner/work/Typus/Typus/test/Test/Unit"
    for filename in os.listdir(test_dir):
        if filename.endswith(".hs"):
            file_path = os.path.join(test_dir, filename)
            for pattern, replacement in fixes:
                fix_pattern_in_file(file_path, pattern, replacement)

if __name__ == "__main__":
    main()