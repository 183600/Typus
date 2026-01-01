#!/usr/bin/env python3
import re

def fix_new_integrated_analyzer():
    file_path = "/home/runner/work/Typus/Typus/test/Test/Unit/AnalyzerCrossAnalysisSpec.hs"
    
    with open(file_path, 'r') as f:
        content = f.read()
    
    # Fix all newIntegratedAnalyzer calls
    content = re.sub(r'newIntegratedAnalyzer True True \{([^}]+)\}', 
                    r'(newIntegratedAnalyzer True True) {\1}', content)
    
    with open(file_path, 'w') as f:
        f.write(content)
    
    print("Fixed AnalyzerCrossAnalysisSpec.hs")

if __name__ == "__main__":
    fix_new_integrated_analyzer()