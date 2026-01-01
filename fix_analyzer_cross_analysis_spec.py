#!/usr/bin/env python3
import re

def fix_analyzer_cross_analysis_spec():
    file_path = "/home/runner/work/Typus/Typus/test/Test/Unit/AnalyzerCrossAnalysisSpec.hs"
    
    with open(file_path, 'r') as f:
        content = f.read()
    
    # Fix all case result of patterns
    content = re.sub(r'case result of:\s*\n\s*Left _ -> assertBool "Should not fail with exception" False\s*\n\s*Right \[\] -> assertBool "[^"]*" False\s*\n\s*Right Right \[([^\]]+)\] ->', 
                    r'case result of:\n    Left _ -> assertBool "Should not fail with exception" False\n    Right [] -> assertBool "Should detect errors" False\n    Right [\1] ->', content)
    
    content = re.sub(r'Right Right _ -> assertBool "[^"]*" False\s*\n\s*Right _ -> assertBool "[^"]*" False', 
                    r'Right _ -> assertBool "Should return specific error" False', content)
    
    # Fix the basic test case
    content = re.sub(r'result <- runExceptT \(evalStateT \(runCrossAnalysis code\) initialState\)\s*\n\s*assertBool "Cross analysis should complete" \(True\)', 
                    r'result <- runExceptT (evalStateT (runCrossAnalysis code) initialState)\n  case result of\n    Left _ -> assertBool "Should not fail with exception" False\n    Right _ -> assertBool "Cross analysis should complete" True', content)
    
    with open(file_path, 'w') as f:
        f.write(content)
    
    print("Fixed AnalyzerCrossAnalysisSpec.hs")

if __name__ == "__main__":
    fix_analyzer_cross_analysis_spec()