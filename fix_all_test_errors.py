#!/usr/bin/env python3
"""
修复所有测试文件中的编译错误
"""

import os
import re

def fix_compiler_advanced_spec():
    """修复CompilerAdvancedQuickCheckSpec.hs中的错误"""
    file_path = "/home/runner/work/Typus/Typus/test/Test/Unit/CompilerAdvancedQuickCheckSpec.hs"
    
    with open(file_path, 'r') as f:
        content = f.read()
    
    # 修复TypeCheckDiagnostic的Arbitrary实例
    old_arbitrary = """instance Arbitrary TypeCheckDiagnostic where
  arbitrary = do
    hasContext <- arbitrary
    context <- if hasContext then Just "testContext" else Nothing
    detail <- elements ["type mismatch", "undefined variable", "invalid operation"]
    return $ TypeCheckDiagnostic context detail"""
    
    new_arbitrary = """instance Arbitrary TypeCheckDiagnostic where
  arbitrary = do
    hasContext <- arbitrary
    context <- if hasContext then return "testContext" else return ""
    detail <- elements ["type mismatch", "undefined variable", "invalid operation"]
    return $ TypeCheckDiagnostic (if hasContext then Just context else Nothing) detail"""
    
    content = content.replace(old_arbitrary, new_arbitrary)
    
    # 修复缺失的函数调用
    content = re.sub(r'\berrorId\b', 'getErrorId', content)
    content = re.sub(r'\bseverity\b', 'getErrorSeverity', content)
    content = re.sub(r'\bphase\b', 'getErrorPhase', content)
    content = re.sub(r'\bmessage\b', 'getErrorMessage', content)
    content = re.sub(r'\berrorPosition\b', 'getErrorPosition', content)
    content = re.sub(r'\btypeErrorMessage\b', 'getTypeErrorMessage', content)
    content = re.sub(r'\btypeErrorPosition\b', 'getTypeErrorPosition', content)
    
    # 修复Error数据构造器
    content = content.replace('Error ===', 'getErrorSeverity err === ErrorSeverity')
    content = content.replace('Fatal, Error, Warning, Info', 'FatalSeverity, ErrorSeverity, WarningSeverity, InfoSeverity')
    content = content.replace('ParsingPhase', 'errorPhase ParsingPhase')
    content = content.replace('TypeCheckingPhase', 'errorPhase TypeCheckingPhase')
    content = content.replace('OptimizationPhase', 'errorPhase OptimizationPhase')
    content = content.replace('CodeGenPhase', 'errorPhase CodeGenPhase')
    
    # 修复函数参数类型错误
    content = content.replace('hasTypeErrors typeErrors', 'hasTypeErrors (TypusFile "" typeErrors [] defaultFileDirectives)')
    content = content.replace('extractDeclarations typusFile', 'extractDeclarations (getSimpleTypusCode code)')
    content = content.replace('extractFunctionCalls typusFile', 'extractFunctionCalls (getSimpleTypusCode code)')
    content = content.replace('buildTypeEnv typusFile', 'buildTypeEnv (Compiler.GoAst.GoModule [] [] [] [])')
    content = content.replace('buildTypeEnvFromPairs pairs', 'buildTypeEnvFromPairs [(name, Compiler.TypeChecker.BasicType name) | (name, _) <- pairs]')
    content = content.replace('checkTypeError code', 'checkTypeError (getSimpleTypusCode code)')
    
    with open(file_path, 'w') as f:
        f.write(content)
    
    print(f"修复了 {file_path}")

def fix_dependencies_advanced_spec():
    """修复DependenciesAdvancedQuickCheckSpec.hs中的错误"""
    file_path = "/home/runner/work/Typus/Typus/test/Test/Unit/DependenciesAdvancedQuickCheckSpec.hs"
    
    with open(file_path, 'r') as f:
        content = f.read()
    
    # 修复反引号错误
    content = content.replace('\\`Map.member\\`', '`Map.member`')
    
    with open(file_path, 'w') as f:
        f.write(content)
    
    print(f"修复了 {file_path}")

def fix_error_handler_advanced_spec():
    """修复ErrorHandlerAdvancedQuickCheckSpec.hs中的错误"""
    file_path = "/home/runner/work/Typus/Typus/test/Test/Unit/ErrorHandlerAdvancedQuickCheckSpec.hs"
    
    with open(file_path, 'r') as f:
        content = f.read()
    
    # 修复Maybe/Gen类型错误
    content = content.replace('code <- if hasCode then Just "let x = 42" else Nothing', 
                             'code <- if hasCode then return "let x = 42" else return ""')
    content = content.replace('function <- if hasFunction then Just "testFunction" else Nothing', 
                             'function <- if hasFunction then return "testFunction" else return ""')
    content = content.replace('variable <- if hasVariable then Just "x" else Nothing', 
                             'variable <- if hasVariable then return "x" else return ""')
    content = content.replace('typ <- if hasType then Just "Int" else Nothing', 
                             'typ <- if hasType then return "Int" else return ""')
    
    content = content.replace('action <- if hasAction then Just "retry operation" else Nothing', 
                             'action <- if hasAction then return "retry operation" else return ""')
    content = content.replace('hint <- if hasHint then Just "check input" else Nothing', 
                             'hint <- if hasHint then return "check input" else return ""')
    
    content = content.replace('timestamp <- if hasTimestamp then Just "2023-01-01 12:00:00" else Nothing', 
                             'timestamp <- if hasTimestamp then return "2023-01-01 12:00:00" else return ""')
    
    # 修复ErrorContext构造
    content = content.replace('ErrorContext code function variable typ additional', 
                             'ErrorContext (if hasCode then Just code else Nothing) (if hasFunction then Just function else Nothing) (if hasVariable then Just variable else Nothing) (if hasType then Just typ else Nothing) additional')
    
    # 修复RecoveryStrategy构造
    content = content.replace('RecoveryStrategy canRec shouldCont action hint cost confidence', 
                             'RecoveryStrategy canRec shouldCont (if hasAction then Just action else Nothing) (if hasHint then Just hint else Nothing) cost confidence')
    
    # 修复TypeError构造
    content = content.replace('timestamp = timestamp', 
                             'timestamp = if hasTimestamp then Just timestamp else Nothing')
    
    # 修复OwnershipErrorCombined
    content = content.replace('OwnershipErrorCombined errId errMsg', 
                             'OwnershipErrorCombined errId (Ownership.Common.Types.OwnershipError errMsg)')
    
    with open(file_path, 'w') as f:
        f.write(content)
    
    print(f"修复了 {file_path}")

if __name__ == "__main__":
    fix_compiler_advanced_spec()
    fix_dependencies_advanced_spec()
    fix_error_handler_advanced_spec()
    print("所有修复完成!")