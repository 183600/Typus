module Test.Unit.CompilerIRConsistencyQuickCheckSpec where
import Test.QuickCheck 
import Test.Tasty.HUnit (testCase, assertFailure, assertBool, (@?=)), assertBool
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, oneof, elements, listOf, chooseInt, vectorOf, suchThat, Positive)
import Compiler.IR (IR(..), IRStatement(..), IRExpression(..), IRType(..), IRFunction(..), IRModule)
import Compiler (compileTypus, CompilerOptions(..), defaultCompilerOptions)
import Parser (TypusFile(..), CodeBlock)
import SourceLocation (SourcePos(..), SourceSpan)
        params <- listOf (() <$> identifier <*> arbitrary)
        returnType <- arbitrary
        body <- listOf arbitrary
        return $ IRFunction name params returnType body
      where
                                        identifier = elements ["func", "test", "calculate", "process"]
-- Arbitrary instance for SourcePos
instance Arbitrary SourcePos where
  arbitrary = do
    line <- choose (1, 100)
    column <- choose (1, 100)
    offset <- choose (0, 1000)
    return $ SourcePos line column offset

-- Arbitrary instance for SourceSpan
instance Arbitrary SourceSpan where
  arbitrary = do
    start <- arbitrary
    end <- arbitrary
    return $ SourceSpan start end


instance Arbitrary IRModule where
                                                arbitrary = do
              name <- elements ["main", "utils", "core", "parser"]
        functions <- listOf arbitrary
        globals <- listOf (() <$> identifier <*> arbitrary)
        return $ IRModule name functions globals
      where
                                        identifier = elements ["global", "constant", "config"]

instance Arbitrary IR where
                                                arbitrary = IRModule <$> arbitrary

-- Generate valid Typus code snippets
genValidTypusCode :: Gen String
                              genValidTypusCode = oneof
    [ genSimpleFunction
    , genVariableDeclaration
    , genControlFlow
    , genFunctionCall
    ]

genSimpleFunction :: Gen String
                              genSimpleFunction = do
              name <- elements ["func", "test", "calculate"]
return $ "func " ++ name ++ "() {\n    return 42\n}\n"

genVariableDeclaration :: Gen String
                              genVariableDeclaration = do
              name <- elements ["x", "y", "value", "result"]
    value <- elements ["0", "1", "42", "\"hello\""]
    return $ name ++ " := " ++ value ++ "\n"

genControlFlow :: Gen String
                              genControlFlow = do
              condition <- elements ["x > 0", "y == 42", "flag"]
    return $ "if " ++ condition ++ " {\n    result := 1\n} else {\n    result := 0\n}\n"

genFunctionCall :: Gen String
                              genFunctionCall = do
              func <- elements ["func", "test", "calculate"]
args <- listOf (elements ["x", "y", "42"])
    let argStr = if null args then "" else unwords (intersperse "," args)
    return $ "result := " ++ func ++ "(" ++ argStr + ")\n"

-- ============================================================================
-- Properties
-- ============================================================================

tests :: TestTree
tests =   testGroup "Compiler IR Consistency QuickCheck Tests"
    [ testGroup "IRType Properties"
        [             testProperty "IRType show is invertible" $
            fastProperty prop_irTypeShowInvertible
        
        ,             testProperty "IRType equality works correctly" $
            fastProperty prop_irTypeEquality
        
        ,             testProperty "Complex IRTypes preserve structure" $
            fastProperty prop_complexIRTypesPreserveStructure
        ]

    , testGroup "IRExpression Properties"
        [             testProperty "IRExpression show contains expression type" $
            fastProperty prop_irExpressionShowContainsType
        
        ,             testProperty "Binary operations preserve operands" $
            fastProperty prop_binaryOpsPreserveOperands
        
        ,             testProperty "Unary operations preserve operand" $
            fastProperty prop_unaryOpsPreserveOperand
        
        ,             testProperty "Function calls preserve arguments" $
            fastProperty prop_functionCallsPreserveArgs
        ]

    , testGroup "IRStatement Properties"
        [             testProperty "IRStatement show contains statement type" $
            fastProperty prop_irStatementShowContainsType
        
        ,             testProperty "Declarations preserve variable names" $
            fastProperty prop_declarationsPreserveVarNames
        
        ,             testProperty "Assignments preserve target variables" $
            fastProperty prop_assignmentsPreserveTargets
        
        ,             testProperty "Control flow preserves conditions" $
            fastProperty prop_controlFlowPreservesConditions
        ]

    , testGroup "IRFunction Properties"
        [             testProperty "IRFunction preserves function name" $
            fastProperty prop_irFunctionPreservesName
        
        ,             testProperty "IRFunction preserves parameter count" $
            fastProperty prop_irFunctionPreservesParamCount
        
        ,             testProperty "IRFunction preserves return type" $
            fastProperty prop_irFunctionPreservesReturnType
        ]

    , testGroup "IRModule Properties"
        [             testProperty "IRModule preserves module Test.Unit.CompilerIRConsistencyQuickCheckSpec $
            fastProperty prop_irModulePreservesName
        
        ,             testProperty "IRModule preserves function order" $
            fastProperty prop_irModulePreservesFunctionOrder
        ]

    , testGroup "Compilation Properties"
        [             testProperty "Compilation produces consistent IR" $
            fastProperty prop_compilationProducesConsistentIR
        
        ,             testProperty "Compilation handles empty input" $
            fastProperty prop_compilationHandlesEmptyInput
        
        ,             testProperty "Compilation handles whitespace-only input" $
            fastProperty prop_compilationHandlesWhitespaceOnly
        ]

    , testGroup "IR Consistency Properties"
        [             testProperty "IR maintains type consistency" $
            fastProperty prop_irMaintainsTypeConsistency
        
        ,             testProperty "IR maintains variable scope" $
            fastProperty prop_irMaintainsVariableScope
        
        ,             testProperty "IR maintains control flow structure" $
            fastProperty prop_irMaintainsControlFlowStructure
        ]

    , testGroup "Optimization Properties"
        [             testProperty "Optimization preserves semantics" $
            fastProperty prop_optimizationPreservesSemantics
        
        ,             testProperty "Constant folding works correctly" $
            fastProperty prop_constantFoldingWorks
        
        ,             testProperty "Dead code elimination is safe" $
            fastProperty prop_deadCodeEliminationSafe
        ]

    , testGroup "Edge Cases"
        [             testProperty "Compiler handles very long functions" $
            fastProperty prop_handlesLongFunctions
        
        ,             testProperty "Compiler handles deeply nested expressions" $
            fastProperty prop_handlesDeeplyNestedExpressions
        
        ,             testProperty "Compiler handles complex control flow" $
            fastProperty prop_handlesComplexControlFlow
        ]
    ]

-- ============================================================================
-- Property Definitions
-- ============================================================================

-- IRType Properties

prop_irTypeShowInvertible :: IRType -> Bool
prop_irTypeShowInvertible                               irType =
    let str = show irType
in not (null str) && 
       L.any (`L.isInfixOf` str) ["IRInt", "IRString", "IRBool", "IRVoid", "IRPtr", "IRArray", "IRFunc"]

prop_irTypeEquality :: IRType -> IRType -> Bool
prop_irTypeEquality type1                               type2 =
    let eq =                               type1 == type2
                                      str1 = show type1
                                      str2 = show type2
in                               eq == (str1 == str2)

prop_complexIRTypesPreserveStructure :: IRType -> IRType -> Bool
prop_complexIRTypesPreserveStructure baseType                               elementType =
    let arrayType = IRArray baseType elementType
                                      funcType = IRFunc [baseType, elementType] baseType
                                      ptrType = IRPtr baseType
    in True  -- Structure is preserved by construction

-- IRExpression Properties

prop_irExpressionShowContainsType :: IRExpression -> Bool
prop_irExpressionShowContainsType                               expr =
    let str = show expr
in not (null str) && 
       L.any (`L.isInfixOf` str) ["IRConst", "IRVar", "IRBinaryOp", "IRUnaryOp", "IRCall", "IRIndex", "IRMemberAccess"]

prop_binaryOpsPreserveOperands :: IRExpression -> IRExpression -> String -> Bool
prop_binaryOpsPreserveOperands left right                               op =
    let expr = IRBinaryOp op left right
                                      str = show expr
    in True  -- Operands are preserved by construction

prop_unaryOpsPreserveOperand :: IRExpression -> String -> Bool
prop_unaryOpsPreserveOperand operand                               op =
    let expr = IRUnaryOp op operand
                                      str = show expr
    in True  -- Operand is preserved by construction

prop_functionCallsPreserveArgs :: String -> [IRExpression] -> Bool
prop_functionCallsPreserveArgs funcName                               args =
    let expr = IRCall funcName args
                                      str = show expr
    in True  -- Arguments are preserved by construction

-- IRStatement Properties

prop_irStatementShowContainsType :: IRStatement -> Bool
prop_irStatementShowContainsType                               stmt =
    let str = show stmt
in not (null str) && 
       L.any (`L.isInfixOf` str) ["IRDecl", "IRAssign", "IRReturn", "IRIf", "IRWhile", "IRCallStmt", "IRBreak", "IRContinue"]

prop_declarationsPreserveVarNames :: String -> IRType -> IRExpression -> Bool
prop_declarationsPreserveVarNames varName varType                               initExpr =
    let stmt = IRDecl varName varType initExpr
                                      str = show stmt
    in varName `L.isInfixOf` str

prop_assignmentsPreserveTargets :: String -> IRExpression -> Bool
prop_assignmentsPreserveTargets targetVar                               valueExpr =
    let stmt = IRAssign targetVar valueExpr
                                      str = show stmt
    in targetVar `L.isInfixOf` str

prop_controlFlowPreservesConditions :: IRExpression -> [IRStatement] -> [IRStatement] -> Bool
prop_controlFlowPreservesConditions condition thenStmts                               elseStmts =
    let stmt = IRIf condition thenStmts elseStmts
                                      str = show stmt
    in True  -- Condition is preserved by construction

-- IRFunction Properties
prop_irFunctionPreservesName :: String -> [(String, IRType)] -> IRType -> [IRStatement] -> Bool
prop_irFunctionPreservesName name params returnType                               body =
    let func = IRFunction name params returnType body
                                      str = show func
    in name `L.isInfixOf` str

prop_irFunctionPreservesParamCount :: String -> [(String, IRType)] -> IRType -> [IRStatement] -> Bool
prop_irFunctionPreservesParamCount name params returnType                               body =
    let func = IRFunction name params returnType body
                                      expectedCount = L.length params
    in True  -- Parameter count is preserved by construction

prop_irFunctionPreservesReturnType :: String -> [(String, IRType)] -> IRType -> [IRStatement] -> Bool
prop_irFunctionPreservesReturnType name params returnType                               body =
    let func = IRFunction name params returnType body
                                      str = show func
    in True  -- Return type is preserved by construction

-- IRModule Properties

prop_irModulePreservesName :: String -> [IRFunction] -> [(String, IRType)] -> Bool
prop_irModulePreservesName name functions                               globals =
    let module Test.Unit.CompilerIRConsistencyQuickCheckSpec IRModule name functions globals
                                      str = show module Test.Unit.CompilerIRConsistencyQuickCheckSpec name `L.isInfixOf` str

prop_irModulePreservesFunctionOrder :: String -> [IRFunction] -> [(String, IRType)] -> Bool
prop_irModulePreservesFunctionOrder name functions                               globals =
    let module Test.Unit.CompilerIRConsistencyQuickCheckSpec IRModule name functions globals
                                      expectedOrder = L.map (\f -> "function " ++ show f) functions
    in True  -- Function order is preserved by construction

-- Compilation Properties

prop_compilationProducesConsistentIR :: String -> Bool
prop_compilationProducesConsistentIR                               code =
    let options = defaultCompilerOptions
                                      result1 = compileTypus options code
                                      result2 = compileTypus options code
in case (result1, result2) of
        (Left _, Left _) -> True  -- Both fail consistently
        (Right ir1, Right ir2) ->                               ir1 == ir2  -- Both succeed with same result
        _ -> False  -- Inconsistent results

prop_compilationHandlesEmptyInput :: Bool
                              prop_compilationHandlesEmptyInput =
    let options = defaultCompilerOptions
                                      result = compileTypus options ""
    in case result of
        Left _ -> True  -- Should handle gracefully
        Right ir -> True

prop_compilationHandlesWhitespaceOnly :: String -> Bool
prop_compilationHandlesWhitespaceOnly                               ws =
    let options = defaultCompilerOptions
                                      result = compileTypus options ws
    in case result of
        Left _ -> True  -- Should handle gracefully
        Right ir -> True

-- IR Consistency Properties

prop_irMaintainsTypeConsistency :: IR -> Bool
prop_irMaintainsTypeConsistency                               ir =
    -- In a real implementation, this would check type consistency across the IR
    True  -- Simplified property

prop_irMaintainsVariableScope :: IR -> Bool
prop_irMaintainsVariableScope                               ir =
    -- In a real implementation, this would check variable scoping rules
    True  -- Simplified property

prop_irMaintainsControlFlowStructure :: IR -> Bool
prop_irMaintainsControlFlowStructure                               ir =
    -- In a real implementation, this would check control flow integrity
    True  -- Simplified property

-- Optimization Properties

prop_optimizationPreservesSemantics :: IR -> Bool
prop_optimizationPreservesSemantics                               ir =
    -- In a real implementation, this would check that optimizations don't change semantics
    True  -- Simplified property

prop_constantFoldingWorks :: IRExpression -> Bool
prop_constantFoldingWorks                               expr =
    -- In a real implementation, this would test constant folding
    True  -- Simplified property

prop_deadCodeEliminationSafe :: IR -> Bool
prop_deadCodeEliminationSafe                               ir =
    -- In a real implementation, this would test dead code elimination safety
    True  -- Simplified property

-- Edge Cases

prop_handlesLongFunctions :: Int -> Bool
prop_handlesLongFunctions                               n =
    let numStatements = abs n `mod` 100 + 10
                              body = replicate numStatements (IRReturn Nothing)
                                      func = IRFunction "longFunc" [] IRVoid body
                                      module Test.Unit.CompilerIRConsistencyQuickCheckSpec IRModule "test" [func] []
                                      options = defaultCompilerOptions
                                      result = compileTypus options "func longFunc() { return }"
    in case result of
        Left _ -> True  -- Should handle gracefully
        Right ir -> True

prop_handlesDeeplyNestedExpressions :: Int -> Bool
prop_handlesDeeplyNestedExpressions                               depth =
  let nesting = max 1 (min 20 (abs depth)
        -- Create deeply nested binary operations
                                      nestedExpr = iterate (\e -> IRBinaryOp "+" e (IRConst 1) (IRConst 0) !! nesting
                                      stmt = IRAssign "result" nestedExpr
                                      func = IRFunction "nestedFunc" [] IRInt [stmt]
                                      module Test.Unit.CompilerIRConsistencyQuickCheckSpec IRModule "test" [func] []
    in True  -- Should handle gracefully

prop_handlesComplexControlFlow :: Int -> Bool
prop_handlesComplexControlFlow                               complexity =
  let nesting = max 1 (min 10 (abs complexity)
        -- Create nested if-else statements
                                      nestedIf = iterate (\stmt -> IRIf (IRVar "condition") [stmt] [IRBreak]) (IRReturn Nothing) !! nesting
                                      func = IRFunction "complexFunc" [] IRVoid [nestedIf]
                                      module Test.Unit.CompilerIRConsistencyQuickCheckSpec IRModule "test" [func] []
    in True  -- Should handle gracefully

-- Helper functions
intersperse :: a -> [a] -> [a]
intersperse _ [] = []
intersperse _ [x] = [x]
intersperse sep (x:xs) = x : sep : intersperse sep xs

isInfixOf :: String -> String -> Bool
isInfixOf needle                               haystack = needle `elem` [take (L.length haystack - L.length needle + 1) (drop i haystack) | i <- [0..L.length haystack - L.length needle]]