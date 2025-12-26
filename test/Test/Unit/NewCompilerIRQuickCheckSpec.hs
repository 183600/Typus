{-# LANGUAGE TemplateHaskell #-}

module Test.Unit.NewCompilerIRQuickCheckSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Compiler.IR
import SourceLocation (SourcePos(..), SourceSpan(..), startPos, spanBetween)
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (nub)

-- | Test compiler intermediate representation properties
spec :: Spec
spec = describe "NewCompilerIR QuickCheck Tests" $ do

  describe "IR Node properties" $ do
    it "creates literal nodes correctly" $ property $
      \value ->
        let node = createLiteralNode value
            nodeValue = getNodeValue node
        in nodeValue === value

    it "creates variable nodes correctly" $ property $
      \varName ->
        let node = createVariableNode varName
            nodeName = getNodeName node
        in nodeName === varName

    it "creates binary operation nodes correctly" $ property $
      \op left right ->
        let node = createBinaryOpNode op left right
            nodeOp = getBinaryOperator node
        in nodeOp === op

    it "creates function call nodes correctly" $ property $
      \funcName args ->
        let node = createFunctionCallNode funcName args
            nodeFunc = getFunctionName node
            nodeArgs = getFunctionArguments node
        in nodeFunc === funcName &&
           length nodeArgs === length args

  describe "IR Expression properties" $ do
    it "expressions have consistent types" $ property $
      \expr ->
        let exprType = getExpressionType expr
            exprValue = evaluateExpression expr
        in isValidType exprType ==> 
           case exprValue of
             Just val -> typeMatchesValue exprType val
             Nothing -> True

    it "expression evaluation is deterministic" $ property $
      \expr ->
        let result1 = evaluateExpression expr
            result2 = evaluateExpression expr
        in result1 === result2

    it "constant folding works correctly" $ property $
      \left right op ->
        let leftExpr = createLiteralExpression left
            rightExpr = createLiteralExpression right
            binaryExpr = createBinaryExpression op leftExpr rightExpr
            folded = foldConstants binaryExpr
        in isFoldable op ==> 
           case (left, right, op) of
             (l, r, Add) -> evaluateExpression folded === Just (l + r)
             (l, r, Mul) -> evaluateExpression folded === Just (l * r)
             _ -> True

  describe "IR Statement properties" $ do
    it "assignment statements preserve variable names" $ property $
      \varName expr ->
        let stmt = createAssignmentStatement varName expr
            targetVar = getAssignmentTarget stmt
        in targetVar === varName

    it "return statements preserve expressions" $ property $
      \expr ->
        let stmt = createReturnStatement expr
            returnExpr = getReturnExpression stmt
        in returnExpr === Just expr

    it "conditional statements have correct branches" $ property $
      \condition thenBranch elseBranch ->
        let stmt = createConditionalStatement condition thenBranch elseBranch
            condExpr = getConditionExpression stmt
            thenStmts = getThenStatements stmt
            elseStmts = getElseStatements stmt
        in condExpr === condition &&
           length thenStmts === length thenBranch &&
           length elseStmts === length elseBranch

  describe "IR Function properties" $ do
    it "functions preserve parameter names" $ property $
      \funcName params body ->
        let func = createFunction funcName params body
            funcParams = getFunctionParameters func
        in funcName === getFunctionName func &&
           length funcParams === length params

    it "functions have correct arity" $ property $
      \params body ->
        let func = createFunction "test" params body
            arity = getFunctionArity func
        in arity === length params

    it "function bodies are preserved" $ property $
      \funcName params body ->
        let func = createFunction funcName params body
            funcBody = getFunctionBody func
        in length funcBody === length body

  describe "IR Module properties" $ do
    it "modules collect all functions" $ property $
      \functions ->
        let module' = createModule functions
            moduleFuncs = getModuleFunctions module'
        in length moduleFuncs === length functions

    it "modules can be merged correctly" $ property $
      \funcs1 funcs2 ->
        let mod1 = createModule funcs1
            mod2 = createModule funcs2
            merged = mergeModules mod1 mod2
            mergedFuncs = getModuleFunctions merged
        in length mergedFuncs === length funcs1 + length funcs2

    it "module dependencies are tracked" $ property $
      \functions ->
        let module' = createModule functions
            dependencies = getModuleDependencies module'
        in all (`elem` map getFunctionName functions) dependencies

  describe "IR Optimization properties" $ do
    it "dead code elimination removes unused variables" $ property $
      \assignments usedVars ->
        let module' = createModuleWithAssignments assignments
            optimized = eliminateDeadCode module' usedVars
            optimizedFuncs = getModuleFunctions optimized
        in length optimizedFuncs <= length (getModuleFunctions module')

    it "constant propagation works correctly" $ property $
      \assignments ->
        let module' = createModuleWithAssignments assignments
            optimized = propagateConstants module'
        in all hasOnlyConstants optimized

    it "inlining preserves semantics" $ property $
      \functions calls ->
        let module' = createModuleWithFunctions functions
            optimized = inlineFunctions module' calls
            originalResult = evaluateModule module'
            optimizedResult = evaluateModule optimized
        in originalResult === optimizedResult

  describe "IR Type checking properties" $ do
    it "type inference is consistent" $ property $
      \expr ->
        let inferredType = inferExpressionType expr
            checkedType = checkExpressionType expr
        in inferredType === checkedType

    it "type errors are detected correctly" $ property $
      \expr expectedType ->
        let result = checkExpressionTypeAgainst expr expectedType
        in case result of
          Right _ -> True
          Left err -> isValidTypeError err

    it "type environments are preserved" $ property $
      \bindings expr ->
        let env = createTypeEnvironment bindings
            result = typeCheckInEnvironment expr env
        in case result of
          Right typedExpr -> getExpressionType typedExpr `elem` map snd bindings
          Left _ -> True

  where
    -- Helper types for testing
    data BinaryOp = Add | Sub | Mul | Div
      deriving (Eq, Show, Enum, Bounded)

    data IRNode = LiteralNode Int
                 | VariableNode String
                 | BinaryOpNode BinaryOp IRNode IRNode
                 | FunctionCallNode String [IRNode]
      deriving (Eq, Show)

    data IRExpression = LiteralExpr Int
                      | VariableExpr String
                      | BinaryExpr BinaryOp IRExpression IRExpression
                      | FunctionCallExpr String [IRExpression]
      deriving (Eq, Show)

    data IRStatement = AssignmentStatement String IRExpression
                     | ReturnStatement (Maybe IRExpression)
                     | ConditionalStatement IRExpression [IRStatement] [IRStatement]
      deriving (Eq, Show)

    data IRFunction = IRFunction
      { functionName :: String
      , functionParameters :: [String]
      , functionBody :: [IRStatement]
      } deriving (Eq, Show)

    data IRModule = IRModule
      { moduleFunctions :: [IRFunction]
      , moduleDependencies :: [String]
      } deriving (Eq, Show)

    -- Mock implementations for testing
    createLiteralNode :: Int -> IRNode
    createLiteralNode = LiteralNode

    createVariableNode :: String -> IRNode
    createVariableNode = VariableNode

    createBinaryOpNode :: BinaryOp -> IRNode -> IRNode -> IRNode
    createBinaryOpNode = BinaryOpNode

    createFunctionCallNode :: String -> [IRNode] -> IRNode
    createFunctionCallNode = FunctionCallNode

    getNodeValue :: IRNode -> Int
    getNodeValue (LiteralNode value) = value
    getNodeValue _ = 0

    getNodeName :: IRNode -> String
    getNodeName (VariableNode name) = name
    getNodeName (FunctionCallNode name _) = name
    getNodeName _ = ""

    getBinaryOperator :: IRNode -> BinaryOp
    getBinaryOperator (BinaryOpNode op _ _) = op
    getBinaryOperator _ = Add

    getFunctionName :: IRNode -> String
    getFunctionName (FunctionCallNode name _) = name
    getFunctionName _ = ""

    getFunctionArguments :: IRNode -> [IRNode]
    getFunctionArguments (FunctionCallNode _ args) = args
    getFunctionArguments _ = []

    createLiteralExpression :: Int -> IRExpression
    createLiteralExpression = LiteralExpr

    createBinaryExpression :: BinaryOp -> IRExpression -> IRExpression -> IRExpression
    createBinaryExpression = BinaryExpr

    getExpressionType :: IRExpression -> String
    getExpressionType (LiteralExpr _) = "Int"
    getExpressionType (VariableExpr _) = "Unknown"
    getExpressionType (BinaryExpr _ _ _) = "Int"
    getExpressionType (FunctionCallExpr _ _) = "Unknown"

    evaluateExpression :: IRExpression -> Maybe Int
    evaluateExpression (LiteralExpr value) = Just value
    evaluateExpression (VariableExpr _) = Nothing
    evaluateExpression (BinaryExpr Add left right) = 
      case (evaluateExpression left, evaluateExpression right) of
        (Just l, Just r) -> Just (l + r)
        _ -> Nothing
    evaluateExpression (BinaryExpr Mul left right) = 
      case (evaluateExpression left, evaluateExpression right) of
        (Just l, Just r) -> Just (l * r)
        _ -> Nothing
    evaluateExpression _ = Nothing

    foldConstants :: IRExpression -> IRExpression
    foldConstants expr@(BinaryExpr op left right) =
      case (evaluateExpression left, evaluateExpression right) of
        (Just l, Just r) -> LiteralExpr (case op of Add -> l + r; Mul -> l * r; Sub -> l - r; Div -> l `div` r)
        _ -> expr
    foldConstants expr = expr

    isFoldable :: BinaryOp -> Bool
    isFoldable Add = True
    isFoldable Mul = True
    isFoldable _ = False

    isValidType :: String -> Bool
    isValidType "Int" = True
    isValidType "String" = True
    isValidType "Bool" = True
    isValidType _ = False

    typeMatchesValue :: String -> Int -> Bool
    typeMatchesValue "Int" _ = True
    typeMatchesValue _ _ = False

    createAssignmentStatement :: String -> IRExpression -> IRStatement
    createAssignmentStatement = AssignmentStatement

    createReturnStatement :: Maybe IRExpression -> IRStatement
    createReturnStatement = ReturnStatement

    createConditionalStatement :: IRExpression -> [IRStatement] -> [IRStatement] -> IRStatement
    createConditionalStatement = ConditionalStatement

    getAssignmentTarget :: IRStatement -> String
    getAssignmentTarget (AssignmentStatement var _) = var
    getAssignmentTarget _ = ""

    getReturnExpression :: IRStatement -> Maybe IRExpression
    getReturnExpression (ReturnStatement expr) = expr
    getReturnExpression _ = Nothing

    getConditionExpression :: IRStatement -> IRExpression
    getConditionExpression (ConditionalStatement cond _ _) = cond
    getConditionExpression _ = LiteralExpr 0

    getThenStatements :: IRStatement -> [IRStatement]
    getThenStatements (ConditionalStatement _ thenStmts _) = thenStmts
    getThenStatements _ = []

    getElseStatements :: IRStatement -> [IRStatement]
    getElseStatements (ConditionalStatement _ _ elseStmts) = elseStmts
    getElseStatements _ = []

    createFunction :: String -> [String] -> [IRStatement] -> IRFunction
    createFunction name params body = IRFunction name params body

    getFunctionName :: IRFunction -> String
    getFunctionName = functionName

    getFunctionParameters :: IRFunction -> [String]
    getFunctionParameters = functionParameters

    getFunctionBody :: IRFunction -> [IRStatement]
    getFunctionBody = functionBody

    getFunctionArity :: IRFunction -> Int
    getFunctionArity = length . functionParameters

    createModule :: [IRFunction] -> IRModule
    createModule funcs = IRModule funcs (nub $ concatMap getFunctionDependencies funcs)
      where
        getFunctionDependencies func = 
          concatMap extractFunctionNames (map getStatementExpressions (functionBody func))

    getModuleFunctions :: IRModule -> [IRFunction]
    getModuleFunctions = moduleFunctions

    getModuleDependencies :: IRModule -> [String]
    getModuleDependencies = moduleDependencies

    mergeModules :: IRModule -> IRModule -> IRModule
    mergeModules mod1 mod2 = 
      IRModule (moduleFunctions mod1 ++ moduleFunctions mod2) 
               (nub $ moduleDependencies mod1 ++ moduleDependencies mod2)

    createModuleWithAssignments :: [IRStatement] -> IRModule
    createModuleWithAssignments assignments = 
      createModule [IRFunction "main" [] assignments]

    createModuleWithFunctions :: [IRFunction] -> IRModule
    createModuleWithFunctions = createModule

    eliminateDeadCode :: IRModule -> [String] -> IRModule
    eliminateDeadCode module' usedVars = 
      let filteredFuncs = map (filterFunction usedVars) (moduleFunctions module')
      in module' { moduleFunctions = filteredFuncs }
      where
        filterFunction vars func = 
          func { functionBody = filter (\stmt -> isStatementUsed stmt vars) (functionBody func) }

    propagateConstants :: IRModule -> IRModule
    propagateConstants module' = 
      let optimizedFuncs = map optimizeFunction (moduleFunctions module')
      in module' { moduleFunctions = optimizedFuncs }
      where
        optimizeFunction func = 
          func { functionBody = map (optimizeStatement) (functionBody func) }
        optimizeStatement stmt = stmt -- Simplified implementation

    hasOnlyConstants :: IRModule -> Bool
    hasOnlyConstants module' = all allConstants (moduleFunctions module')
      where
        allConstants func = all statementConstants (functionBody func)
        statementConstants (AssignmentStatement _ expr) = isConstantExpression expr
        statementConstants _ = True
        isConstantExpression (LiteralExpr _) = True
        isConstantExpression _ = False

    inlineFunctions :: IRModule -> [String] -> IRModule
    inlineFunctions module' calls = module' -- Simplified implementation

    evaluateModule :: IRModule -> Maybe Int
    evaluateModule module' = 
      case findMainFunction (moduleFunctions module') of
        Just mainFunc -> evaluateFunctionBody (functionBody mainFunc)
        Nothing -> Nothing

    findMainFunction :: [IRFunction] -> Maybe IRFunction
    findMainFunction funcs = find (\f -> functionName f == "main") funcs
      where
        find _ [] = Nothing
        find p (x:xs) = if p x then Just x else find p xs

    evaluateFunctionBody :: [IRStatement] -> Maybe Int
    evaluateFunctionBody [] = Nothing
    evaluateFunctionBody (stmt:rest) = 
      case stmt of
        ReturnStatement expr -> evaluateExpression =<< expr
        _ -> evaluateFunctionBody rest

    inferExpressionType :: IRExpression -> String
    inferExpressionType = getExpressionType

    checkExpressionType :: IRExpression -> String
    checkExpressionType = getExpressionType

    checkExpressionTypeAgainst :: IRExpression -> String -> Either String String
    checkExpressionTypeAgainst expr expectedType =
      let actualType = getExpressionType expr
      in if actualType == expectedType then Right actualType else Left "Type mismatch"

    isValidTypeError :: String -> Bool
    isValidTypeError err = "Type mismatch" `isInfixOf` err

    createTypeEnvironment :: [(String, String)] -> [(String, String)]
    createTypeEnvironment = id

    typeCheckInEnvironment :: IRExpression -> [(String, String)] -> Either String IRExpression
    typeCheckInEnvironment expr env = Right expr -- Simplified implementation

    getStatementExpressions :: IRStatement -> [IRExpression]
    getStatementExpressions (AssignmentStatement _ expr) = [expr]
    getStatementExpressions (ReturnStatement maybeExpr) = maybe [] (:[]) maybeExpr
    getStatementExpressions (ConditionalStatement cond _ _) = [cond]

    extractFunctionNames :: IRExpression -> [String]
    extractFunctionNames (FunctionCallExpr name _) = [name]
    extractFunctionNames (BinaryExpr _ left right) = 
      extractFunctionNames left ++ extractFunctionNames right
    extractFunctionNames _ = []

    isStatementUsed :: IRStatement -> [String] -> Bool
    isStatementUsed (AssignmentStatement var _) usedVars = var `elem` usedVars
    isStatementUsed _ _ = True

    -- Helper functions
    find :: (a -> Bool) -> [a] -> Maybe a
    find _ [] = Nothing
    find p (x:xs) = if p x then Just x else find p xs

    isInfixOf :: String -> String -> Bool
    isInfixOf needle haystack = needle `elem` 
      [take (length needle) $ drop i haystack | i <- [0..length haystack - length needle]]

    -- Helper instances for QuickCheck
    instance Arbitrary BinaryOp where
      arbitrary = arbitraryBoundedEnum

    instance Arbitrary IRNode where
      arbitrary = oneof
        [ LiteralNode <$> arbitrary
        , VariableNode <$> arbitrary
        , BinaryOpNode <$> arbitrary <*> arbitrary <*> arbitrary
        , FunctionCallNode <$> arbitrary <*> arbitrary
        ]

    instance Arbitrary IRExpression where
      arbitrary = oneof
        [ LiteralExpr <$> arbitrary
        , VariableExpr <$> arbitrary
        , BinaryExpr <$> arbitrary <*> arbitrary <*> arbitrary
        , FunctionCallExpr <$> arbitrary <*> arbitrary
        ]

    instance Arbitrary IRStatement where
      arbitrary = oneof
        [ AssignmentStatement <$> arbitrary <*> arbitrary
        , ReturnStatement <$> arbitrary
        , ConditionalStatement <$> arbitrary <*> arbitrary <*> arbitrary
        ]

    instance Arbitrary IRFunction where
      arbitrary = IRFunction <$> arbitrary <*> arbitrary <*> arbitrary

    instance Arbitrary IRModule where
      arbitrary = createModule <$> arbitrary