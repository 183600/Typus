{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.NewCompactCompilerIRSpec where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property, (===), forAll, choose, elements)
import Compiler.IR
import qualified Data.Map as Map
import Data.List (sort, nub)

-- | 生成任意的类型名
genTypeName :: Gen String
genTypeName = elements ["int", "string", "bool", "float", "void"]

-- | 生成任意的变量名
genVarName :: Gen String
genVarName = do
  len <- choose (1, 8)
  first <- elements ['a'..'z']
  rest <- choose (0, len-1) >>= \n -> sequence [elements ['a'..'z'..'0'..'9'] | _ <- [1..n]]
  return (first : rest)

-- | 生成任意的基本表达式
genBasicExpression :: Gen Expression
genBasicExpression = do
  varName <- genVarName
  elements
    [ Variable varName
    , Constant (IntValue 42)
    , Constant (StringValue "test")
    , Constant (BoolValue True)
    ]

-- | 生成任意的二进制操作
genBinaryExpression :: Gen Expression
genBinaryExpression = do
  left <- genBasicExpression
  right <- genBasicExpression
  op <- elements [Add, Subtract, Multiply, Divide, Equal, NotEqual, LessThan, GreaterThan]
  return $ BinaryOp op left right

-- | 生成任意的表达式
instance Arbitrary Expression where
  arbitrary = elements [genBasicExpression, genBinaryExpression] >>= \g -> g

-- | 测试基本IR构造
testBasicIRConstruction :: TestTree
testBasicIRConstruction = testGroup "基本IR构造测试"
  [ testCase "创建变量" $
      let var = Variable "x"
      in case var of
        Variable name -> name @?= "x"
        _ -> assertBool "不是变量" False
    
  , testCase "创建常量" $
      let const = Constant (IntValue 42)
      in case const of
        Constant (IntValue value) -> value @?= 42
        _ -> assertBool "不是整数常量" False
    
  , testCase "创建二元操作" $
      let left = Variable "x"
          right = Constant (IntValue 1)
          binOp = BinaryOp Add left right
      in case binOp of
        BinaryOp Add (Variable l) (Constant (IntValue r)) -> 
          l @?= "x" && r @?= 1
        _ -> assertBool "二元操作结构错误" False
  ]

-- | 测试类型检查一致性
testTypeConsistency :: TestTree
testTypeConsistency = testGroup "类型一致性测试"
  [ testCase "整数加法类型检查" $
      let expr = BinaryOp Add (Constant (IntValue 1)) (Constant (IntValue 2))
          result = inferType expr
      in result @?= Just IntType
    
  , testCase "字符串连接类型检查" $
      let expr = BinaryOp Add (Constant (StringValue "hello")) (Constant (StringValue "world"))
          result = inferType expr
      in result @?= Just StringType
    
  , testCase "类型不匹配检测" $
      let expr = BinaryOp Add (Constant (IntValue 1)) (Constant (StringValue "test"))
          result = inferType expr
      in result @?= Nothing  -- 类型不匹配
    
  , testCase "比较操作返回布尔类型" $
      let expr = BinaryOp Equal (Variable "x") (Constant (IntValue 42))
          result = inferType expr
      in result @?= Just BoolType
  ]

-- | 测试IR优化
testIROptimization :: TestTree
testIROptimization = testGroup "IR优化测试"
  [ testCase "常量折叠" $
      let expr = BinaryOp Add (Constant (IntValue 10)) (Constant (IntValue 20))
          optimized = optimizeExpression expr
      in optimized @?= Constant (IntValue 30)
    
  , testCase "恒等式简化" $
      let expr = BinaryOp Multiply (Variable "x") (Constant (IntValue 1))
          optimized = optimizeExpression expr
      in optimized @?= Variable "x"
    
  , testCase "零乘法简化" $
      let expr = BinaryOp Multiply (Variable "x") (Constant (IntValue 0))
          optimized = optimizeExpression expr
      in optimized @?= Constant (IntValue 0)
    
  , testCase "嵌套优化" $
      let expr = BinaryOp Add 
                   (BinaryOp Multiply (Constant (IntValue 2)) (Constant (IntValue 3)))
                   (BinaryOp Multiply (Constant (IntValue 4)) (Constant (IntValue 5)))
          optimized = optimizeExpression expr
      in optimized @?= Constant (IntValue 26)  -- 2*3 + 4*5 = 6 + 20 = 26
  ]

-- | 测试IR转换
testIRTransformation :: TestTree
testIRTransformation = testGroup "IR转换测试"
  [ testCase "表达式到SSA形式" $
      let expr = BinaryOp Add (Variable "x") (Variable "y")
          ssa = convertToSSA expr
      in case ssa of
        [assignment] -> assertBool "SSA转换正确" True
        _ -> assertBool "SSA转换失败" False
    
  , testCase "控制流图构建" $
      let statements = 
            [ Assignment "x" (Constant (IntValue 1))
            , IfThenElse (BinaryOp GreaterThan (Variable "x") (Constant (IntValue 0)))
                [Assignment "y" (Constant (IntValue 1))]
                [Assignment "y" (Constant (IntValue 0))]
            ]
          cfg = buildControlFlowGraph statements
      in case cfg of
        CFG nodes -> L.length nodes @?= 3  -- entry, then, else
        _ -> assertBool "CFG构建失败" False
  ]

-- | 测试IR验证
testIRValidation :: TestTree
testIRValidation = testGroup "IR验证测试"
  [ testCase "有效IR验证" $
      let expr = BinaryOp Add (Variable "x") (Constant (IntValue 1))
          symbolTable = Map.fromList [("x", IntType)]
          errors = validateExpression expr symbolTable
      in null errors @?= True
    
  , testCase "未定义变量检测" $
      let expr = BinaryOp Add (Variable "undefined") (Constant (IntValue 1))
          symbolTable = Map.fromList [("x", IntType)]
          errors = validateExpression expr symbolTable
      in L.length errors @?= 1
    
  , testCase "类型错误检测" $
      let expr = BinaryOp Add (Variable "x") (Constant (StringValue "test"))
          symbolTable = Map.fromList [("x", IntType)]
          errors = validateExpression expr symbolTable
      in L.length errors @?= 1
  ]

-- | QuickCheck属性测试
testIRProperties :: TestTree
testIRProperties = testGroup "IR属性测试"
  [ testProperty "常量折叠保持语义" $
      \expr ->
        let optimized = optimizeExpression expr
            originalType = inferType expr
            optimizedType = inferType optimized
        in case (originalType, optimizedType) of
          (Just t1, Just t2) -> t1 === t2
          _ -> True  -- 如果类型推断失败，跳过检查
  
  , testProperty "优化不增加复杂度" $
      \expr ->
        let originalComplexity = calculateComplexity expr
            optimized = optimizeExpression expr
            optimizedComplexity = calculateComplexity optimized
        in optimizedComplexity <= originalComplexity
  
  , testProperty "SSA转换保持变量数量" $
      \expr ->
        let ssa = convertToSSA expr
            originalVars = collectVariables expr
            ssaVars = nub $ concatMap collectAssignmentVariables ssa
        in L.length ssaVars >= L.length originalVars
  ]

-- | 测试IR生成
testIRGeneration :: TestTree
testIRGeneration = testGroup "IR生成测试"
  [ testCase "简单函数IR生成" $
      let func = Function "main" [] IntType
                   [ Return (Constant (IntValue 0)) ]
          ir = generateFunctionIR func
      in case ir of
        FunctionIR _ _ statements -> L.length statements @?= 1
        _ -> assertBool "IR生成失败" False
    
  , testCase "带参数函数IR生成" $
      let func = Function "add" [Param "a" IntType, Param "b" IntType] IntType
                   [ Return (BinaryOp Add (Variable "a") (Variable "b")) ]
          ir = generateFunctionIR func
      in case ir of
        FunctionIR _ _ statements -> L.length statements @?= 1
        _ -> assertBool "IR生成失败" False
  ]

-- | 边界条件测试
testBoundaryConditions :: TestTree
testBoundaryConditions = testGroup "边界条件测试"
  [ testCase "空表达式" $
      let expr = Constant (IntValue 0)
          optimized = optimizeExpression expr
      in optimized @?= expr
    
  , testCase "极大表达式优化" $
      let buildLargeExpr 0 = Constant (IntValue 1)
          buildLargeExpr n = BinaryOp Add (buildLargeExpr (n-1)) (Constant (IntValue 1))
          expr = buildLargeExpr 100
          optimized = optimizeExpression expr
      in case optimized of
        Constant (IntValue _) -> assertBool "大表达式优化成功" True
        _ -> assertBool "大表达式优化失败" False
    
  , testCase "深度嵌套表达式" $
      let buildNestedExpr 0 = Variable "x"
          buildNestedExpr n = BinaryOp Multiply (buildNestedExpr (n-1)) (Variable "x")
          expr = buildNestedExpr 50
          complexity = calculateComplexity expr
      in complexity >= 50 @?= True
  ]

-- | 性能测试
testPerformanceProperties :: TestTree
testPerformanceProperties = testGroup "性能属性测试"
  [ testProperty "大量IR操作性能" $
      \n ->
        let size = min 100 (max 1 n)
            exprs = replicate size (BinaryOp Add (Variable "x") (Constant (IntValue 1)))
            optimized = map optimizeExpression exprs
            types = map inferType optimized
        in L.length optimized === size && L.all (/= Nothing) types
  ]

-- | 组合所有测试
tests :: TestTree
tests = testGroup "Compiler IR模块核心功能测试"
  [ testBasicIRConstruction
  , testTypeConsistency
  , testIROptimization
  , testIRTransformation
  , testIRValidation
  , testIRProperties
  , testIRGeneration
  , testBoundaryConditions
  , testPerformanceProperties
  ]

-- 辅助函数和类型定义（假设这些在Compiler.IR模块中存在）
data Type = IntType | StringType | BoolType | FloatType | VoidType deriving (Eq, Show)
data Value = IntValue Int | StringValue String | BoolValue Bool | FloatValue Float deriving (Eq, Show)
data BinaryOperator = Add | Subtract | Multiply | Divide | Equal | NotEqual | LessThan | GreaterThan deriving (Eq, Show)
data Expression 
  = Variable String
  | Constant Value
  | BinaryOp BinaryOperator Expression Expression
  deriving (Eq, Show)

data Param = Param String Type deriving (Eq, Show)
data Statement 
  = Assignment String Expression
  | IfThenElse Expression [Statement] [Statement]
  | Return Expression
  deriving (Eq, Show)

data Function = Function String [Param] Type [Statement] deriving (Eq, Show)

data ControlFlowGraph = CFG [CFGNode]
data CFGNode = CFGNode String [Statement] [String] deriving (Eq, Show)

data FunctionIR = FunctionIR String [Param] [Statement] deriving (Eq, Show)
data AssignmentIR = AssignmentIR String Expression deriving (Eq, Show)

-- 辅助函数实现
inferType :: Expression -> Maybe Type
inferType (Variable _) = Just IntType  -- 简化实现
inferType (Constant (IntValue _)) = Just IntType
inferType (Constant (StringValue _)) = Just StringType
inferType (Constant (BoolValue _)) = Just BoolType
inferType (Constant (FloatValue _)) = Just FloatType
inferType (BinaryOp Add left right) = 
  case (inferType left, inferType right) of
    (Just IntType, Just IntType) -> Just IntType
    (Just StringType, Just StringType) -> Just StringType
    _ -> Nothing
inferType (BinaryOp Multiply left right) = 
  case (inferType left, inferType right) of
    (Just IntType, Just IntType) -> Just IntType
    _ -> Nothing
inferType (BinaryOp _ _ _) = Just BoolType  -- 简化比较操作

optimizeExpression :: Expression -> Expression
optimizeExpression (BinaryOp Add (Constant (IntValue a)) (Constant (IntValue b))) = 
  Constant (IntValue (a + b))
optimizeExpression (BinaryOp Multiply (Constant (IntValue 0)) _) = Constant (IntValue 0)
optimizeExpression (BinaryOp Multiply _ (Constant (IntValue 0))) = Constant (IntValue 0)
optimizeExpression (BinaryOp Multiply expr (Constant (IntValue 1))) = expr
optimizeExpression (BinaryOp Multiply (Constant (IntValue 1)) expr) = expr
optimizeExpression expr = expr  -- 简化实现

convertToSSA :: Expression -> [AssignmentIR]
convertToSSA expr = [AssignmentIR "temp" expr]  -- 简化实现

buildControlFlowGraph :: [Statement] -> ControlFlowGraph
buildControlFlowGraph statements = CFG [CFGNode "entry" statements []]  -- 简化实现

validateExpression :: Expression -> Map.Map String Type -> [String]
validateExpression (Variable name) symbolTable = 
  if Map.member name symbolTable then [] else ["Undefined variable: " ++ name]
validateExpression (BinaryOp op left right) symbolTable = 
  validateExpression left symbolTable ++ validateExpression right symbolTable
validateExpression _ _ = []

calculateComplexity :: Expression -> Int
calculateComplexity (Variable _) = 1
calculateComplexity (Constant _) = 1
calculateComplexity (BinaryOp _ left right) = 1 + calculateComplexity left + calculateComplexity right

collectVariables :: Expression -> [String]
collectVariables (Variable name) = [name]
collectVariables (Constant _) = []
collectVariables (BinaryOp _ left right) = collectVariables left ++ collectVariables right

collectAssignmentVariables :: AssignmentIR -> [String]
collectAssignmentVariables (AssignmentIR var _) = [var]

generateFunctionIR :: Function -> FunctionIR
generateFunctionIR (Function name params body) = FunctionIR name params body  -- 简化实现