module Test.Unit.NewCompactCompilerIRSpec where
import Test.QuickCheck 
import Test.Tasty.HUnit (testCase, assertFailure, assertBool, (@?=)), assertBool
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property, (===), forAll, choose, elements)
import Compiler.IR
import qualified Data.Map as Map
import Data.List 
              len <- choose (1, 8)
  first <- elements ['a'..'z']
  rest <- choose (0, len-1) >>= \n -> sequence [elements ['a'..'z'..'0'..'9'] | _ <- [1..n]]
  return (first : rest)
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


-- | 
genBasicExpression :: Gen Expression
                              genBasicExpression = do
              varName <- genVarName
  elements
    [ Variable varName
    , Constant (IntValue 42)
    , Constant (StringValue "test")
    , Constant (BoolValue True)
    ]

-- | 
genBinaryExpression :: Gen Expression
                              genBinaryExpression = do
              left <- genBasicExpression
  right <- genBasicExpression
  op <- elements [Add, Subtract, Multiply, Divide, Equal, NotEqual, LessThan, GreaterThan]
  return $ BinaryOp op left right

-- | 
instance Arbitrary Expression where
                                              arbitrary = elements [genBasicExpression, genBinaryExpression] >>= \g -> g

-- | IR
testBasicIRConstruction :: TestTree
testBasicIRConstruction = testGroup "IR"
  [             testCase "" $
      let var = Variable "x"
      in case var of
        Variable name -> name @?= "x"
        _ -> assertBool "" False
    
    ,             testCase "" $
      let const = Constant (IntValue 42)
      in case const of
        Constant (IntValue value) -> value @?= 42
        _ -> assertBool "" False
    
    ,             testCase "" $
      let left = Variable "x"
                                        right = Constant (IntValue 1)
                                        binOp = BinaryOp Add left right
      in case binOp of
        BinaryOp Add (Variable l) (Constant (IntValue r) -> 
          l @?= "x" && r @?= 1
        _ -> assertBool "" False
  ]

-- | 
testTypeConsistency :: TestTree
testTypeConsistency = testGroup ""
  [             testCase "" $
      let expr = BinaryOp Add (Constant (IntValue 1) (Constant (IntValue 2)
                                        result = inferType expr
      in result @?= Just IntType
    
    ,             testCase "" $
      let expr = BinaryOp Add (Constant (StringValue "hello") (Constant (StringValue "world")
                                        result = inferType expr
      in result @?= Just StringType
    
    ,             testCase "" $
      let expr = BinaryOp Add (Constant (IntValue 1) (Constant (StringValue "test")
                                        result = inferType expr
      in result @?= Nothing  -- 
    
    ,             testCase "" $
      let expr = BinaryOp Equal (Variable "x") (Constant (IntValue 42)
                                        result = inferType expr
      in result @?= Just BoolType
  ]

-- | IR
testIROptimization :: TestTree
testIROptimization = testGroup "IR"
  [             testCase "" $
      let expr = BinaryOp Add (Constant (IntValue 10) (Constant (IntValue 20)
                                        optimized = optimizeExpression expr
      in optimized @?= Constant (IntValue 30)
    
    ,             testCase "" $
      let expr = BinaryOp Multiply (Variable "x") (Constant (IntValue 1)
                                        optimized = optimizeExpression expr
      in optimized @?= Variable "x"
    
    ,             testCase "" $
      let expr = BinaryOp Multiply (Variable "x") (Constant (IntValue 0)
                                        optimized = optimizeExpression expr
      in optimized @?= Constant (IntValue 0)
    
    ,             testCase "" $
      let expr = BinaryOp Add 
                   (BinaryOp Multiply (Constant (IntValue 2) (Constant (IntValue 3))
                   (BinaryOp Multiply (Constant (IntValue 4) (Constant (IntValue 5))
                                        optimized = optimizeExpression expr
      in optimized @?= Constant (IntValue 26)  -- 2*3 + 4*5 = 6 +                               20 = 26
  ]

-- | IR
testIRTransformation :: TestTree
testIRTransformation = testGroup "IR"
  [             testCase "SSA" $
      let expr = BinaryOp Add (Variable "x") (Variable "y")
                                        ssa = convertToSSA expr
      in case ssa of
        [assignment] -> assertBool "SSA" True
        _ -> assertBool "SSA" False
    
    ,             testCase "" $
      let statements = 
            [ Assignment "x" (Constant (IntValue 1)
            , IfThenElse (BinaryOp GreaterThan (Variable "x") (Constant (IntValue 0))
                [Assignment "y" (Constant (IntValue 1)]
                [Assignment "y" (Constant (IntValue 0)]
            ]
                                        cfg = buildControlFlowGraph statements
      in case cfg of
        CFG nodes -> L.length nodes @?= 3  -- entry, then, else
        _ -> assertBool "CFG" False
  ]

-- | IR
testIRValidation :: TestTree
testIRValidation = testGroup "IR"
  [             testCase "IR" $
      let expr = BinaryOp Add (Variable "x") (Constant (IntValue 1)
                                        symbolTable = Map.fromList [("x", IntType)]
                                        errors = validateExpression expr symbolTable
      in null errors @?= True
    
    ,             testCase "" $
      let expr = BinaryOp Add (Variable "undefined") (Constant (IntValue 1)
                                        symbolTable = Map.fromList [("x", IntType)]
                                        errors = validateExpression expr symbolTable
      in L.length errors @?= 1
    
    ,             testCase "" $
      let expr = BinaryOp Add (Variable "x") (Constant (StringValue "test")
                                        symbolTable = Map.fromList [("x", IntType)]
                                        errors = validateExpression expr symbolTable
      in L.length errors @?= 1
  ]

-- | QuickCheck
testIRProperties :: TestTree
testIRProperties = testGroup "IR"
  [             testProperty "" $
      \expr ->
        let optimized = optimizeExpression expr
                                          originalType = inferType expr
                                          optimizedType = inferType optimized
        in case (originalType, optimizedType) of
          (Just t1, Just t2) ->                               t1 === t2
          _ -> True  -- 
  
  ,             testProperty "" $
      \expr ->
        let originalComplexity = calculateComplexity expr
                                          optimized = optimizeExpression expr
                                          optimizedComplexity = calculateComplexity optimized
        in optimizedComplexity <= originalComplexity
  
  ,             testProperty "SSA" $
      \expr ->
        let ssa = convertToSSA expr
                                          originalVars = collectVariables expr
                                          ssaVars = nub $ concatMap collectAssignmentVariables ssa
        in L.length ssaVars >= L.length originalVars
  ]

-- | IR
testIRGeneration :: TestTree
testIRGeneration = testGroup "IR"
  [             testCase "IR" $
      let func = Function "main" [] IntType
                   [ Return (Constant (IntValue 0) ]
                                        ir = generateFunctionIR func
      in case ir of
        FunctionIR _ _ statements -> L.length statements @?= 1
        _ -> assertBool "IR" False
    
    ,             testCase "IR" $
      let func = Function "add" [Param "a" IntType, Param "b" IntType] IntType
                   [ Return (BinaryOp Add (Variable "a") (Variable "b") ]
                                        ir = generateFunctionIR func
      in case ir of
        FunctionIR _ _ statements -> L.length statements @?= 1
        _ -> assertBool "IR" False
  ]

-- | 
testBoundaryConditions :: TestTree
testBoundaryConditions = testGroup ""
  [             testCase "" $
      let expr = Constant (IntValue 0)
                                        optimized = optimizeExpression expr
      in optimized @?= expr
    
    ,             testCase "" $
      let buildLargeExpr                               0 = Constant (IntValue 1)
          buildLargeExpr                               n = BinaryOp Add (buildLargeExpr (n-1) (Constant (IntValue 1)
                                        expr = buildLargeExpr 100
                                        optimized = optimizeExpression expr
      in case optimized of
        Constant (IntValue _) -> assertBool "" True
        _ -> assertBool "" False
    
    ,             testCase "" $
      let buildNestedExpr                               0 = Variable "x"
          buildNestedExpr                               n = BinaryOp Multiply (buildNestedExpr (n-1) (Variable "x")
                                        expr = buildNestedExpr 50
                                        complexity = calculateComplexity expr
      in complexity >= 50 @?= True
  ]

-- | 
testPerformanceProperties :: TestTree
testPerformanceProperties = testGroup ""
  [             testProperty "IR" $
      \n ->
        let size = min 100 (max 1 n)
                                          exprs = replicate size (BinaryOp Add (Variable "x") (Constant (IntValue 1))
                                          optimized = map optimizeExpression exprs
                                          types = map inferType optimized
        in L.length                               optimized === size && L.all (/= Nothing) types
  ]

-- | 
tests :: TestTree
tests =   testGroup "Compiler IR"
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

-- Compiler.IR
data                               Type = IntType | StringType | BoolType | FloatType | VoidType deriving (Eq, Show)
data                               Value = IntValue Int | StringValue String | BoolValue Bool | FloatValue Float deriving (Eq, Show)
data                               BinaryOperator = Add | Subtract | Multiply | Divide | Equal | NotEqual | LessThan | GreaterThan deriving (Eq, Show)
data                               Expression = Variable String
  | Constant Value
  | BinaryOp BinaryOperator Expression Expression
  deriving (Eq, Show)

data                               Param = Param String Type deriving (Eq, Show)
data                               Statement = Assignment String Expression
  | IfThenElse Expression [Statement] [Statement]
  | Return Expression
  deriving (Eq, Show)

data                               Function = Function String [Param] Type [Statement] deriving (Eq, Show)

data                               ControlFlowGraph = CFG [CFGNode]
data                               CFGNode = CFGNode String [Statement] [String] deriving (Eq, Show)

data                               FunctionIR = FunctionIR String [Param] [Statement] deriving (Eq, Show)
data                               AssignmentIR = AssignmentIR String Expression deriving (Eq, Show)

-- 
inferType :: Expression -> Maybe Type
inferType (Variable _) = Just IntType  -- 
inferType (Constant (IntValue _) = Just IntType
inferType (Constant (StringValue _) = Just StringType
inferType (Constant (BoolValue _) = Just BoolType
inferType (Constant (FloatValue _) = Just FloatType
inferType (BinaryOp Add left right) = 
  case (inferType left, inferType right) of
    (Just IntType, Just IntType) -> Just IntType
    (Just StringType, Just StringType) -> Just StringType
    _ -> Nothing
inferType (BinaryOp Multiply left right) = 
  case (inferType left, inferType right) of
    (Just IntType, Just IntType) -> Just IntType
    _ -> Nothing
inferType (BinaryOp _ _ _) = Just BoolType  -- 

optimizeExpression :: Expression -> Expression
optimizeExpression (BinaryOp Add (Constant (IntValue a) (Constant (IntValue b)) = 
  Constant (IntValue (a + b)
optimizeExpression (BinaryOp Multiply (Constant (IntValue 0) _) = Constant (IntValue 0)
optimizeExpression (BinaryOp Multiply _ (Constant (IntValue 0)) = Constant (IntValue 0)
optimizeExpression (BinaryOp Multiply expr (Constant (IntValue 1)) = expr
optimizeExpression (BinaryOp Multiply (Constant (IntValue 1) expr) = expr
optimizeExpression                               expr = expr  -- 

convertToSSA :: Expression -> [AssignmentIR]
convertToSSA                               expr = [AssignmentIR "temp" expr]  -- 

buildControlFlowGraph :: [Statement] -> ControlFlowGraph
buildControlFlowGraph                               statements = CFG [CFGNode "entry" statements []]  -- 
validateExpression :: Expression -> Map.Map String Type -> [String]
validateExpression (Variable name)                               symbolTable = 
  if Map.member name symbolTable then [] else ["Undefined variable: " ++ name]
validateExpression (BinaryOp op left right)                               symbolTable = 
  validateExpression left symbolTable ++ validateExpression right symbolTable
validateExpression _                               _ = []

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
generateFunctionIR (Function name params body) = FunctionIR name params body  -- 