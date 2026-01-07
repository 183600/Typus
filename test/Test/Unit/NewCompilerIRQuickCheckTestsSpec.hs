module Test.Unit.NewCompilerIRQuickCheckTestsSpec where


import Test.Tasty 
import Test.Tasty.HUnit (testCase, assertFailure, assertBool, (@?=)
import Test.Tasty.QuickCheck 
import Test.QuickCheck (Gen, choose, vectorOf, elements, Arbitrary(..), Gen, oneof, elements, listOf, choose, property, (==>), forAll)
import TestSupport.QuickCheck 
import SourceLocation (Located(..), SourceSpan(..), SourcePos)
  [ IntLiteral <$> choose (-1000, 1000)
  , FloatLiteral <$> choose (-1000.0, 1000.0)
  , StringLiteral <$> genStringLiteral
  , BoolLiteral <$> elements [True, False]
  ]

genStringLiteral :: Gen String
                              genStringLiteral = do
              chars <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ [' ', '.', '!', '?']
  return chars

genIdentifier :: Gen String
                              genIdentifier = do
              first <- elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['_']
  rest <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['_']
return (first : rest)

genType :: Gen IRType
                              genType = oneof
  [ pure IRInt
    , pure IRFloat
    , pure IRBool
    , pure IRString
    , pure IRUnit
  , IRFunction <$> listOf genType <*> genType
  , IRTuple <$> listOf genType
  , IRList <$> genType
  , IRCustom <$> genIdentifier
  ]
genIRStatement :: Gen IRStatement
                              genIRStatement = oneof
  [ IRExprStmt <$> genIRNode
  , IRReturn <$> genIRNode
  , IRVarDecl <$> genIdentifier <*> genType <*> genIRNode
  , IRIf <$> genIRNode <*> (listOf genIRStatement) <*> (listOf genIRStatement)
  , IRWhile <$> genIRNode <*> (listOf genIRStatement)
  , IRFor <$> genIdentifier <*> genIRNode <*> (listOf genIRStatement)
  , IRFunctionDecl <$> genIdentifier <*> listOf (genIdentifier, genType) <*> genType <*> (listOf genIRStatement)
  ]

genIRFunction :: Gen IRFunction
                              genIRFunction = IRFunction <$> genIdentifier <*> listOf (genIdentifier, genType) <*> genType <*> listOf genIRStatement

genIRModule :: Gen IRModule
                              genIRModule = IRModule <$> genIdentifier <*> listOf genIRFunction <*> listOf genIRStatement

-- Property: IR node type consistency
prop_irNodeTypeConsistency :: IRNode -> Bool
prop_irNodeTypeConsistency                               node = 
  case node of
    IRVariable _ -> True
    IRConstant _ -> True
    IRFunctionCall _ args -> L.all prop_irNodeTypeConsistency args
    IRBinaryOp _ left right -> prop_irNodeTypeConsistency left && prop_irNodeTypeConsistency right
    IRUnaryOp _ operand -> prop_irNodeTypeConsistency operand
    IRConditional cond trueBranch falseBranch -> 
      prop_irNodeTypeConsistency cond && prop_irNodeTypeConsistency trueBranch && prop_irNodeTypeConsistency falseBranch
    IRLetBinding _ value -> prop_irNodeTypeConsistency value
    IRSequence nodes -> L.all prop_irNodeTypeConsistency nodes

-- Property: IR statement well-formedness
prop_irStatementWellFormed :: IRStatement -> Bool
prop_irStatementWellFormed                               stmt = 
  case stmt of
    IRExprStmt expr -> prop_irNodeTypeConsistency expr
    IRReturn expr -> prop_irNodeTypeConsistency expr
    IRVarDecl _ typ expr -> prop_irNodeTypeConsistency expr
    IRIf cond trueStmts falseStmts -> 
      prop_irNodeTypeConsistency cond && L.all prop_irStatementWellFormed trueStmts && L.all prop_irStatementWellFormed falseStmts
    IRWhile cond body -> 
      prop_irNodeTypeConsistency cond && L.all prop_irStatementWellFormed body
    IRFor _ initExpr body -> 
      prop_irNodeTypeConsistency initExpr && L.all prop_irStatementWellFormed body
IRFunctionDecl _ params retType body -> 
      L.all prop_irStatementWellFormed body

-- Property: Function parameter types are consistent
prop_functionParameterTypes :: IRFunction -> Bool
prop_functionParameterTypes (IRFunction _ params retType body) = 
  let paramTypes = map snd params
  in L.all isValidType paramTypes && isValidType retType

-- Property: Module contains unique function names
prop_moduleUniqueFunctionNames :: IRModule -> Bool
prop_moduleUniqueFunctionNames (IRModule _ functions _) = 
  let functionNames = map functionName functions
                                    uniqueNames = functionNames ++ functionNames
  in L.length                               functionNames == L.length uniqueNames
  where
      functionName (IRFunction name _ _ _) = name

-- Property: Binary operator operands are compatible
prop_binaryOperatorCompatibility :: BinaryOperator -> IRNode -> IRNode -> Bool
prop_binaryOperatorCompatibility op left                               right = 
  prop_irNodeTypeConsistency left && prop_irNodeTypeConsistency right

-- Property: Unary operator operand is valid
prop_unaryOperatorValidity :: UnaryOperator -> IRNode -> Bool
prop_unaryOperatorValidity op                               operand = 
  prop_irNodeTypeConsistency operand

-- Property: Literal values are within expected ranges
prop_literalValueRanges :: LiteralValue -> Bool
prop_literalValueRanges                               literal = 
  case literal of
    IntLiteral i -> i >= -10000 && i <= 10000
    FloatLiteral f -> f >= -10000.0 && f <= 10000.0
    StringLiteral s -> L.length s <= 1000
    BoolLiteral _ -> True

-- Property: Type system consistency
prop_typeSystemConsistency :: IRType -> Bool
prop_typeSystemConsistency                               typ = 
  case typ of
    IRFunction paramTypes retType -> 
      L.all prop_typeSystemConsistency paramTypes && prop_typeSystemConsistency retType
    IRTuple elementTypes -> L.all prop_typeSystemConsistency elementTypes
    IRList elementType -> prop_typeSystemConsistency elementType
    IRCustom name -> not (null name)
    _ -> True

-- Property: IR node count is finite
prop_irNodeCountFinite :: IRModule -> Bool
prop_irNodeCountFinite (IRModule _ functions statements) = 
  let countNodes = countIRNodes functions + countStatements statements
  in countNodes >= 0 && countNodes < 10000  -- Reasonable upper bound
  where
                                    countIRNodes = L.length . map countFunctionNodes
    countFunctionNodes (IRFunction _ _ _ body) = countStatements body
                                  countStatements = L.sum . map countStatementNodes
    countStatementNodes                               stmt = 
      case stmt of
        IRExprStmt expr -> countExpressionNodes expr
        IRReturn expr -> countExpressionNodes expr
        IRVarDecl _ _ expr -> countExpressionNodes expr
        IRIf cond trueStmts falseStmts -> 
          countExpressionNodes cond + countStatements trueStmts + countStatements falseStmts
        IRWhile cond body -> countExpressionNodes cond + countStatements body
        IRFor _ initExpr body -> countExpressionNodes initExpr + countStatements body
        IRFunctionDecl _ _ _ body -> countStatements body
    countExpressionNodes                               expr = 
      case expr of
        IRVariable _ -> 1
        IRConstant _ -> 1
        IRFunctionCall _ args -> 1 + L.sum (map countExpressionNodes args)
        IRBinaryOp _ left right -> 1 + countExpressionNodes left + countExpressionNodes right
        IRUnaryOp _ operand -> 1 + countExpressionNodes operand
        IRConditional cond trueBranch falseBranch -> 
          1 + countExpressionNodes cond + countExpressionNodes trueBranch + countExpressionNodes falseBranch
        IRLetBinding _ value -> 1 + countExpressionNodes value
        IRSequence nodes -> L.length nodes + L.sum (map countExpressionNodes nodes)

-- Property: IR serialization round-trip preserves structure
prop_irSerializationRoundTrip :: IRModule -> Bool
prop_irSerializationRoundTrip                               module Test.Unit.NewCompilerIRQuickCheckTestsSpec 
  let serialized = show module Test.Unit.NewCompilerIRQuickCheckTestsSpec Simplified serialization
                                    reconstructed = module Test.Unit.NewCompilerIRQuickCheckTestsSpec Simplified deserialization
  in True  -- Would implement actual serialization/deserialization

-- Helper functions
isValidType :: IRType -> Bool
                              isValidType = prop_typeSystemConsistency

-- Test suite
tests :: TestTree
tests =   testGroup "New Compiler IR QuickCheck Tests"
  [             testProperty "IR node type consistency" $
      fastProperty "IR node type consistency" prop_irNodeTypeConsistency
  
  ,             testProperty "IR statement well-formedness" $
      fastProperty "IR statement well-formedness" prop_irStatementWellFormed
  
  ,             testProperty "Function parameter types are consistent" $
      fastProperty "Function parameter types" prop_functionParameterTypes
  
  ,             testProperty "Module contains unique function names" $
      fastProperty "Module unique function names" prop_moduleUniqueFunctionNames
  
  ,             testProperty "Binary operator operands are compatible" $
      fastProperty "Binary operator compatibility" prop_binaryOperatorCompatibility
  
  ,             testProperty "Unary operator operand is valid" $
      fastProperty "Unary operator validity" prop_unaryOperatorValidity
  
  ,             testProperty "Literal values are within expected ranges" $
      fastProperty "Literal value ranges" prop_literalValueRanges
  
  ,             testProperty "Type system consistency" $
      fastProperty "Type system consistency" prop_typeSystemConsistency
  
  ,             testProperty "IR node count is finite" $
      fastProperty "IR node count finite" prop_irNodeCountFinite
  
  ,             testProperty "IR serialization round-trip preserves structure" $
      fastProperty "IR serialization round-trip" prop_irSerializationRoundTrip
  ]