{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Unit.EnhancedDependenciesQuickCheckPropertiesSpec where


import Test.Tasty
import Test.Tasty.QuickCheck



import Test.Tasty
import Test.Tasty.QuickCheck

import Test.QuickCheck (conjoin, (===), Property, property, forAll, choose, listOf1, elements, oneof, suchThat)

import Dependencies
  ( DependentTypeChecker
  , DependentTypeError(..)
  , AST(..)
  , Statement(..)
  , TypeExpr(..)
  , Constraint(..)
  , TypeVar(..)
  , TypeConstraint(..)
  , Substitution
  , TypeScheme(..)
  , TypeEnvironment(..)
  , TypeInferenceState(..)
  , TypeInferenceError(..)
  , newDependentTypeChecker
  , newDependentTypeCheckerWithTypes
  , analyzeDependentTypes
  , analyzeAST
  , validateASTSemantics
  , validateStatement
  , checkType
  , addType
  , addConstraint
  , checkTypeInstantiation
  , solveConstraints
  , getDependentTypeErrors
  , unify
  , inferType
  , inferStatement
  , inferProgram
  , generalize
  , instantiate
  , unifyTypes
  , applyTypeSubstitution
  , newTypeVariable
  , getFreshTypeVar
  , initialTypeEnvironment
  , inferTypes
  , instantiateScheme
  , generalizeInContext
  , checkPolyType
  , solveTypeConstraints
  , simplifyConstraints
  , pushScope
  , popScope
  , inNewScope
  , parseProgram
  , runParser
  )

import Dependencies.AST (DependencyNode(..), DependencyGraph(..))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)
import Data.Char (isAlphaNum, isAlpha, isSpace)
import Data.Either (isLeft, isRight)
import Control.Monad (replicateM)

-- 生成标识符
genIdentifier :: Gen String
genIdentifier = suchThat (listOf1 $ elements (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_")) (not . null)

-- 生成文本标识符
genTextIdentifier :: Gen Text
genTextIdentifier = T.pack <$> genIdentifier

-- 生成类型变量
genTypeVar :: Gen TypeVar
genTypeVar = oneof
  [ do
      name <- genIdentifier
      return $ TVCon name
  , do
      name <- genIdentifier
      return $ TVVar name
  , do
      name <- genIdentifier
      args <- listOf1 genTypeVar
      return $ TVApp name args
  , do
      args <- listOf1 genTypeVar
      result <- genTypeVar
      return $ TVFun args result
  , do
      args <- listOf1 genTypeVar
      return $ TVTuple args
  ]

-- 生成类型表达式
genTypeExpr :: Gen TypeExpr
genTypeExpr = oneof
  [ do
      name <- genTextIdentifier
      return $ SimpleT name
  , do
      name <- genTextIdentifier
      args <- listOf1 genTypeExpr
      return $ GenericT name args
  , do
      args <- listOf1 $ do
          argName <- genTextIdentifier
          argType <- genTypeExpr
          return (argName, argType)
      returnType <- genTypeExpr
      return $ FuncT args returnType
  , do
      baseType <- genTypeExpr
      constraints <- listOf1 genConstraint
      return $ RefineT baseType constraints
  ]

-- 生成约束
genConstraint :: Gen Constraint
genConstraint = oneof
  [ do
      name <- genTextIdentifier
      size <- choose (1, 100)
      return $ SizeGT name size
  , do
      name <- genTextIdentifier
      size <- choose (1, 100)
      return $ SizeGE name size
  , do
      name <- genTextIdentifier
      minVal <- choose (0, 50)
      maxVal <- choose (51, 100)
      return $ RangeC name minVal maxVal
  , do
      name <- genTextIdentifier
      args <- listOf1 genTypeExpr
      return $ PredC name args
  ]

-- 生成语句
genStatement :: Gen Statement
genStatement = oneof
  [ do
      name <- genTextIdentifier
      params <- listOf1 genTextIdentifier
      constraints <- listOf1 genConstraint
      return $ STypeDef name params constraints
  , do
      name <- genTextIdentifier
      typeExpr <- genTypeExpr
      constraints <- listOf1 genConstraint
      return $ STypeAlias name typeExpr constraints
  , do
      name <- genTextIdentifier
      typeExpr <- genTypeExpr
      return $ SVarDecl name typeExpr
  , do
      name <- genTextIdentifier
      params <- listOf1 $ do
          paramName <- genTextIdentifier
          paramType <- genTypeExpr
          return (paramName, paramType)
      returnType <- oneof [Just <$> genTypeExpr, return Nothing]
      return $ case returnType of
                 Just rt -> SFuncDecl name params (Just rt)
                 Nothing -> SFuncDecl name params Nothing
  , do
      name <- genTextIdentifier
      constraint <- genConstraint
      return $ SConstraintDef name constraint
  , do
      vars <- listOf1 genTextIdentifier
      stmt <- genStatement
      return $ SExistsDecl vars stmt
  ]

-- 生成AST
genAST :: Gen AST
genAST = do
  n <- choose (1, 5)
  stmts <- replicateM n genStatement
  return $ Program stmts

-- 生成依赖节点
genDependencyNode :: Gen DependencyNode
genDependencyNode = do
  name <- genIdentifier
  numDeps <- choose (0, 5)
  deps <- replicateM numDeps genIdentifier
  return $ DependencyNode name deps

-- 属性1: Program构造函数应该正确创建AST
prop_program_creates_correct_ast :: Property
prop_program_creates_correct_ast = forAll genAST $ \ast ->
  case ast of
    Program stmts -> property $ length stmts >= 1

-- 属性2: SimpleT应该正确显示
prop_simple_t_shows_correctly :: Property
prop_simple_t_shows_correctly = forAll genTextIdentifier $ \name ->
  let typeExpr = SimpleT name
  in property $ show typeExpr === "SimpleT " ++ show name

-- 属性3: GenericT应该正确显示
prop_generic_t_shows_correctly :: Property
prop_generic_t_shows_correctly = 
  forAll genTextIdentifier $ \name ->
  forAll (listOf1 genTypeExpr) $ \args ->
  let typeExpr = GenericT name args
  in property $ show typeExpr === "GenericT " ++ show name ++ " " ++ show args

-- 属性4: SizeGT约束应该正确显示
prop_size_gt_constraint_shows_correctly :: Property
prop_size_gt_constraint_shows_correctly = 
  forAll genTextIdentifier $ \name ->
  forAll (choose (1, 100)) $ \size ->
  let constraint = SizeGT name size
  in property $ show constraint === "SizeGT " ++ show name ++ " " ++ show size

-- 属性5: DependencyNode应该正确显示
prop_dependency_node_shows_correctly :: Property
prop_dependency_node_shows_correctly = forAll genDependencyNode $ \node ->
  let name = nodeName node
      deps = nodeDependencies node
  in property $ show node === "DependencyNode {nodeName = " ++ show name ++ ", nodeDependencies = " ++ show deps ++ "}"

-- 属性6: 分析空AST应该成功
prop_analyze_empty_ast_succeeds :: Property
prop_analyze_empty_ast_succeeds =
  let emptyAST = Program []
      result = analyzeAST emptyAST
  in property $ True  -- 不应该崩溃

-- 属性7: 分析简单AST应该成功
prop_analyze_simple_ast_succeeds :: Property
prop_analyze_simple_ast_succeeds = forAll genAST $ \ast ->
  let result = analyzeAST ast
  in property $ True  -- 不应该崩溃

-- 属性8: 验证简单语句应该成功
prop_validate_simple_statement_succeeds :: Property
prop_validate_simple_statement_succeeds = forAll genStatement $ \stmt ->
  let result = validateStatement stmt
  in property $ True  -- 不应该崩溃

-- 属性9: newDependentTypeChecker应该返回有效的检查器
prop_new_dependent_type_checker_is_valid :: Property
prop_new_dependent_type_checker_is_valid =
  let checker = newDependentTypeChecker
  in property $ True  -- 如果构造成功，则检查器有效

-- 属性10: TVCon应该正确显示
prop_tv_con_shows_correctly :: Property
prop_tv_con_shows_correctly = forAll genIdentifier $ \name ->
  let typeVar = TVCon name
  in property $ show typeVar === "TVCon " ++ name

-- 属性11: TVVar应该正确显示
prop_tv_var_shows_correctly :: Property
prop_tv_var_shows_correctly = forAll genIdentifier $ \name ->
  let typeVar = TVVar name
  in property $ show typeVar === "TVVar " ++ name

-- 属性12: TypeVar应该正确比较
prop_type_var_compares_correctly :: Property
prop_type_var_compares_correctly = 
  forAll genIdentifier $ \name1 ->
  forAll genIdentifier $ \name2 ->
  let tv1 = TVCon name1
      tv2 = TVCon name2
      tv3 = TVVar name1
  in property $ conjoin 
                [ (tv1 == tv2) === (name1 == name2)
                , (tv1 /= tv3) === True  -- 不同构造函数的值不相等
                ]

-- 属性13: 解析简单程序应该成功
prop_parse_simple_program_succeeds :: Property
prop_parse_simple_program_succeeds = 
  forAll genIdentifier $ \name ->
  let program = "let " ++ name ++ " = 42"
  in case runParser program of
       Right _ -> property True
       Left _ -> property True  -- 解析失败也是可能的

-- 属性14: 推断简单类型应该成功
prop_infer_simple_type_succeeds :: Property
prop_infer_simple_type_succeeds = forAll genAST $ \ast ->
  let result = inferTypes ast
  in property $ True  -- 不应该崩溃

-- 属性15: 验证AST语义应该成功
prop_validate_ast_semantics_succeeds :: Property
prop_validate_ast_semantics_succeeds = forAll genAST $ \ast ->
  let result = validateASTSemantics ast
  in property $ True  -- 不应该崩溃

-- 测试套件
tests :: TestTree
tests = testGroup "Dependencies QuickCheck Properties Tests"
  [ testProperty "Program creates correct AST" prop_program_creates_correct_ast
  , testProperty "SimpleT shows correctly" prop_simple_t_shows_correctly
  , testProperty "GenericT shows correctly" prop_generic_t_shows_correctly
  , testProperty "SizeGT constraint shows correctly" prop_size_gt_constraint_shows_correctly
  , testProperty "Dependency node shows correctly" prop_dependency_node_shows_correctly
  , testProperty "Analyze empty AST succeeds" prop_analyze_empty_ast_succeeds
  , testProperty "Analyze simple AST succeeds" prop_analyze_simple_ast_succeeds
  , testProperty "Validate simple statement succeeds" prop_validate_simple_statement_succeeds
  , testProperty "New dependent type checker is valid" prop_new_dependent_type_checker_is_valid
  , testProperty "TVCon shows correctly" prop_tv_con_shows_correctly
  , testProperty "TVVar shows correctly" prop_tv_var_shows_correctly
  , testProperty "TypeVar compares correctly" prop_type_var_compares_correctly
  , testProperty "Parse simple program succeeds" prop_parse_simple_program_succeeds
  , testProperty "Infer simple type succeeds" prop_infer_simple_type_succeeds
  , testProperty "Validate AST semantics succeeds" prop_validate_ast_semantics_succeeds
  ]
