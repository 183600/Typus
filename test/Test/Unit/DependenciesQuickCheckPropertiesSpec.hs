module Test.Unit.DependenciesQuickCheckPropertiesSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Dependencies.AST
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T

-- Arbitrary instance for Text
instance Arbitrary Text where
  arbitrary = T.pack <$> arbitrary

-- ============================================================================
-- Arbitrary Instances
-- ============================================================================

instance Arbitrary Statement where
  arbitrary = oneof
    [ STypeDef <$> arbitrary <*> arbitrary <*> arbitrary
    , STypeAlias <$> arbitrary <*> arbitrary <*> arbitrary
    , SVarDecl <$> arbitrary <*> arbitrary
    , SFuncDecl <$> arbitrary <*> arbitrary <*> arbitrary
    , SConstraintDef <$> arbitrary <*> arbitrary
    , SExistsDecl <$> arbitrary <*> arbitrary
    ]

instance Arbitrary TypeExpr where
  arbitrary = oneof
    [ SimpleT <$> arbitrary
    , GenericT <$> arbitrary <*> arbitrary
    , FuncT <$> arbitrary <*> arbitrary
    , RefineT <$> arbitrary <*> arbitrary
    ]

instance Arbitrary Constraint where
  arbitrary = oneof
    [ SizeGT <$> arbitrary <*> arbitrary
    , SizeGE <$> arbitrary <*> arbitrary
    , RangeC <$> arbitrary <*> arbitrary <*> arbitrary
    , PredC <$> arbitrary <*> arbitrary
    ]

-- DependencyNode instance is now defined in Dependencies.AST

instance Arbitrary DependencyGraph where
  arbitrary = DependencyGraph . Map.fromList <$> arbitrary

-- ============================================================================
-- AST Properties
-- ============================================================================

-- Property: Program constructor should preserve statement order
prop_program_preserves_order :: [Statement] -> [Statement] -> Property
prop_program_preserves_order stmts1 stmts2 = 
  let program1 = Program stmts1
      program2 = Program stmts2
  in property $ 
    if stmts1 == stmts2
    then program1 == program2
    else program1 /= program2

-- Property: Program should be equal if statements are equal
prop_program_equality :: [Statement] -> Property
prop_program_equality stmts = 
  let program1 = Program stmts
      program2 = Program stmts
  in property $ program1 == program2

-- ============================================================================
-- Statement Properties
-- ============================================================================

-- Property: STypeDef should preserve its components
prop_type_def_preserves_components :: Text -> [Text] -> [Constraint] -> Property
prop_type_def_preserves_components name params constraints = 
  let stmt = STypeDef name params constraints
  in property $ 
    case stmt of
      STypeDef n p c -> n == name && p == params && c == constraints
      _ -> False

-- Property: STypeAlias should preserve its components
prop_type_alias_preserves_components :: Text -> TypeExpr -> [Constraint] -> Property
prop_type_alias_preserves_components name typ constraints = 
  let stmt = STypeAlias name typ constraints
  in property $ 
    case stmt of
      STypeAlias n t c -> n == name && t == typ && c == constraints
      _ -> False

-- Property: SVarDecl should preserve its components
prop_var_decl_preserves_components :: Text -> TypeExpr -> Property
prop_var_decl_preserves_components name typ = 
  let stmt = SVarDecl name typ
  in property $ 
    case stmt of
      SVarDecl n t -> n == name && t == typ
      _ -> False

-- Property: SFuncDecl should preserve its components
prop_func_decl_preserves_components :: Text -> [(Text, TypeExpr)] -> Maybe TypeExpr -> Property
prop_func_decl_preserves_components name params retType = 
  let stmt = SFuncDecl name params retType
  in property $ 
    case stmt of
      SFuncDecl n p r -> n == name && p == params && r == retType
      _ -> False

-- Property: SConstraintDef should preserve its components
prop_constraint_def_preserves_components :: Text -> Constraint -> Property
prop_constraint_def_preserves_components name constraint = 
  let stmt = SConstraintDef name constraint
  in property $ 
    case stmt of
      SConstraintDef n c -> n == name && c == constraint
      _ -> False

-- Property: SExistsDecl should preserve its components
prop_exists_decl_preserves_components :: [Text] -> Statement -> Property
prop_exists_decl_preserves_components vars stmt = 
  let existsStmt = SExistsDecl vars stmt
  in property $ 
    case existsStmt of
      SExistsDecl v s -> v == vars && s == stmt
      _ -> False

-- ============================================================================
-- TypeExpr Properties
-- ============================================================================

-- Property: SimpleT should preserve its name
prop_simple_t_preserves_name :: Text -> Property
prop_simple_t_preserves_name name = 
  let typ = SimpleT name
  in property $ 
    case typ of
      SimpleT n -> n == name
      _ -> False

-- Property: GenericT should preserve its name and parameters
prop_generic_t_preserves_components :: Text -> [TypeExpr] -> Property
prop_generic_t_preserves_components name params = 
  let typ = GenericT name params
  in property $ 
    case typ of
      GenericT n p -> n == name && p == params
      _ -> False

-- Property: FuncT should preserve its parameters and return type
prop_func_t_preserves_components :: [(Text, TypeExpr)] -> TypeExpr -> Property
prop_func_t_preserves_components params retType = 
  let typ = FuncT params retType
  in property $ 
    case typ of
      FuncT p r -> p == params && r == retType
      _ -> False

-- Property: RefineT should preserve its type and constraints
prop_refine_t_preserves_components :: TypeExpr -> [Constraint] -> Property
prop_refine_t_preserves_components typ constraints = 
  let refined = RefineT typ constraints
  in property $ 
    case refined of
      RefineT t c -> t == typ && c == constraints
      _ -> False

-- ============================================================================
-- Constraint Properties
-- ============================================================================

-- Property: SizeGT should preserve its components
prop_size_gt_preserves_components :: Text -> Int -> Property
prop_size_gt_preserves_components var size = 
  let constraint = SizeGT var size
  in property $ 
    case constraint of
      SizeGT v s -> v == var && s == size
      _ -> False

-- Property: SizeGE should preserve its components
prop_size_ge_preserves_components :: Text -> Int -> Property
prop_size_ge_preserves_components var size = 
  let constraint = SizeGE var size
  in property $ 
    case constraint of
      SizeGE v s -> v == var && s == size
      _ -> False

-- Property: RangeC should preserve its components
prop_range_c_preserves_components :: Text -> Int -> Int -> Property
prop_range_c_preserves_components var min max = 
  let constraint = RangeC var min max
  in property $ 
    case constraint of
      RangeC v mn mx -> v == var && mn == min && mx == max
      _ -> False

-- Property: PredC should preserve its components
prop_pred_c_preserves_components :: Text -> [TypeExpr] -> Property
prop_pred_c_preserves_components name args = 
  let constraint = PredC name args
  in property $ 
    case constraint of
      PredC n a -> n == name && a == args
      _ -> False

-- ============================================================================
-- Dependency Node Properties
-- ============================================================================

-- Property: DependencyNode should preserve its components
prop_dependency_node_preserves_components :: String -> [String] -> Property
prop_dependency_node_preserves_components name deps = 
  let node = DependencyNode name deps
  in property $ 
    nodeName node == name && 
    nodeDependencies node == deps

-- Property: DependencyNode with same components should be equal
prop_dependency_node_equality :: String -> [String] -> Property
prop_dependency_node_equality name deps = 
  let node1 = DependencyNode name deps
      node2 = DependencyNode name deps
  in property $ node1 == node2

-- ============================================================================
-- Dependency Graph Properties
-- ============================================================================

-- Property: DependencyGraph should preserve its nodes
prop_dependency_graph_preserves_nodes :: [DependencyNode] -> Property
prop_dependency_graph_preserves_nodes nodes = 
  let nodeMap = Map.fromList [(nodeName n, n) | n <- nodes]
      graph = DependencyGraph nodeMap
  in property $ graphNodes graph == nodeMap

-- Property: DependencyGraph with same nodes should be equal
prop_dependency_graph_equality :: [DependencyNode] -> Property
prop_dependency_graph_equality nodes = 
  let nodeMap = Map.fromList [(nodeName n, n) | n <- nodes]
      graph1 = DependencyGraph nodeMap
      graph2 = DependencyGraph nodeMap
  in property $ graph1 == graph2

-- Property: Adding node to graph should increase node count
prop_dependency_graph_add_node :: DependencyGraph -> String -> [String] -> Property
prop_dependency_graph_add_node graph name deps = 
  let node = DependencyNode name deps
      nodeMap = graphNodes graph
      newMap = Map.insert name node nodeMap
      newGraph = DependencyGraph newMap
  in property $ 
    Map.size (graphNodes newGraph) >= Map.size (graphNodes graph)

-- ============================================================================
-- Complex AST Properties
-- ============================================================================

-- Property: Nested statements should preserve structure
prop_nested_statements_preserve_structure :: Text -> TypeExpr -> [Constraint] -> Property
prop_nested_statements_preserve_structure name typ constraints = 
  let typeDef = STypeDef name [T.pack "T"] constraints
      varDecl = SVarDecl name typ
      existsDecl = SExistsDecl [T.pack "T"] varDecl
      program = Program [typeDef, existsDecl]
  in property $ 
    case program of
      Program [STypeDef n p c, SExistsDecl v (SVarDecl vn t)] -> 
        n == name && p == [T.pack "T"] && c == constraints && 
        v == [T.pack "T"] && vn == name && t == typ
      _ -> False

-- Property: Complex type expressions should preserve structure
prop_complex_type_expressions_preserve_structure :: Text -> [Text] -> [TypeExpr] -> Property
prop_complex_type_expressions_preserve_structure name params typeArgs = 
  let simpleType = SimpleT name
      genericType = GenericT name typeArgs
      funcType = FuncT [(name, simpleType)] genericType
      refinedType = RefineT funcType [SizeGT name 0]
  in property $ 
    case refinedType of
      RefineT (FuncT [(n, SimpleT sn)] (GenericT gn args)) [SizeGT v s] -> 
        n == name && sn == name && gn == name && args == typeArgs && 
        v == name && s == 0
      _ -> False

-- Property: Complex constraints should preserve structure
prop_complex_constraints_preserve_structure :: Text -> [TypeExpr] -> Int -> Int -> Property
prop_complex_constraints_preserve_structure name args min max = 
  let sizeGT = SizeGT name min
      sizeGE = SizeGE name max
      range = RangeC name min max
      pred = PredC name args
      constraints = [sizeGT, sizeGE, range, pred]
  in property $ 
    length constraints == 4 &&
    case constraints of
      [SizeGT v1 s1, SizeGE v2 s2, RangeC v3 mn mx, PredC v4 a] -> 
        v1 == name && s1 == min &&
        v2 == name && s2 == max &&
        v3 == name && mn == min && mx == max &&
        v4 == name && a == args
      _ -> False

tests :: TestTree
tests = testGroup "Dependencies QuickCheck Properties Tests"
  [ testProperty "Program preserves order" prop_program_preserves_order
  , testProperty "Program equality" prop_program_equality
  , testProperty "STypeDef preserves components" prop_type_def_preserves_components
  , testProperty "STypeAlias preserves components" prop_type_alias_preserves_components
  , testProperty "SVarDecl preserves components" prop_var_decl_preserves_components
  , testProperty "SFuncDecl preserves components" prop_func_decl_preserves_components
  , testProperty "SConstraintDef preserves components" prop_constraint_def_preserves_components
  , testProperty "SExistsDecl preserves components" prop_exists_decl_preserves_components
  , testProperty "SimpleT preserves name" prop_simple_t_preserves_name
  , testProperty "GenericT preserves components" prop_generic_t_preserves_components
  , testProperty "FuncT preserves components" prop_func_t_preserves_components
  , testProperty "RefineT preserves components" prop_refine_t_preserves_components
  , testProperty "SizeGT preserves components" prop_size_gt_preserves_components
  , testProperty "SizeGE preserves components" prop_size_ge_preserves_components
  , testProperty "RangeC preserves components" prop_range_c_preserves_components
  , testProperty "PredC preserves components" prop_pred_c_preserves_components
  , testProperty "DependencyNode preserves components" prop_dependency_node_preserves_components
  , testProperty "DependencyNode equality" prop_dependency_node_equality
  , testProperty "DependencyGraph preserves nodes" prop_dependency_graph_preserves_nodes
  , testProperty "DependencyGraph equality" prop_dependency_graph_equality
  , testProperty "DependencyGraph add node" prop_dependency_graph_add_node
  , testProperty "Nested statements preserve structure" prop_nested_statements_preserve_structure
  , testProperty "Complex type expressions preserve structure" prop_complex_type_expressions_preserve_structure
  , testProperty "Complex constraints preserve structure" prop_complex_constraints_preserve_structure
  ]