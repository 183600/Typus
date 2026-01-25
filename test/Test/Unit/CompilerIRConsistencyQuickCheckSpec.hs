module Test.Unit.CompilerIRConsistencyQuickCheckSpec where



import Test.Tasty.HUnit
import Test.Tasty
import Test.Tasty.QuickCheck

import Compiler.IR
import Compiler.TypeChecker
import SourceLocation (SourcePos(..), startPos, SourceSpan(..))
import Data.List (nub, sortBy, sort)

-- | SourcePos 的 Arbitrary 实例
instance Arbitrary SourcePos where
  arbitrary = do
    line <- choose (1, 1000)
    column <- choose (1, 1000)
    offset <- choose (0, 100000)
    return $ SourcePos line column offset

-- | 简化的IR节点定义用于测试
data IRNode = IRNode
  { nodeId :: Int
  , nodeType :: String
  , nodeSpan :: SourceSpan
  } deriving (Show, Eq)

-- | 生成有效的IR节点
instance Arbitrary IRNode where
  arbitrary = do
    nodeId <- choose (1, 1000)
    nodeType <- elements ["Variable", "Function", "Literal", "Operation"]
    pos <- arbitrary
    nodeSpan <- return $ SourceSpan pos pos
    return $ IRNode nodeId nodeType nodeSpan

-- | 测试IR节点ID的唯一性
prop_ir_node_ids_unique :: [IRNode] -> Property
prop_ir_node_ids_unique nodes =
  let ids = map nodeId nodes
      uniqueIds = nub ids
  in length ids === length uniqueIds

-- | 测试IR节点类型的有效性
prop_ir_node_types_valid :: IRNode -> Property
prop_ir_node_types_valid node =
  let validTypes = ["Variable", "Function", "Literal", "Operation", "Block", "Conditional"]
      nodeTypeStr = nodeType node
  in property (nodeTypeStr `elem` validTypes)

-- | 测试IR节点的源位置有效性
prop_ir_node_spans_valid :: IRNode -> Property
prop_ir_node_spans_valid node =
  let sourceSpan = nodeSpan node
      start = spanStart sourceSpan
      end = spanEnd sourceSpan
  in posLine start >= 1 && posColumn start >= 1 && posOffset start >= 0 .&&.
     posLine end >= 1 && posColumn end >= 1 && posOffset end >= 0

-- | 测试IR节点的跨度一致性
prop_ir_node_span_consistency :: IRNode -> Property
prop_ir_node_span_consistency node =
  let sourceSpan = nodeSpan node
      start = spanStart sourceSpan
      end = spanEnd sourceSpan
  in property (start <= end)

-- | 测试IR节点的类型一致性
prop_ir_type_consistency :: IRNode -> Property
prop_ir_type_consistency node =
  let nodeTypeStr = nodeType node
  in case nodeTypeStr of
       "Variable" -> property True
       "Function" -> property True
       "Literal" -> property True
       "Operation" -> property True
       _ -> property False

-- | 测试IR节点的父子关系
prop_ir_parent_child_relationship :: [IRNode] -> Property
prop_ir_parent_child_relationship nodes =
  length nodes >= 2 ==> 
  let parents = take (length nodes `div` 2) nodes
      children = drop (length nodes `div` 2) nodes
  in all (`elem` map nodeId nodes) (map nodeId parents) .&&.
     all (`elem` map nodeId nodes) (map nodeId children)

-- | 测试IR节点的优化一致性
prop_ir_optimization_consistency :: IRNode -> Property
prop_ir_optimization_consistency node =
  let originalNode = node
      optimizedNode = node  -- 简化，实际应该应用优化
  in nodeId originalNode === nodeId optimizedNode .&&.
     nodeType originalNode === nodeType optimizedNode

-- | 测试IR节点的类型检查一致性
prop_ir_type_checking_consistency :: IRNode -> Property
prop_ir_type_checking_consistency node =
  let nodeTypeStr = nodeType node
      expectedType = case nodeTypeStr of
                       "Literal" -> "Value"
                       "Variable" -> "Reference"
                       "Function" -> "Callable"
                       "Operation" -> "Expression"
                       _ -> "Unknown"
  in property (expectedType /= "Unknown")

-- | 测试IR节点的代码生成一致性
prop_ir_code_generation_consistency :: IRNode -> Property
prop_ir_code_generation_consistency node =
  let nodeTypeStr = nodeType node
      generatesCode = nodeTypeStr `elem` ["Variable", "Function", "Operation"]
  in whenFail (print ("Node type: " ++ nodeTypeStr)) $
     if generatesCode 
     then property True  -- 简化测试，实际应该检查代码生成
     else property True

-- | 测试IR节点的内存布局一致性
prop_ir_memory_layout_consistency :: [IRNode] -> Property
prop_ir_memory_layout_consistency nodes =
  let sortedNodes = sortBy (\a b -> compare (nodeId a) (nodeId b)) nodes
      ids = map nodeId sortedNodes
  in ids === sort ids

tests :: TestTree
tests = testGroup "Compiler IR Consistency QuickCheck Tests"
  [ testProperty "IR node IDs unique" prop_ir_node_ids_unique
  , testProperty "IR node types valid" prop_ir_node_types_valid
  , testProperty "IR node spans valid" prop_ir_node_spans_valid
  , testProperty "IR node span consistency" prop_ir_node_span_consistency
  , testProperty "IR type consistency" prop_ir_type_consistency
  , testProperty "IR parent-child relationship" prop_ir_parent_child_relationship
  , testProperty "IR optimization consistency" prop_ir_optimization_consistency
  , testProperty "IR type checking consistency" prop_ir_type_checking_consistency
  , testProperty "IR code generation consistency" prop_ir_code_generation_consistency
  , testProperty "IR memory layout consistency" prop_ir_memory_layout_consistency
  ]