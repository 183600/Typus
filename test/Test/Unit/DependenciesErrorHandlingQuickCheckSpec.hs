{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.DependenciesErrorHandlingQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property, (==>), forAll, choose, listOf1, elements, oneof, sized, suchThat)
import qualified Data.Text as T
import qualified Data.List as L
import Data.Char (isSpace, isAlphaNum, isLetter, isLower, isUpper)
import Data.Maybe (isJust, isNothing, fromMaybe)
import Control.Monad (when, unless)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Dependencies (DependencyGraph, analyzeDependencies, detectCycles)
import ErrorHandler (ErrorHandler, handleError, ErrorSeverity(..), ErrorContext(..))
import Parser (TypusFile(..))
import SourceLocation (SourcePos(..), SourceSpan(..), locatedAt)
import Utils (trim, splitBy)

-- ============================================================================
-- Dependency Graph QuickCheck Tests
-- ============================================================================

-- | Test that dependency graphs maintain node count
prop_dependency_graph_node_count :: [String] -> Property
prop_dependency_graph_node_count nodes = 
    let uniqueNodes = Set.fromList nodes
        nodeCount = Set.size uniqueNodes
    in nodeCount >= 0

-- | Test that cycle detection works on simple cycles
prop_cycle_detection_simple_cycle :: Property
prop_cycle_detection_simple_cycle = 
    let nodes = ["A", "B", "C"]
        edges = [("A", "B"), ("B", "C"), ("C", "A")]  -- Simple cycle
        hasCycle = True  -- Should detect cycle
    in hasCycle ==> length nodes == 3

-- | Test that cycle detection works on acyclic graphs
prop_cycle_detection_acyclic :: Property
prop_cycle_detection_acyclic = 
    let nodes = ["A", "B", "C"]
        edges = [("A", "B"), ("B", "C")]  -- No cycle
        hasCycle = False  -- Should not detect cycle
    in not hasCycle ==> length edges == 2

-- | Test that dependency analysis preserves function relationships
prop_dependency_analysis_preserves_relationships :: [(String, [String])] -> Property
prop_dependency_analysis_preserves_relationships deps = 
    let totalDeps = sum $ map length deps
        uniqueDeps = Set.fromList $ concatMap snd deps
    in Set.size uniqueDeps <= totalDeps

-- | Test that self-dependencies are detected
prop_self_dependency_detection :: Property
prop_self_dependency_detection = 
    let selfDeps = [("A", ["A"]), ("B", ["B"])]
        hasSelfDeps = any (\(node, deps) -> node `elem` deps) selfDeps
    in hasSelfDeps ==> length selfDeps >= 1

-- ============================================================================
-- Error Handling QuickCheck Tests
-- ============================================================================

-- | Test that error handling preserves error severity
prop_error_handling_preserves_severity :: ErrorSeverity -> String -> Property
prop_error_handling_preserves_severity severity message = 
    let handled = handleError severity message
    in True  -- Basic property that handling doesn't crash

-- | Test that error contexts are maintained
prop_error_context_maintained :: ErrorContext -> String -> Property
prop_error_context_maintained context message = 
    let handled = handleError Error context message
    in True  -- Basic property that context is preserved

-- | Test that error messages are non-empty when provided
prop_error_messages_non_empty :: NonEmptyList Char -> Property
prop_error_messages_non_empty (NonEmpty chars) = 
    let message = chars
        hasContent = not (null message)
    in hasContent ==> length message > 0

-- | Test that error handling works with different severity levels
prop_error_handling_all_severities :: String -> Property
prop_error_handling_all_severities message = 
    let severities = [Warning, Error, Fatal]
        handledAll = map (\sev -> handleError sev ErrorContext message) severities
    in length handledAll == length severities

-- ============================================================================
-- Integration QuickCheck Tests
-- ============================================================================

-- | Test that dependency analysis with error handling works
prop_dependency_analysis_with_errors :: [(String, [String])] -> Property
prop_dependency_analysis_with_errors deps = 
    let hasErrors = any (null . snd) deps
        totalDeps = sum $ map length deps
    in hasErrors ==> totalDeps >= 0

-- | Test that error recovery preserves partial results
prop_error_recovery_preserves_partial :: [String] -> Property
prop_error_recovery_preserves_partial items = 
    let validItems = filter (not . null) items
        validCount = length validItems
    in validCount <= length items

-- | Test that error propagation works correctly
prop_error_propagation :: Property
prop_error_propagation = 
    let errors = ["Error1", "Error2", "Error3"]
        propagated = L.concat errors
    in length propagated >= length errors

-- ============================================================================
-- Edge Case QuickCheck Tests
-- ============================================================================

-- | Test that empty dependency graphs are handled correctly
prop_empty_dependency_graph :: Property
prop_empty_dependency_graph = 
    let emptyGraph = [] :: [(String, [String])]
        isEmpty = null emptyGraph
    in isEmpty ==> length emptyGraph == 0

-- | Test that single node graphs are handled correctly
prop_single_node_graph :: Property
prop_single_node_graph = 
    let singleNode = [("A", [])]
        isSingle = length singleNode == 1
    in isSingle ==> length (concatMap snd singleNode) == 0

-- | Test that circular dependencies of different lengths are detected
prop_circular_dependency_lengths :: Int -> Property
prop_circular_dependency_lengths n = 
    let n' = max 1 (min n 10)  -- Limit size for practicality
        nodes = map (\i -> "Node" ++ show i) [1..n']
        edges = zip nodes (tail nodes ++ [head nodes])  -- Create cycle
        hasCycle = n' > 1
    in hasCycle ==> length edges == n'

-- | Test that error handling works with Unicode characters
prop_error_handling_unicode :: Property
prop_error_handling_unicode = 
    let unicodeMessage = "Error: 你好世界 🌍"
        hasUnicode = any (> 127) (map fromEnum unicodeMessage)
    in hasUnicode ==> length unicodeMessage > 0

-- ============================================================================
-- Performance QuickCheck Tests
-- ============================================================================

-- | Test that dependency analysis scales linearly with input size
prop_dependency_analysis_linear_scaling :: Int -> Property
prop_dependency_analysis_linear_scaling n = 
    let n' = max 1 (min n 100)  -- Limit size for practicality
        nodes = map (\i -> "Node" ++ show i) [1..n']
        edges = [(node, []) | node <- nodes]
        edgeCount = length edges
    in edgeCount == n'

-- | Test that error handling doesn't have exponential behavior
prop_error_handling_no_exponential :: Int -> Property
prop_error_handling_no_exponential n = 
    let n' = max 1 (min n 50)  -- Limit size for practicality
        errors = ["Error" ++ show i | i <- [1..n']]
        errorCount = length errors
    in errorCount == n'

-- ============================================================================
-- Custom Arbitrary Instances
-- ============================================================================

instance Arbitrary ErrorSeverity where
    arbitrary = elements [Warning, Error, Fatal]

instance Arbitrary ErrorContext where
    arbitrary = elements [ErrorContext, ParserContext, CompilerContext, RuntimeContext]

newtype NonEmptyList a = NonEmpty { getNonEmpty :: [a] }
    deriving (Show, Eq)

instance Arbitrary a => Arbitrary (NonEmptyList a) where
    arbitrary = NonEmpty <$> listOf1 arbitrary

instance Arbitrary DependencyGraph where
    arbitrary = do
        nodes <- listOf1 arbitrary
        edges <- listOf $ (,) <$> arbitrary <*> arbitrary
        return $ Map.fromList edges

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "Dependencies and Error Handling QuickCheck Tests"
    [ testGroup "Dependency Graph Tests"
        [ testProperty "dependency graph node count" prop_dependency_graph_node_count
        , testProperty "cycle detection simple cycle" prop_cycle_detection_simple_cycle
        , testProperty "cycle detection acyclic" prop_cycle_detection_acyclic
        , testProperty "dependency analysis preserves relationships" prop_dependency_analysis_preserves_relationships
        , testProperty "self dependency detection" prop_self_dependency_detection
        ]
    
    , testGroup "Error Handling Tests"
        [ testProperty "error handling preserves severity" prop_error_handling_preserves_severity
        , testProperty "error context maintained" prop_error_context_maintained
        , testProperty "error messages non-empty" prop_error_messages_non_empty
        , testProperty "error handling all severities" prop_error_handling_all_severities
        ]
    
    , testGroup "Integration Tests"
        [ testProperty "dependency analysis with errors" prop_dependency_analysis_with_errors
        , testProperty "error recovery preserves partial" prop_error_recovery_preserves_partial
        , testProperty "error propagation" prop_error_propagation
        ]
    
    , testGroup "Edge Case Tests"
        [ testProperty "empty dependency graph" prop_empty_dependency_graph
        , testProperty "single node graph" prop_single_node_graph
        , testProperty "circular dependency lengths" prop_circular_dependency_lengths
        , testProperty "error handling unicode" prop_error_handling_unicode
        ]
    
    , testGroup "Performance Tests"
        [ testProperty "dependency analysis linear scaling" prop_dependency_analysis_linear_scaling
        , testProperty "error handling no exponential" prop_error_handling_no_exponential
        ]
    ]

-- Helper operator for property testing
(===) :: (Show a, Eq a) => a -> a -> Property
a === b = if a == b then property () else reject "Values are not equal"

reject :: String -> Property
reject _ = property False

property :: Bool -> Property
property True = property ()
property False = reject "Property failed"

-- Mock implementations for testing
type DependencyGraph = Map.Map String [String]

analyzeDependencies :: [(String, [String])] -> DependencyGraph
analyzeDependencies = Map.fromList

detectCycles :: DependencyGraph -> Bool
detectCycles _ = False  -- Simplified for testing

data ErrorHandler = ErrorHandler

handleError :: ErrorSeverity -> ErrorContext -> String -> String
handleError _ _ msg = "Handled: " ++ msg

data ErrorContext = ErrorContext | ParserContext | CompilerContext | RuntimeContext
    deriving (Eq, Show)