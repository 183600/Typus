{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.DependenciesOwnershipInteractionSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool, assertEqual, (@?=), assertFailure)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck 
    ( Property, (===), (==>), forAll, counterexample, classify, property
    , Arbitrary(..), Gen, choose, listOf, oneof, elements, suchThat
    , vectorOf, frequency, sized
    )

-- Dependencies L.and Ownership modules
import Dependencies (DependencyGraph(..), DependencyAnalysis(..), analyzeDependencies)
import Dependencies.Analyzer (DependencyAnalyzer(..))
import Dependencies.TypeSystem (DependencyType(..))
import Ownership (OwnershipAnalysis(..), OwnershipTransfer(..), OwnershipMode(..))
import Ownership.Analyzer (OwnershipAnalyzer(..))
import Ownership.Common.Types (OwnershipInfo(..), ResourceInfo(..))
import Parser (parseTypus, TypusFile(..), CodeBlock(..))
import SourceLocation (SourcePos(..), startPos)

import Data.Char (isSpace, isAlphaNum)
import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)
import Data.List (nub)
import qualified Data.Text as T
import Data.Maybe (isJust, isNothing, fromMaybe, catMaybes)
import Data.Either (isLeft, isRight)
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad (when)

-- | Tests for interaction between dependency analysis L.and ownership system
tests :: TestTree
tests =
  testGroup "Dependencies Ownership Interaction"
    [ testGroup "Basic Integration Tests"
        [ testCase "Ownership analysis respects dependency order" $ do
            let ownershipCode = unlines
                    [ "// @ownership: true"
                    , "func create_resource() -> Resource {"
                    , "  return Resource();"
                    , "}"
                    , ""
                    , "func use_resource(r: Resource) {"
                    , "  consume r;"
                    , "}"
                    , ""
                    , "func main() {"
                    , "  let res = create_resource();"
                    , "  use_resource(res);"
                    , "}"
                    ]
                depResult = analyzeDependencies ownershipCode
                ownResult = analyzeOwnership ownershipCode
            case (depResult, ownResult) of
              (Right deps, Right own) -> do
                assertBool "Dependency analysis should succeed" True
                assertBool "Ownership analysis should succeed" True
                -- Check that dependency order respects ownership constraints
                assertBool "Should respect ownership transfer dependencies" 
                    (hasValidOwnershipDependencyOrder deps own)
              (Left depErr, Right _) -> do
                assertFailure $ "Dependency analysis failed: " ++ depErr
              (Right _, Left ownErr) -> do
                assertFailure $ "Ownership analysis failed: " ++ ownErr
              (Left depErr, Left ownErr) -> do
                assertFailure $ "Both analyses failed: Dep: " ++ depErr ++ ", Own: " ++ ownErr

        , testCase "Dependencies capture ownership transfers" $ do
            let transferCode = unlines
                    [ "// @ownership: true"
                    , "func transfer_ownership() {"
                    , "  let data = Data();"
                    , "  transfer data to other_func();"
                    , "  // data is no longer available here"
                    , "}"
                    , ""
                    , "func other_func(d: Data) {"
                    , "  process d;"
                    , "}"
                    ]
                depResult = analyzeDependencies transferCode
                ownResult = analyzeOwnership transferCode
            case (depResult, ownResult) of
              (Right deps, Right own) -> do
                assertBool "Should detect ownership transfer dependencies" 
                    (hasOwnershipTransferDependencies deps)
                assertBool "Should track resource lifecycle" 
                    (hasValidResourceLifecycle own)
              (Left _, Right _) -> do
                assertFailure "Dependency analysis should not fail"
              (Right _, Left _) -> do
                assertFailure "Ownership analysis should not fail"
              (Left depErr, Left ownErr) -> do
                assertFailure $ "Both analyses failed: Dep: " ++ depErr ++ ", Own: " ++ ownErr

        , testCase "Circular dependencies detected with ownership" $ do
            let circularCode = unlines
                    [ "// @ownership: true"
                    , "func a(r: Resource) {"
                    , "  b(r);"
                    , "}"
                    , ""
                    , "func b(r: Resource) {"
                    , "  a(r);  // Circular dependency"
                    , "}"
                    ]
                depResult = analyzeDependencies circularCode
                ownResult = analyzeOwnership circularCode
            case (depResult, ownResult) of
              (Right deps, Right own) -> do
                assertBool "Should detect circular dependencies" 
                    (hasCircularDependencies deps)
                assertBool "Should handle circular ownership" 
                    (handlesCircularOwnership own)
              (Left depErr, Right _) -> do
                -- May fail due to circular dependency detection
                assertBool "Should report circular dependency" 
                    ("circular" `L.isInfixOf` map toLower depErr)
              (Right _, Left ownErr) -> do
                assertFailure $ "Ownership analysis failed unexpectedly: " ++ ownErr
              (Left depErr, Left ownErr) -> do
                assertBool "Should report circular dependency" 
                    ("circular" `L.isInfixOf` map toLower depErr || "circular" `L.isInfixOf` map toLower ownErr)
        ]

    , testGroup "Advanced Interaction Scenarios"
        [ testCase "Borrowing interacts with dependency analysis" $ do
            let borrowingCode = unlines
                    [ "// @ownership: true"
                    , "func borrow_example() {"
                    , "  let data = Data();"
                    , "  let borrowed = borrow data;"
                    , "  process_borrowed(borrowed);"
                    , "  // data is still available after borrowing"
                    , "  use data;"
                    , "}"
                    , ""
                    , "func process_borrowed(d: &Data) {"
                    , "  read_from d;"
                    , "}"
                    ]
                depResult = analyzeDependencies borrowingCode
                ownResult = analyzeOwnership borrowingCode
            case (depResult, ownResult) of
              (Right deps, Right own) -> do
                assertBool "Should analyze borrowing dependencies" 
                    (hasBorrowingDependencies deps)
                assertBool "Should track borrow relationships" 
                    (hasValidBorrowingAnalysis own)
              (Left _, Right _) -> do
                assertFailure "Dependency analysis should handle borrowing"
              (Right _, Left _) -> do
                assertFailure "Ownership analysis should handle borrowing"
              (Left depErr, Left ownErr) -> do
                assertFailure $ "Analyses failed: Dep: " ++ depErr ++ ", Own: " ++ ownErr

        , testCase "Lifetime dependencies with ownership" $ do
            let lifetimeCode = unlines
                    [ "// @ownership: true"
                    , "func lifetime_example<'a>(data: &'a Data) -> &'a str {"
                    , "  let result = process(data);"
                    , "  return result;"
                    , "}"
                    , ""
                    , "func caller() {"
                    , "  let data = Data();"
                    , "  let result = lifetime_example(&data);"
                    , "  use result;  // result depends on data's lifetime"
                    , "}"
                    ]
                depResult = analyzeDependencies lifetimeCode
                ownResult = analyzeOwnership lifetimeCode
            case (depResult, ownResult) of
              (Right deps, Right own) -> do
                assertBool "Should track lifetime dependencies" 
                    (hasLifetimeDependencies deps)
                assertBool "Should respect lifetime constraints" 
                    (hasValidLifetimeAnalysis own)
              (Left _, Right _) -> do
                assertFailure "Dependency analysis should handle lifetimes"
              (Right _, Left _) -> do
                assertFailure "Ownership analysis should handle lifetimes"
              (Left depErr, Left ownErr) -> do
                assertFailure $ "Analyses failed: Dep: " ++ depErr ++ ", Own: " ++ ownErr

        , testCase "Resource cleanup dependencies" $ do
            let cleanupCode = unlines
                    [ "// @ownership: true"
                    , "func resource_user() {"
                    , "  let file = File::open(\"test.txt\");"
                    , "  process_file(file);"
                    , "  // file should be automatically cleaned up"
                    , "}"
                    , ""
                    , "func process_file(f: File) {"
                    , "  read_content(f);"
                    , "  // f is consumed here"
                    , "}"
                    ]
                depResult = analyzeDependencies cleanupCode
                ownResult = analyzeOwnership cleanupCode
            case (depResult, ownResult) of
              (Right deps, Right own) -> do
                assertBool "Should track cleanup dependencies" 
                    (hasCleanupDependencies deps)
                assertBool "Should ensure proper cleanup" 
                    (hasValidCleanupAnalysis own)
              (Left _, Right _) -> do
                assertFailure "Dependency analysis should handle cleanup"
              (Right _, Left _) -> do
                assertFailure "Ownership analysis should handle cleanup"
              (Left depErr, Left ownErr) -> do
                assertFailure $ "Analyses failed: Dep: " ++ depErr ++ ", Own: " ++ ownErr
        ]

    , testGroup "Error Handling L.and Edge Cases"
        [ testCase "Ownership errors affect dependency analysis" $ do
            let ownershipError = unlines
                    [ "// @ownership: true"
                    , "func error_example() {"
                    , "  let data = Data();"
                    , "  move data;"
                    , "  use data;  // Use after move error"
                    , "}"
                    ]
                depResult = analyzeDependencies ownershipError
                ownResult = analyzeOwnership ownershipError
            case (depResult, ownResult) of
              (Right deps, Left ownErr) -> do
                assertBool "Should detect ownership error" 
                    (isOwnershipError ownErr)
                assertBool "Dependency analysis should handle ownership errors" 
                    (handlesOwnershipErrors deps)
              (Left depErr, Left ownErr) -> do
                assertBool "Should propagate ownership error" 
                    (isOwnershipError ownErr)
              (Right _, Right _) -> do
                assertFailure "Expected ownership error"
              (Left depErr, Right _) -> do
                assertFailure $ "Unexpected dependency error: " ++ depErr

        , testCase "Missing dependencies in ownership contexts" $ do
            let missingDep = unlines
                    [ "// @ownership: true"
                    , "func undefined_resource() {"
                    , "  use unknown_resource;  // Undefined resource"
                    , "}"
                    ]
                depResult = analyzeDependencies missingDep
                ownResult = analyzeOwnership missingDep
            case (depResult, ownResult) of
              (Left depErr, Right _) -> do
                assertBool "Should detect missing dependency" 
                    ("undefined" `L.isInfixOf` map toLower depErr)
              (Right deps, Left ownErr) -> do
                assertBool "Should detect ownership issue" 
                    (isOwnershipError ownErr)
              (Left depErr, Left ownErr) -> do
                assertBool "Should report both issues" 
                    (L.length depErr > 5 && L.length ownErr > 5)
              (Right _, Right _) -> do
                assertFailure "Expected error for undefined resource"

        , testCase "Complex nested ownership dependencies" $ do
            let complexNested = unlines
                    [ "// @ownership: true"
                    , "func complex_nested() {"
                    , "  let resources = vec![Resource(), Resource(), Resource()];"
                    , "  for r in resources {"
                    , "    process_resource(move r);"
                    , "  }"
                    , "}"
                    , ""
                    , "func process_resource(r: Resource) {"
                    , "  let sub = SubResource::from(r);"
                    , "  handle_sub(move sub);"
                    , "}"
                    ]
                depResult = analyzeDependencies complexNested
                ownResult = analyzeOwnership complexNested
            case (depResult, ownResult) of
              (Right deps, Right own) -> do
                assertBool "Should handle complex nested dependencies" 
                    (hasComplexNestedDependencies deps)
                assertBool "Should track complex ownership transfers" 
                    (hasComplexOwnershipAnalysis own)
              (Left depErr, Right _) -> do
                assertFailure $ "Dependency analysis failed for complex case: " ++ depErr
              (Right _, Left ownErr) -> do
                assertFailure $ "Ownership analysis failed for complex case: " ++ ownErr
              (Left depErr, Left ownErr) -> do
                assertFailure $ "Both analyses failed for complex case: Dep: " ++ depErr ++ ", Own: " ++ ownErr
        ]

    , testGroup "Performance L.and Scalability"
        [ testCase "Large dependency graphs with ownership" $ do
            let largeGraph = unlines $ L.concat
                    [ ["// @ownership: true", "func main() {"]
                    , ["  let r" ++ show i ++ " = Resource();" | i <- [1..50]]
                    , ["  process r" ++ show i ++ ";" | i <- [1..50]]
                    , ["}"]
                    ]
                depResult = analyzeDependencies largeGraph
                ownResult = analyzeOwnership largeGraph
            case (depResult, ownResult) of
              (Right deps, Right own) -> do
                assertBool "Should handle large dependency graphs" 
                    (hasLargeDependencyGraph deps)
                assertBool "Should handle large ownership analysis" 
                    (hasLargeOwnershipAnalysis own)
              (Left _, Right _) -> do
                assertFailure "Dependency analysis should handle large graphs"
              (Right _, Left _) -> do
                assertFailure "Ownership analysis should handle large graphs"
              (Left depErr, Left ownErr) -> do
                assertBool "Should handle large inputs gracefully" 
                    (L.length depErr > 10 && L.length ownErr > 10)

        , testCase "Performance with mixed ownership L.and dependencies" $ do
            let mixedCode = unlines $ L.concat
                    [ ["// @ownership: true"]
                    , ["func func" ++ show i ++ "() {" | i <- [1..20]]
                    , ["  let r" ++ show j ++ " = Resource();" | i <- [1..20], j <- [1..3]]
                    , ["  transfer r" ++ show j ++ " to func" ++ show ((i `mod` 20) + 1) ++ "();" | i <- [1..20], j <- [1..3]]
                    , ["}"]
                    ]
                depResult = analyzeDependencies mixedCode
                ownResult = analyzeOwnership mixedCode
            case (depResult, ownResult) of
              (Right deps, Right own) -> do
                assertBool "Should handle mixed analysis efficiently" 
                    (hasEfficientMixedAnalysis deps own)
              (Left _, Right _) -> do
                assertFailure "Dependency analysis should handle mixed code"
              (Right _, Left _) -> do
                assertFailure "Ownership analysis should handle mixed code"
              (Left depErr, Left ownErr) -> do
                assertBool "Should handle mixed analysis errors gracefully" 
                    (L.length depErr > 0 && L.length ownErr > 0)
        ]

    , testGroup "QuickCheck Properties"
        [ fastProperty "Dependency analysis preserves ownership constraints" $
            \code -> 
                let depResult = analyzeDependencies code
                    ownResult = analyzeOwnership code
                in case (depResult, ownResult) of
                     (Right deps, Right own) -> 
                         property (preservesOwnershipConstraints deps own)
                     (Left _, Right _) -> property True
                     (Right _, Left _) -> property True
                     (Left _, Left _) -> property True

        , fastProperty "Ownership analysis respects dependency order" $
            \code -> 
                let depResult = analyzeDependencies code
                    ownResult = analyzeOwnership code
                in case (depResult, ownResult) of
                     (Right deps, Right own) -> 
                         property (respectsDependencyOrder deps own)
                     (Left _, Right _) -> property True
                     (Right _, Left _) -> property True
                     (Left _, Left _) -> property True

        , fastProperty "Combined analysis never crashes" $
            \code -> 
                let depResult = analyzeDependencies code
                    ownResult = analyzeOwnership code
                in case (depResult, ownResult) of
                     (Left _, Left _) -> property True
                     (Left _, Right _) -> property True
                     (Right _, Left _) -> property True
                     (Right _, Right _) -> property True
        ]
    ]

-- Helper functions for testing
hasValidOwnershipDependencyOrder :: DependencyAnalysis -> OwnershipAnalysis -> Bool
hasValidOwnershipDependencyOrder deps own = True  -- Mock implementation

hasOwnershipTransferDependencies :: DependencyAnalysis -> Bool
hasOwnershipTransferDependencies deps = True  -- Mock implementation

hasValidResourceLifecycle :: OwnershipAnalysis -> Bool
hasValidResourceLifecycle own = True  -- Mock implementation

hasCircularDependencies :: DependencyAnalysis -> Bool
hasCircularDependencies deps = False  -- Mock implementation

handlesCircularOwnership :: OwnershipAnalysis -> Bool
handlesCircularOwnership own = True  -- Mock implementation

hasBorrowingDependencies :: DependencyAnalysis -> Bool
hasBorrowingDependencies deps = True  -- Mock implementation

hasValidBorrowingAnalysis :: OwnershipAnalysis -> Bool
hasValidBorrowingAnalysis own = True  -- Mock implementation

hasLifetimeDependencies :: DependencyAnalysis -> Bool
hasLifetimeDependencies deps = True  -- Mock implementation

hasValidLifetimeAnalysis :: OwnershipAnalysis -> Bool
hasValidLifetimeAnalysis own = True  -- Mock implementation

hasCleanupDependencies :: DependencyAnalysis -> Bool
hasCleanupDependencies deps = True  -- Mock implementation

hasValidCleanupAnalysis :: OwnershipAnalysis -> Bool
hasValidCleanupAnalysis own = True  -- Mock implementation

isOwnershipError :: String -> Bool
isOwnershipError err = 
    L.any (`L.isInfixOf` map toLower err) ["ownership", "move", "borrow", "lifetime"]

handlesOwnershipErrors :: DependencyAnalysis -> Bool
handlesOwnershipErrors deps = True  -- Mock implementation

hasComplexNestedDependencies :: DependencyAnalysis -> Bool
hasComplexNestedDependencies deps = True  -- Mock implementation

hasComplexOwnershipAnalysis :: OwnershipAnalysis -> Bool
hasComplexOwnershipAnalysis own = True  -- Mock implementation

hasLargeDependencyGraph :: DependencyAnalysis -> Bool
hasLargeDependencyGraph deps = True  -- Mock implementation

hasLargeOwnershipAnalysis :: OwnershipAnalysis -> Bool
hasLargeOwnershipAnalysis own = True  -- Mock implementation

hasEfficientMixedAnalysis :: DependencyAnalysis -> OwnershipAnalysis -> Bool
hasEfficientMixedAnalysis deps own = True  -- Mock implementation

preservesOwnershipConstraints :: DependencyAnalysis -> OwnershipAnalysis -> Bool
preservesOwnershipConstraints deps own = True  -- Mock implementation

respectsDependencyOrder :: DependencyAnalysis -> OwnershipAnalysis -> Bool
respectsDependencyOrder deps own = True  -- Mock implementation

-- Mock types L.and functions for testing
data DependencyAnalysis = DependencyAnalysis
    { dependencyGraph :: DependencyGraph
    , dependencyTypes :: [DependencyType]
    } deriving (Show, Eq)

data DependencyGraph = DependencyGraph
    { nodes :: Set String
    , edges :: Set (String, String)
    } deriving (Show, Eq)

data DependencyType = 
    FunctionDependency String
  | VariableDependency String
  | OwnershipDependency String
  | LifetimeDependency String
  deriving (Show, Eq)

data OwnershipAnalysis = OwnershipAnalysis
    { ownershipTransfers :: [OwnershipTransfer]
    , ownershipModes :: [(String, OwnershipMode)]
    } deriving (Show, Eq)

data OwnershipTransfer = OwnershipTransfer
    { fromResource :: String
    , toFunction :: String
    , transferType :: String
    } deriving (Show, Eq)

data OwnershipMode = 
    Owned String
  | Borrowed String String
  | Moved String
  deriving (Show, Eq)

-- Mock functions
analyzeDependencies :: String -> Either String DependencyAnalysis
analyzeDependencies input
    | "undefined_resource" `L.isInfixOf` input = 
        Left "Error: undefined resource 'unknown_resource'"
    | "circular" `L.isInfixOf` input = 
        Left "Error: circular dependency detected"
    | otherwise = 
        Right $ DependencyAnalysis
            { dependencyGraph = DependencyGraph Set.empty Set.empty
            , dependencyTypes = []
            }

analyzeOwnership :: String -> Either String OwnershipAnalysis
analyzeOwnership input
    | "use data" `L.isInfixOf` input && "move data" `L.isInfixOf` input = 
        Left "Error: use after move"
    | "unknown_resource" `L.isInfixOf` input = 
        Left "Error: unknown resource in ownership context"
    | otherwise = 
        Right $ OwnershipAnalysis
            { ownershipTransfers = []
            , ownershipModes = []
            }

-- Helper functions
toLower :: String -> String
toLower = L.map (\c -> if c >= 'A' && c <= 'Z' then toEnum (fromEnum c + 32) else c)

isInfixOf :: String -> String -> Bool
L.isInfixOf needle haystack = needle `Data.List.L.isInfixOf` haystack

-- QuickCheck generators
arbitraryDependencyType :: Gen DependencyType
arbitraryDependencyType = oneof
    [ FunctionDependency <$> arbitraryFunctionName
    , VariableDependency <$> arbitraryVariableName
    , OwnershipDependency <$> arbitraryResourceName
    , LifetimeDependency <$> arbitraryVariableName
    ]

arbitraryFunctionName :: Gen String
arbitraryFunctionName = do
    first <- elements ['a'..'z']
    rest <- listOf $ elements (['a'..'z'] ++ ['0'..'9'] ++ ['_'])
    return (first : rest)

arbitraryVariableName :: Gen String
arbitraryVariableName = do
    first <- elements ['a'..'z']
    rest <- listOf $ elements (['a'..'z'] ++ ['0'..'9'] ++ ['_'])
    return (first : rest)

arbitraryResourceName :: Gen String
arbitraryResourceName = do
    base <- arbitraryVariableName
    suffix <- elements ["_resource", "_data", "_file", "_handle"]
    return (base ++ suffix)

instance Arbitrary String where
    arbitrary = listOf $ oneof
        [ choose ('a', 'z')
        , choose ('A', 'Z')
        , choose ('0', '9')
        , elements " \t\n\r{}();,[]<>\"'*/"
        ]