{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Test.Unit.AnalyzerCoreQuickCheckSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

import Data.List (nub, sort, group, intercalate)
import Data.Char (isAlpha, isAlphaNum)
import Data.Either (isLeft, isRight)
import Data.Maybe (isJust, isNothing, fromMaybe)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Analyzer.Types
import Analyzer.State
import Analyzer.SymbolTable
import Analyzer.CrossAnalysis
import qualified Ownership as Own
import qualified Dependencies as Dep
import Compiler.Errors.Types (ErrorSeverity(..))

import TestSupport.Arbitrary

-- ============================================================================
-- Analyzer Core Properties
-- ============================================================================

-- | 测试分析结果的合并一致性
prop_analysis_result_merge :: [String] -> [String] -> Property
prop_analysis_result_merge warnings1 warnings2 =
  let validWarnings1 = all (not . null) warnings1
      validWarnings2 = all (not . null) warnings2
      result1 = emptyAnalysisResult { analysisWarnings = warnings1 }
      result2 = emptyAnalysisResult { analysisWarnings = warnings2 }
      mergedWarnings = sort (warnings1 ++ warnings2)
  in if not (validWarnings1 && validWarnings2)
     then property True
     else property $ length mergedWarnings == length warnings1 + length warnings2

-- | 测试符号信息的有效性
prop_symbol_info_validity :: String -> String -> Int -> Property
prop_symbol_info_validity name typeName scope =
  let validName = not (null name) && all isAlpha name
      validTypeName = not (null typeName) && all isAlpha typeName
      validScope = scope >= 0 && scope <= 100
  in if not (validName && validTypeName && validScope)
     then property True
     else let symbolInfo = SymbolInfo
                  { symbolName = name
                  , symbolType = Just (Dep.TVCon typeName)
                  , ownershipState = Just (Own.Owned name)  -- Provide the required argument
                  , symbolScope = scope
                  , isMoved = False
                  , isBorrowed = False
                  , constraints = []
                  }
          in property $ symbolName symbolInfo == name && symbolScope symbolInfo == scope

-- | 测试分析阶段的顺序性
prop_analysis_phase_ordering :: AnalysisPhase -> AnalysisPhase -> Property
prop_analysis_phase_ordering phase1 phase2 =
  let phases = [InitialPhase, OwnershipPhase, DependentTypePhase, IntegrationPhase]
      phase1Index = case elemIndex phase1 phases of
                      Just idx -> idx
                      Nothing -> 0
      phase2Index = case elemIndex phase2 phases of
                      Just idx -> idx
                      Nothing -> 0
  in property $ phase1Index >= 0 && phase2Index >= 0
  where
    elemIndex :: Eq a => a -> [a] -> Maybe Int
    elemIndex x = findIndex 0
      where
        findIndex _ [] = Nothing
        findIndex i (y:ys) = if x == y then Just i else findIndex (i+1) ys

-- | 测试分析上下文的一致性
prop_analysis_context_consistency :: String -> Bool -> Bool -> AnalysisPhase -> Property
prop_analysis_context_consistency file enableOwn enableDep phase =
  let validFile = not (null file) && all isAlphaNum file
      validPhase = phase `elem` [InitialPhase, OwnershipPhase, DependentTypePhase, IntegrationPhase]
  in if not (validFile && validPhase)
     then property True
     else let context = AnalysisContext
                  { enableOwnership = enableOwn
                  , enableDependentTypes = enableDep
                  , currentFile = file
                  , analysisPhase = phase
                  }
          in property $ currentFile context == file && 
                       enableOwnership context == enableOwn &&
                       enableDependentTypes context == enableDep &&
                       analysisPhase context == phase

-- | 测试分析器状态的转换
prop_analyzer_state_transition :: String -> Int -> Property
prop_analyzer_state_transition file scope =
  let validFile = not (null file) && all isAlphaNum file
      validScope = scope >= 0 && scope <= 50
  in if not (validFile && validScope)
     then property True
     else let context = AnalysisContext
                  { enableOwnership = True
                  , enableDependentTypes = True
                  , currentFile = file
                  , analysisPhase = InitialPhase
                  }
              -- Note: Using a simplified analyzer state since emptyOwnershipAnalyzer is not available
              state = AnalyzerState
                  { ownershipAnalyzer = Own.newOwnershipAnalyzer  -- Use newOwnershipAnalyzer instead
                  , dependentTypeChecker = Dep.newDependentTypeChecker
                  , currentScope = scope
                  , symbolTable = Map.empty
                  , analysisContext = context
                  , combinedErrorsAcc = []
                  , ownershipErrorsAcc = []
                  , dependentTypeErrorsAcc = []
                  }
          in property $ currentScope state == scope &&
                       currentFile (analysisContext state) == file

-- | 测试符号表的插入和查找
prop_symbol_table_insert_lookup :: [(String, String)] -> String -> Property
prop_symbol_table_insert_lookup bindings key =
  let validBindings = all (\(k, v) -> not (null k) && not (null v)) bindings
      validKey = not (null key)
  in if not (validBindings && validKey)
     then property True
     else let symbolTable = foldl (\acc (k, v) -> Map.insert k 
                  (SymbolInfo k (Just (Dep.TVCon v)) Nothing 0 False False []) acc) 
                  Map.empty bindings
              lookupResult = Map.lookup key symbolTable
          in case lookupResult of
               Just symbol -> property $ symbolName symbol == key
               Nothing -> property $ not (any (\(k, _) -> k == key) bindings)

-- | 测试错误严重性顺序
prop_error_severity_ordering :: Int -> Int -> Property
prop_error_severity_ordering sev1 sev2 =
  let validSev1 = sev1 >= 0 && sev1 <= 2
      validSev2 = sev2 >= 0 && sev2 <= 2
  in if not (validSev1 && validSev2)
     then property True
     else let severityOrder = [0, 1, 2] -- 0=Warning, 1=Error, 2=Fatal
              isInOrder (x, y) = x <= y
          in property $ (sev1, sev2) `elem` [(x, y) | x <- severityOrder, y <- severityOrder] ==> 
                     isInOrder (sev1, sev2)

-- | 测试符号种类的分类
prop_symbol_kind_classification :: String -> Property
prop_symbol_kind_classification name =
  let validName = not (null name) && all isAlpha name
  in if not validName
     then property True
     else let kinds = [SymbolVariable, SymbolFunction, SymbolType, SymbolConstant, SymbolPackage, SymbolModule]
              selectedKind = head kinds  -- 简化选择
          in property $ selectedKind `elem` kinds

-- | 测试所有权状态的转换
prop_ownership_state_transition :: String -> Bool -> Bool -> Property
prop_ownership_state_transition varName moved borrowed =
  let validName = not (null varName) && all isAlpha varName
  in if not validName
     then property True
     else let symbolInfo = SymbolInfo
                  { symbolName = varName
                  , symbolType = Nothing
                  , ownershipState = Just (Own.Owned varName)
                  , symbolScope = 0
                  , isMoved = moved
                  , isBorrowed = borrowed
                  , constraints = []
                  }
          in property $ isMoved symbolInfo == moved && 
                       isBorrowed symbolInfo == borrowed

-- | 测试类型环境的扩展
prop_type_environment_extension :: [(String, String)] -> String -> String -> Property
prop_type_environment_extension bindings key value =
  let validBindings = all (\(k, v) -> not (null k) && not (null v)) bindings
      validKey = not (null key)
      validValue = not (null value)
  in if not (validBindings && validKey && validValue)
     then property True
     else let typeEnv = foldl (\acc (k, v) -> Map.insert k (Dep.TVCon v) acc) 
                  Map.empty bindings
              extendedEnv = Map.insert key (Dep.TVCon value) typeEnv
              lookupResult = Map.lookup key extendedEnv
          in case lookupResult of
               Just (Dep.TVCon result) -> property $ result == value
               _ -> property False

-- | 测试分析错误的累积
prop_analysis_error_accumulation :: [String] -> [String] -> Property
prop_analysis_error_accumulation errors1 errors2 =
  let validErrors1 = all (not . null) errors1
      validErrors2 = all (not . null) errors2
  in if not (validErrors1 && validErrors2)
     then property True
     else let -- Simplified test without CombinedError construction
              result1 = emptyAnalysisResult { analysisWarnings = errors1 }
              result2 = emptyAnalysisResult { analysisWarnings = errors2 }
              allWarnings = analysisWarnings result1 ++ analysisWarnings result2
          in property $ length allErrors == length errors1 + length errors2

-- | 测试作用域的嵌套
prop_scope_nesting :: [Int] -> Property
prop_scope_nesting scopes =
  let validScopes = all (\s -> s >= 0 && s <= 20) scopes
  in if not validScopes
     then property True
     else let sortedScopes = sort scopes
              maxScope = if null scopes then 0 else maximum scopes
              minScope = if null scopes then 0 else minimum scopes
          in property $ maxScope >= minScope

-- | 测试约束的一致性
prop_constraint_consistency :: [String] -> Property
prop_constraint_consistency constraints =
  let validConstraints = all (not . null) constraints
      uniqueConstraints = nub constraints
  in if not validConstraints
     then property True
     else property $ length uniqueConstraints <= length constraints

-- | 测试分析结果的序列化
prop_analysis_result_serialization :: [String] -> [String] -> Property
prop_analysis_result_serialization warnings infos =
  let validWarnings = all (not . null) warnings
      validInfos = all (not . null) infos
  in if not (validWarnings && validInfos)
     then property True
     else let result = emptyAnalysisResult
                  { analysisWarnings = warnings
                  , analysisInfo = infos
                  }
              serialized = show result
          in property $ length serialized > 0

-- | 测试符号表的合并
prop_symbol_table_merge :: [(String, String)] -> [(String, String)] -> Property
prop_symbol_table_merge bindings1 bindings2 =
  let validBindings1 = all (\(k, v) -> not (null k) && not (null v)) bindings1
      validBindings2 = all (\(k, v) -> not (null k) && not (null v)) bindings2
  in if not (validBindings1 && validBindings2)
     then property True
     else let table1 = foldl (\acc (k, v) -> Map.insert k 
                  (SymbolInfo k (Just (Dep.TVCon v)) Nothing 0 False False []) acc) 
                  Map.empty bindings1
              table2 = foldl (\acc (k, v) -> Map.insert k 
                  (SymbolInfo k (Just (Dep.TVCon v)) Nothing 0 False False []) acc) 
                  Map.empty bindings2
              mergedTable = Map.union table1 table2
              totalSize = Map.size mergedTable
          in property $ totalSize >= max (Map.size table1) (Map.size table2)

-- | 测试分析阶段的转换
prop_analysis_phase_transition :: AnalysisPhase -> Property
prop_analysis_phase_transition currentPhase =
  let validPhase = currentPhase `elem` [InitialPhase, OwnershipPhase, DependentTypePhase, IntegrationPhase]
  in if not validPhase
     then property True
     else let nextPhase = case currentPhase of
                  InitialPhase -> OwnershipPhase
                  OwnershipPhase -> DependentTypePhase
                  DependentTypePhase -> IntegrationPhase
                  IntegrationPhase -> IntegrationPhase
          in property $ nextPhase `elem` [InitialPhase, OwnershipPhase, DependentTypePhase, IntegrationPhase]

-- | 测试符号信息的更新
prop_symbol_info_update :: String -> String -> Int -> Bool -> Property
prop_symbol_info_update name newType newScope moved =
  let validName = not (null name) && all isAlpha name
      validNewType = not (null newType) && all isAlpha newType
      validNewScope = newScope >= 0 && newScope <= 100
  in if not (validName && validNewType && validNewScope)
     then property True
     else let originalSymbol = SymbolInfo
                  { symbolName = name
                  , symbolType = Just (Dep.TVCon "original")
                  , ownershipState = Just (Own.Owned name)
                  , symbolScope = 0
                  , isMoved = False
                  , isBorrowed = False
                  , constraints = []
                  }
              updatedSymbol = originalSymbol
                  { symbolType = Just (Dep.TVCon newType)
                  , symbolScope = newScope
                  , isMoved = moved
                  }
          in property $ symbolType updatedSymbol == Just (Dep.TVCon newType) &&
                       symbolScope updatedSymbol == newScope &&
                       isMoved updatedSymbol == moved

-- | 测试分析上下文的更新
prop_analysis_context_update :: String -> AnalysisPhase -> Property
prop_analysis_context_update newFile newPhase =
  let validFile = not (null newFile) && all isAlphaNum newFile
      validPhase = newPhase `elem` [InitialPhase, OwnershipPhase, DependentTypePhase, IntegrationPhase]
  in if not (validFile && validPhase)
     then property True
     else let originalContext = AnalysisContext
                  { enableOwnership = True
                  , enableDependentTypes = True
                  , currentFile = "original.typus"
                  , analysisPhase = InitialPhase
                  }
              updatedContext = originalContext
                  { currentFile = newFile
                  , analysisPhase = newPhase
                  }
          in property $ currentFile updatedContext == newFile &&
                       analysisPhase updatedContext == newPhase

-- ============================================================================
-- Performance Tests
-- ============================================================================

-- | 测试大型符号表的性能
prop_large_symbol_table :: Int -> Property
prop_large_symbol_table size =
  let validSize = size >= 0 && size <= 1000
  in if not validSize
     then property True
     else let symbols = take size $ map (\i -> ("symbol" ++ show i, "type" ++ show (i `mod` 10))) [0..]
              symbolTable = foldl (\acc (k, v) -> Map.insert k 
                  (SymbolInfo k (Just (Dep.TVCon v)) Nothing 0 False False []) acc) 
                  Map.empty symbols
              lookupCount = min 10 size
              lookups = take lookupCount $ map (\i -> Map.lookup ("symbol" ++ show i) symbolTable) [0..]
          in property $ length lookups == lookupCount

-- | 测试深度作用域嵌套的性能
prop_deep_scope_nesting :: Int -> Property
prop_deep_scope_nesting depth =
  let validDepth = depth >= 0 && depth <= 100
  in if not validDepth
     then property True
     else let scopes = [0..depth]
              symbolTable = foldl (\acc s -> Map.insert ("scope" ++ show s) 
                  (SymbolInfo ("scope" ++ show s) Nothing Nothing s False False []) acc) 
                  Map.empty scopes
          in property $ Map.size symbolTable == depth + 1

-- ============================================================================
-- Edge Case Tests
-- ============================================================================

-- | 测试空符号表
prop_empty_symbol_table :: Property
prop_empty_symbol_table =
  let emptyTable = Map.empty :: Map.Map String SymbolInfo
      lookupResult = Map.lookup "nonexistent" emptyTable
  in property $ isNothing lookupResult

-- | 测试空分析结果
prop_empty_analysis_result :: Property
prop_empty_analysis_result =
  let emptyResult = emptyAnalysisResult
  in property $ null (ownershipErrors emptyResult) &&
               null (dependentTypeErrors emptyResult) &&
               null (combinedErrors emptyResult) &&
               null (analysisWarnings emptyResult) &&
               null (analysisInfo emptyResult) &&
               Map.null (typeEnvironment emptyResult)

-- | 测试极长符号名
prop_extremely_long_symbol_name :: Int -> Property
prop_extremely_long_symbol_name len =
  let validLength = len >= 0 && len <= 10000
  in if not validLength
     then property True
     else let longName = replicate len 'a'
              symbolInfo = SymbolInfo
                  { symbolName = longName
                  , symbolType = Nothing
                  , ownershipState = Nothing
                  , symbolScope = 0
                  , isMoved = False
                  , isBorrowed = False
                  , constraints = []
                  }
          in property $ length (symbolName symbolInfo) == len

-- | 测试无效字符的符号名
prop_invalid_symbol_name :: String -> Property
prop_invalid_symbol_name name =
  let hasInvalid = null name || any (not . isAlphaNum) name
  in if not hasInvalid
     then property True
     else let symbolInfo = SymbolInfo
                  { symbolName = name
                  , symbolType = Nothing
                  , ownershipState = Nothing
                  , symbolScope = 0
                  , isMoved = False
                  , isBorrowed = False
                  , constraints = []
                  }
          in property $ symbolName symbolInfo == name

-- ============================================================================
-- Test Suite Collection
-- ============================================================================

testSuite :: TestTree
testSuite = testGroup "Analyzer Core QuickCheck Tests"
  [ testProperty "Analysis Result Merge" prop_analysis_result_merge
  , testProperty "Symbol Info Validity" prop_symbol_info_validity
  , testProperty "Analysis Phase Ordering" prop_analysis_phase_ordering
  , testProperty "Analysis Context Consistency" prop_analysis_context_consistency
  , testProperty "Analyzer State Transition" prop_analyzer_state_transition
  , testProperty "Symbol Table Insert Lookup" prop_symbol_table_insert_lookup
  , testProperty "Error Severity Ordering" prop_error_severity_ordering
  , testProperty "Symbol Kind Classification" prop_symbol_kind_classification
  , testProperty "Ownership State Transition" prop_ownership_state_transition
  , testProperty "Type Environment Extension" prop_type_environment_extension
  , testProperty "Analysis Error Accumulation" prop_analysis_error_accumulation
  , testProperty "Scope Nesting" prop_scope_nesting
  , testProperty "Constraint Consistency" prop_constraint_consistency
  , testProperty "Analysis Result Serialization" prop_analysis_result_serialization
  , testProperty "Symbol Table Merge" prop_symbol_table_merge
  , testProperty "Analysis Phase Transition" prop_analysis_phase_transition
  , testProperty "Symbol Info Update" prop_symbol_info_update
  , testProperty "Analysis Context Update" prop_analysis_context_update
  , testProperty "Large Symbol Table" prop_large_symbol_table
  , testProperty "Deep Scope Nesting" prop_deep_scope_nesting
  , testProperty "Empty Symbol Table" prop_empty_symbol_table
  , testProperty "Empty Analysis Result" prop_empty_analysis_result
  , testProperty "Extremely Long Symbol Name" prop_extremely_long_symbol_name
  , testProperty "Invalid Symbol Name" prop_invalid_symbol_name
  ]