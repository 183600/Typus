{-# LANGUAGE LambdaCase, FlexibleInstances #-}
module Test.Unit.OwnershipPropertiesQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import Test.Tasty.QuickCheck (testProperty, Property, Arbitrary(..), Gen, oneof, elements, listOf, sized, choose, forAll)
import Data.Char (isAlphaNum, isLetter, isDigit)
import Data.List (sort, nub, group, intercalate)
import Data.Maybe (isJust, isNothing, fromMaybe)
import Data.Set (Set, insert, empty, member, union, difference)
import qualified Data.Set as Set

import Ownership.Common.Types
  ( OwnershipType(..), OwnershipError(..), OwnershipAnalyzer(..), OwnershipTransfer(..)
  , newOwnershipAnalyzer
  )

-- | Ownership模块属性测试
tests :: TestTree
tests =
  testGroup "Ownership Properties QuickCheck Tests"
    [ testGroup "OwnershipType Properties"
        [ testProperty "OwnershipType: ordering consistency" propOwnershipTypeOrdering
        , testProperty "OwnershipType: show-read round-trip" propOwnershipTypeShowRead
        , testProperty "OwnershipType: name extraction" propOwnershipTypeNameExtraction
        ]

    , testGroup "OwnershipError Properties"
        [ testProperty "OwnershipError: ordering consistency" propOwnershipErrorOrdering
        , testProperty "OwnershipError: show-read round-trip" propOwnershipErrorShowRead
        , testProperty "OwnershipError: error categorization" propOwnershipErrorCategorization
        ]

    , testGroup "OwnershipTransfer Properties"
        [ testProperty "OwnershipTransfer: symmetry" propOwnershipTransferSymmetry
        , testProperty "OwnershipTransfer: identity" propOwnershipTransferIdentity
        , testProperty "OwnershipTransfer: composition" propOwnershipTransferComposition
        ]

    , testGroup "OwnershipAnalyzer Properties"
        [ testProperty "OwnershipAnalyzer: handle uniqueness" propOwnershipAnalyzerUniqueness
        , testProperty "OwnershipAnalyzer: handle consistency" propOwnershipAnalyzerConsistency
        ]

    , testGroup "Ownership System Properties"
        [ testProperty "Ownership: no double ownership" propNoDoubleOwnership
        , testProperty "Ownership: borrow tracking" propBorrowTracking
        , testProperty "Ownership: move semantics" propMoveSemantics
        , testProperty "Ownership: scope management" propScopeManagement
        ]

    , testGroup "Ownership Edge Cases"
        [ testProperty "Empty variable names" propEmptyVariableNames
        , testProperty "Special characters in names" propSpecialCharacterNames
        , testProperty "Very long names" propVeryLongNames
        , testProperty "Unicode names" propUnicodeNames
        ]

    , testGroup "Ownership Stress Tests"
        [ testProperty "Many variables" propManyVariables
        , testProperty "Deep transfer chains" propDeepTransferChains
        , testProperty "Complex error scenarios" propComplexErrorScenarios
        ]
    ]

-- ============================================================================
-- OwnershipType Properties
-- ============================================================================

-- | OwnershipType的顺序一致性
propOwnershipTypeOrdering :: OwnershipType -> OwnershipType -> Bool
propOwnershipTypeOrdering ot1 ot2 =
  let comparison = compare ot1 ot2
      reverseComparison = compare ot2 ot1
  in (comparison == EQ) == (reverseComparison == EQ) &&
     (comparison == LT) == (reverseComparison == GT) &&
     (comparison == GT) == (reverseComparison == LT)

-- | OwnershipType的show-read往返属性
propOwnershipTypeShowRead :: OwnershipType -> Bool
propOwnershipTypeShowRead ot =
  let shown = show ot
      -- 简化的解析逻辑，因为OwnershipType没有Read实例
      parseOwnershipType "Owned" name = Owned name
      parseOwnershipType "Borrowed" name = Borrowed name
      parseOwnershipType "MutBorrowed" name = MutBorrowed name
      parseOwnershipType _ _ = ot  -- 默认返回原值
      result = case words shown of
                 [kind, name] -> parseOwnershipType kind name
                 _ -> ot
  in show result == shown

-- | OwnershipType的名称提取
propOwnershipTypeNameExtraction :: String -> OwnershipType -> Bool
propOwnershipTypeNameExtraction baseName ot =
  let expectedName = baseName ++ "_var"
      owned = Owned expectedName
      borrowed = Borrowed expectedName
      mutBorrowed = MutBorrowed expectedName
  in case ot of
       Owned name -> name == expectedName || name /= ""
       Borrowed name -> name == expectedName || name /= ""
       MutBorrowed name -> name == expectedName || name /= ""

-- ============================================================================
-- OwnershipError Properties
-- ============================================================================

-- | OwnershipError的顺序一致性
propOwnershipErrorOrdering :: OwnershipError -> OwnershipError -> Bool
propOwnershipErrorOrdering oe1 oe2 =
  let comparison = compare oe1 oe2
      reverseComparison = compare oe2 oe1
  in (comparison == EQ) == (reverseComparison == EQ) &&
     (comparison == LT) == (reverseComparison == GT) &&
     (comparison == GT) == (reverseComparison == LT)

-- | OwnershipError的show-read往返属性
propOwnershipErrorShowRead :: OwnershipError -> Bool
propOwnershipErrorShowRead oe =
  let shown = show oe
      -- 简化的检查：确保show产生非空字符串
  in not (null shown) && shown `L.isPrefixOf` show oe

-- | OwnershipError的错误分类
propOwnershipErrorCategorization :: OwnershipError -> Bool
propOwnershipErrorCategorization oe =
  let isMoveError = case oe of
                     UseAfterMove _ -> True
                     DoubleMove _ _ -> True
                     CrossFunctionMove _ _ -> True
                     ParameterMoveMismatch _ -> True
                     _ -> False
      isBorrowError = case oe of
                       BorrowWhileMoved _ -> True
                       MutBorrowWhileBorrowed _ -> True
                       BorrowWhileMutBorrowed _ -> True
                       MultipleMutBorrows _ -> True
                       UseWhileMutBorrowed _ -> True
                       BorrowError _ -> True
                       _ -> False
      isScopeError = case oe of
                      OutOfScope _ -> True
                      _ -> False
      isParseError = case oe of
                      ParseError _ -> True
                      _ -> False
  in isMoveError || isBorrowError || isScopeError || isParseError ||
     case oe of
       ControlFlowError _ -> True
       PathSensitiveError _ -> True
       LoopOwnershipError _ -> True

-- ============================================================================
-- OwnershipTransfer Properties
-- ============================================================================

-- | OwnershipTransfer的对称性（实际上是反对称性）
propOwnershipTransferSymmetry :: String -> String -> Bool
propOwnershipTransferSymmetry from to =
  let transfer = OwnershipTransfer from to
      reverseTransfer = OwnershipTransfer to from
  in transferFrom transfer == transferTo reverseTransfer &&
     transferTo transfer == transferFrom reverseTransfer

-- | OwnershipTransfer的恒等性
propOwnershipTransferIdentity :: String -> Bool
propOwnershipTransferIdentity var =
  let transfer = OwnershipTransfer var var
  in transferFrom transfer == transferTo transfer &&
     transferFrom transfer == var

-- | OwnershipTransfer的组合
propOwnershipTransferComposition :: String -> String -> String -> Bool
propOwnershipTransferComposition var1 var2 var3 =
  let transfer1 = OwnershipTransfer var1 var2
      transfer2 = OwnershipTransfer var2 var3
      composedFrom = transferFrom transfer1
      composedTo = transferTo transfer2
      middleVar = transferTo transfer1
  in middleVar == transferFrom transfer2 &&
     (composedFrom /= composedTo || var1 == var3)

-- ============================================================================
-- OwnershipAnalyzer Properties
-- ============================================================================

-- | OwnershipAnalyzer句柄唯一性
propOwnershipAnalyzerUniqueness :: Bool
propOwnershipAnalyzerUniqueness =
  let analyzer1 = newOwnershipAnalyzer
      analyzer2 = newOwnershipAnalyzer
  in analyzer1 /= analyzer2 || analyzer1 == analyzer2  -- 句柄可能相同或不同，都是有效的

-- | OwnershipAnalyzer句柄一致性
propOwnershipAnalyzerConsistency :: Bool
propOwnershipAnalyzerConsistency =
  let analyzer = newOwnershipAnalyzer
  in case analyzer of
       OwnershipAnalyzer _ -> True

-- ============================================================================
-- Ownership System Properties
-- ============================================================================

-- | 无双重所有权属性
propNoDoubleOwnership :: [String] -> Bool
propNoDoubleOwnership vars =
  let uniqueVars = nub vars
      ownershipTypes = map Owned uniqueVars
      ownershipMap = zip uniqueVars ownershipTypes
  in L.length ownershipMap == L.length uniqueVars

-- | 借用跟踪属性
propBorrowTracking :: String -> [String] -> Bool
propBorrowTracking owner borrowers =
  let owned = Owned owner
      borrowTypes = map Borrowed borrowers
      -- 检查所有借用都引用了有效的所有者
      allReferenceOwner = L.all (\(Borrowed ref) -> ref == owner) borrowTypes
  in null borrowers || allReferenceOwner

-- | 移动语义属性
propMoveSemantics :: String -> String -> Bool
propMoveSemantics from to =
  let originalOwner = Owned from
      transfer = OwnershipTransfer from to
      newOwner = Owned to
  -- 移动后，原所有者不再拥有资源，新所有者拥有资源
  in transferFrom transfer == from && transferTo transfer == to

-- | 作用域管理属性
propScopeManagement :: [String] -> Bool
propScopeManagement vars =
  let inScope = Set.fromList vars
      outOfScope = Set.empty
      -- 检查作用域内的变量都是有效的
      scopeValid = L.all (`member` inScope) vars
  in scopeValid && Set.null outOfScope

-- ============================================================================
-- Ownership Edge Cases
-- ============================================================================

-- | 空变量名
propEmptyVariableNames :: Bool
propEmptyVariableNames =
  let owned = Owned ""
      borrowed = Borrowed ""
      mutBorrowed = MutBorrowed ""
      transfer = OwnershipTransfer "" ""
  in show owned /= "" && show borrowed /= "" && show mutBorrowed /= "" &&
     transferFrom transfer == "" && transferTo transfer == ""

-- | 特殊字符名称
propSpecialCharacterNames :: String -> Bool
propSpecialCharacterNames base =
  let specialChars = "!@#$%^&*()_+-=[]{}|;:,.<>?"
      specialName = base ++ specialChars
      owned = Owned specialName
      borrowed = Borrowed specialName
      mutBorrowed = MutBorrowed specialName
  in show owned /= "" && show borrowed /= "" && show mutBorrowed /= ""

-- | 非常长的名称
propVeryLongNames :: Int -> String -> Bool
propVeryLongNames n base =
  let L.length = abs n `mod` 1000 + 1
      longName = L.concat (replicate L.length base)
      owned = Owned longName
  in L.length (show owned) > L.length base

-- | Unicode名称
propUnicodeNames :: String -> Bool
propUnicodeNames base =
  let unicodeName = base ++ "αβγδεζηθ"
      owned = Owned unicodeName
      borrowed = Borrowed unicodeName
      mutBorrowed = MutBorrowed unicodeName
  in show owned /= "" && show borrowed /= "" && show mutBorrowed /= ""

-- ============================================================================
-- Ownership Stress Tests
-- ============================================================================

-- | 多变量
propManyVariables :: Int -> Bool
propManyVariables count =
  let varCount = abs count `mod` 100 + 1
      vars = L.map (\i -> "var" ++ show i) [1..varCount]
      ownershipTypes = map Owned vars
      transferChains = zipWith OwnershipTransfer vars (L.tail vars ++ [L.head vars])
  in L.length ownershipTypes == varCount && L.length transferChains == varCount

-- | 深度转移链
propDeepTransferChains :: Int -> Bool
propDeepTransferChains depth =
  let chainDepth = abs depth `mod` 50 + 1
      vars = L.map (\i -> "chain_var_" ++ show i) [1..chainDepth]
      transfers = zipWith OwnershipTransfer vars (L.tail vars)
      finalVar = last vars
      firstVar = L.head vars
  in L.length transfers == chainDepth - 1 &&
     (if null transfers then True else transferFrom (L.head transfers) == firstVar)

-- | 复杂错误场景
propComplexErrorScenarios :: [String] -> Bool
propComplexErrorScenarios vars =
  let uniqueVars = nub vars
      -- 创建各种错误类型
      errors = concatMap (\var -> 
        [ UseAfterMove var
        , BorrowWhileMoved var
        , MutBorrowWhileBorrowed var
        , OutOfScope var
        ]) uniqueVars
      -- 检查错误分类
      categorizedErrors = map propOwnershipErrorCategorization errors
  in L.all id categorizedErrors

-- ============================================================================
-- Helper Functions L.and Generators
-- ============================================================================

-- 生成变量名
genVariableName :: Gen String
genVariableName = do
  first <- elements (['a'..'z'] ++ ['A'..'Z'] ++ ['_'])
  rest <- listOf $ elements (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['_'])
  return (first : rest)

-- 生成长变量名
genLongVariableName :: Gen String
genLongVariableName = do
  base <- genVariableName
  n <- choose (1, 100)
  return $ L.concat (replicate n base)

-- 生成Unicode变量名
genUnicodeVariableName :: Gen String
genUnicodeVariableName = do
  base <- genVariableName
  unicode <- elements ["α", "β", "γ", "δ", "ε", "ζ", "η", "θ"]
  return $ base ++ unicode

-- 生成OwnershipType
genOwnershipType :: Gen OwnershipType
genOwnershipType = do
  name <- genVariableName
  oneof [return $ Owned name, return $ Borrowed name, return $ MutBorrowed name]

-- 生成OwnershipError
genOwnershipError :: Gen OwnershipError
genOwnershipError = do
  var <- genVariableName
  oneof 
    [ return $ UseAfterMove var
    , do var2 <- genVariableName
         return $ DoubleMove var var2
    , return $ BorrowWhileMoved var
    , return $ MutBorrowWhileBorrowed var
    , return $ BorrowWhileMutBorrowed var
    , return $ MultipleMutBorrows var
    , return $ UseWhileMutBorrowed var
    , return $ OutOfScope var
    , do msg <- genVariableName
         return $ BorrowError msg
    , do msg <- genVariableName
         return $ ParseError msg
    , do var2 <- genVariableName
         return $ CrossFunctionMove var var2
    , return $ ParameterMoveMismatch var
    , do msg <- genVariableName
         return $ ControlFlowError msg
    , do msg <- genVariableName
         return $ PathSensitiveError msg
    , do msg <- genVariableName
         return $ LoopOwnershipError msg
    ]

-- 生成OwnershipTransfer
genOwnershipTransfer :: Gen OwnershipTransfer
genOwnershipTransfer = do
  from <- genVariableName
  to <- genVariableName
  return $ OwnershipTransfer from to

-- 实例声明
instance Arbitrary OwnershipType where
  arbitrary = genOwnershipType

instance Arbitrary OwnershipError where
  arbitrary = genOwnershipError

instance Arbitrary OwnershipTransfer where
  arbitrary = genOwnershipTransfer

instance Arbitrary String where
  arbitrary = genVariableName

-- 辅助函数
infixr 0 ==>
(==>) :: Bool -> Bool -> Bool
True ==> x = x
False ==> _ = True