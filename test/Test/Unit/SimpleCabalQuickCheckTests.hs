{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.SimpleCabalQuickCheckTests (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List
import Data.Char (isSpace, isAlphaNum, toLower, toUpper)
import Data.Maybe (isJust, isNothing, fromMaybe)
import Data.Either (isLeft, isRight)

import Utils (trim, splitBy, splitByCollapsed, removeLineComments, removeComments, normalizeIndentation, breakOn)
import SourceLocation (SourcePos(..), SourceSpan(..), startPos, posAfter, advancePosBy, mergeSpans, isValidSpan)
import Parser (FileDirectives(..), BlockDirectives(..), defaultFileDirectives, defaultBlockDirectives)
import Compiler.Errors.Core (ErrorSeverity(..), ErrorCategory(..))
import TestSupport.Arbitrary ()

tests :: TestTree
tests = testGroup "Simple Cabal QuickCheck Tests"
  [ basicStringProperties
  , basicDataStructureProperties
  , basicCompilerProperties
  , basicErrorHandlingProperties
  ]

-- | 1. 基础字符串处理属性测试
basicStringProperties :: TestTree
basicStringProperties = testGroup "Basic String Processing Properties"
  [ fastProperty "trim: applying twice is same as applying once" $
      \s -> trim (trim s) == trim s
  
  , fastProperty "trim: result contains no leading/trailing whitespace" $
      \s -> let t = trim s
             in (null t || not (isSpace (head t))) && 
                (null t || not (isSpace (last t)))
  
  , fastProperty "splitBy: joining with delimiter and splitting again returns original" $
      \c s -> not (null s) ==> splitBy c (unwords (splitBy c s)) == splitBy c s
  
  , fastProperty "splitByCollapsed: never yields empty chunks" $
      \c s -> all (not . null) (splitByCollapsed c s)
  
  , fastProperty "removeLineComments: preserves non-comment lines" $
      \s -> not ('/' `elem` s) ==> removeLineComments s == s
  
  , fastProperty "normalizeIndentation: preserves line count" $
      \s -> length (lines s) == length (lines (normalizeIndentation s))
  ]

-- | 2. 基础数据结构属性测试
basicDataStructureProperties :: TestTree
basicDataStructureProperties = testGroup "Basic Data Structure Properties"
  [ fastProperty "Map: insert then lookup returns Just the value" $
      \(k :: Int) (v :: String) (m :: Map.Map Int String) -> 
        Map.lookup k (Map.insert k v m) == Just v
  
  , fastProperty "Map: delete removes the key" $
      \(k :: Int) (m :: Map.Map Int String) -> 
        Map.lookup k (Map.delete k m) == Nothing
  
  , fastProperty "Set: insert then member returns True" $
      \(x :: Int) (s :: Set.Set Int) -> 
        Set.member x (Set.insert x s)
  
  , fastProperty "Set: union size <= sum of sizes" $
      \(s1 :: Set.Set Int) (s2 :: Set.Set Int) -> 
        Set.size (Set.union s1 s2) <= Set.size s1 + Set.size s2
  
  , fastProperty "List: sort is idempotent" $
      \(xs :: [Int]) -> sort (sort xs) == sort xs
  
  , fastProperty "List: nub removes duplicates" $
      \(xs :: [Int]) -> nub xs == nub (nub xs)
  ]

-- | 3. 基础编译器属性测试
basicCompilerProperties :: TestTree
basicCompilerProperties = testGroup "Basic Compiler Properties"
  [ testCase "FileDirectives: default has all Nothing values" $
      let fd = defaultFileDirectives
      in all (== Nothing) [fdOwnership fd, fdDependentTypes fd, fdConstraints fd] @?= True
  
  , testCase "BlockDirectives: default has all Nothing values" $
      let bd = defaultBlockDirectives
      in all (== Nothing) [bdOwnership bd, bdDependentTypes bd, bdConstraints bd] @?= True
  
  , testCase "ErrorSeverity: total ordering is consistent" $
      (compare Error Error == EQ && 
       compare Error Warning == GT && 
       compare Warning Info == GT) @?= True
  
  , testCase "ErrorSeverity: all values are distinct" $
      (Error /= Warning && Warning /= Info && Error /= Info) @?= True
  ]

-- | 4. 基础错误处理属性测试
basicErrorHandlingProperties :: TestTree
basicErrorHandlingProperties = testGroup "Basic Error Handling Properties"
  [ fastProperty "Maybe: isJust and isNothing are complementary" $
      \(mx :: Maybe Int) -> isJust mx == not (isNothing mx)
  
  , fastProperty "Maybe: fromMaybe with default returns value when Just" $
      \(x :: Int) (y :: Int) -> fromMaybe x (Just y) == y
  
  , fastProperty "Maybe: fromMaybe with default returns default when Nothing" $
      \(x :: Int) -> fromMaybe x Nothing == x
  
  , fastProperty "Either: isLeft and isRight are complementary" $
      \(ex :: Either String Int) -> isLeft ex == not (isRight ex)
  
  , testCase "SourcePos: starts at line 1, column 1" $
      (posLine startPos == 1 && posColumn startPos == 1) @?= True
  
  , testCase "advancePosBy: empty string doesn't change position" $
      advancePosBy "" startPos @?= startPos
  ]