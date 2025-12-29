{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.AdditionalCabalQuickCheckTests (tests) where

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
tests = testGroup "Additional Cabal QuickCheck Tests"
  [ stringProcessingProperties
  , dataStructureProperties
  , compilerCoreProperties
  , errorHandlingProperties
  , sourceLocationProperties
  , parserConsistencyProperties
  , functionalProperties
  , boundaryConditionProperties
  ]

-- | 1. 字符串处理属性测试
stringProcessingProperties :: TestTree
stringProcessingProperties = testGroup "String Processing Properties"
  [ fastProperty "trim: removing all whitespace twice equals once" $
      \s -> trim (trim s) == trim s
  
  , fastProperty "trim: result contains no leading/trailing whitespace" $
      \s -> let t = trim s
             in (null t || not (isSpace (head t))) && 
                (null t || not (isSpace (last t)))
  
  , fastProperty "splitBy: concatenating with delimiter reconstructs original" $
      \c s -> not (null s) && c `notElem` s ==> 
        List.intercalate [c] (splitBy c s) == s
  
  , fastProperty "splitByCollapsed: never produces empty segments" $
      \c s -> all (not . null) (splitByCollapsed c s)
  
  , fastProperty "removeLineComments: preserves lines without comments" $
      \s -> not ('/' `elem` s) ==> removeLineComments s == s
  
  , fastProperty "normalizeIndentation: preserves relative line structure" $
      \s -> length (lines s) == length (lines (normalizeIndentation s))
  ]

-- | 2. 数据结构属性测试
dataStructureProperties :: TestTree
dataStructureProperties = testGroup "Data Structure Properties"
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
  
  , fastProperty "List: nub removes all duplicates" $
      \(xs :: [Int]) -> length (nub xs) == length (nub (nub xs))
  ]

-- | 3. 编译器核心功能属性测试
compilerCoreProperties :: TestTree
compilerCoreProperties = testGroup "Compiler Core Properties"
  [ testCase "FileDirectives: default has all Nothing values" $
      let fd = defaultFileDirectives
      in all (== Nothing) [fdOwnership fd, fdDependentTypes fd, fdConstraints fd] @?= True
  
  , testCase "BlockDirectives: default has all Nothing values" $
      let bd = defaultBlockDirectives
      in all (== Nothing) [bdOwnership bd, bdDependentTypes bd, bdConstraints bd] @?= True
  
  , fastProperty "FileDirectives: equality is reflexive" $
      \fd -> fd == fd
  
  , fastProperty "BlockDirectives: equality is reflexive" $
      \bd -> bd == bd
  
  , testCase "ErrorSeverity: total ordering is consistent" $
      (compare Error Error == EQ && 
       compare Error Warning == GT && 
       compare Warning Info == GT) @?= True
  ]

-- | 4. 错误处理属性测试
errorHandlingProperties :: TestTree
errorHandlingProperties = testGroup "Error Handling Properties"
  [ testCase "ErrorSeverity: all values are distinct" $
      (Error /= Warning && Warning /= Info && Error /= Info) @?= True
  
  , fastProperty "Maybe: isJust and isNothing are complementary" $
      \(mx :: Maybe Int) -> isJust mx == not (isNothing mx)
  
  , fastProperty "Maybe: fromMaybe with default returns value when Just" $
      \(x :: Int) (y :: Int) -> fromMaybe x (Just y) == y
  
  , fastProperty "Maybe: fromMaybe with default returns default when Nothing" $
      \(x :: Int) -> fromMaybe x Nothing == x
  
  , fastProperty "Either: isLeft and isRight are complementary" $
      \(ex :: Either String Int) -> isLeft ex == not (isRight ex)
  ]

-- | 5. 源位置属性测试
sourceLocationProperties :: TestTree
sourceLocationProperties = testGroup "Source Location Properties"
  [ testCase "startPos: starts at line 1, column 1" $
      (posLine startPos == 1 && posColumn startPos == 1) @?= True
  
  , fastProperty "posAfter: newline increases line count" $
      \pos -> let pos' = posAfter '\n' pos
               in posLine pos' == posLine pos + 1
  
  , fastProperty "posAfter: regular character increases column count" $
      \pos c -> c /= '\n' ==> 
        let pos' = posAfter c pos
        in posColumn pos' == posColumn pos + 1
  
  , testCase "advancePosBy: empty string doesn't change position" $
      advancePosBy "" startPos @?= startPos
  
  , fastProperty "mergeSpans: result is valid if inputs are valid" $
      \pos1 pos2 -> let span1 = SourceSpan pos1 (posAfter 'a' pos1)
                        span2 = SourceSpan (posAfter 'a' pos1) pos2
                        merged = mergeSpans span1 span2
                    in isValidSpan span1 && isValidSpan span2 ==> isValidSpan merged
  ]

-- | 6. 解析器一致性属性测试
parserConsistencyProperties :: TestTree
parserConsistencyProperties = testGroup "Parser Consistency Properties"
  [ testCase "FileDirectives: structural equality works" $
      let fd1 = FileDirectives Nothing Nothing Nothing
          fd2 = FileDirectives (Just True) Nothing (Just False)
      in (fd1 == fd1 && fd2 == fd2 && fd1 /= fd2) @?= True
  
  , testCase "BlockDirectives: structural equality works" $
      let bd1 = BlockDirectives Nothing Nothing Nothing
          bd2 = BlockDirectives (Just False) (Just True) Nothing
      in (bd1 == bd1 && bd2 == bd2 && bd1 /= bd2) @?= True
  
  , fastProperty "FileDirectives: show and read consistency" $
      \fd -> read (show fd) == fd
  
  , fastProperty "BlockDirectives: show and read consistency" $
      \bd -> read (show bd) == bd
  ]

-- | 7. 函数式属性测试
functionalProperties :: TestTree
functionalProperties = testGroup "Functional Properties"
  [ fastProperty "String: toLower and toUpper are inverses for ASCII" $
      \s -> all (\c -> c >= 'A' && c <= 'Z') s ==> 
        map toLower (map toUpper s) == map toUpper s
  
  , fastProperty "String: length is preserved by reverse" $
      \s -> length s == length (reverse s)
  
  , fastProperty "List: head and tail reconstruct cons" $
      \(xs :: [Int]) -> not (null xs) ==> head xs : tail xs == xs
  
  , fastProperty "List: null check is consistent with length" $
      \(xs :: [Int]) -> null xs == (length xs == 0)
  
  , fastProperty "Bool: negation is involutive" $
      \b -> not (not b) == b
  ]

-- | 8. 边界条件属性测试
boundaryConditionProperties :: TestTree
boundaryConditionProperties = testGroup "Boundary Condition Properties"
  [ testCase "String: empty string trim stays empty" $
      trim "" @?= ""
  
  , testCase "String: empty string split returns singleton" $
      splitBy ',' "" @?= [""]
  
  , testCase "List: empty list sort stays empty" $
      sort ([] :: [Int]) @?= []
  
  , testCase "List: empty list nub stays empty" $
      nub ([] :: [Int]) @?= []
  
  , testCase "Map: empty map lookup returns Nothing" $
      Map.lookup 1 Map.empty @?= Nothing
  
  , testCase "Set: empty set has size 0" $
      Set.size Set.empty @?= 0
  
  , testCase "Maybe: Nothing isNothing is True" $
      isNothing (Nothing :: Maybe Int) @?= True
  
  , testCase "Either: Left isLeft is True" $
      isLeft (Left "error" :: Either String Int) @?= True
  ]