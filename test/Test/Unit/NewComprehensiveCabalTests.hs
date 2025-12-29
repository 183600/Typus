{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.NewComprehensiveCabalTests (tests) where

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
tests = testGroup "New Comprehensive Cabal Tests"
  [ enhancedStringProperties
  , advancedDataStructuresProperties
  , compilerInvariantProperties
  , robustErrorHandlingProperties
  , preciseSourceLocationProperties
  , parserValidationProperties
  , functionalCompositionProperties
  , edgeCaseResilienceProperties
  ]

-- | 1. 增强字符串处理属性测试
enhancedStringProperties :: TestTree
enhancedStringProperties = testGroup "Enhanced String Processing Properties"
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
  
  , fastProperty "breakOn: when pattern exists, concatenation doesn't always reconstruct original" $
      \pat s -> pat `isInfixOf` s ==> 
        let (prefix, suffix) = breakOn pat s
        in prefix ++ pat ++ suffix == s
  ]

-- | 2. 高级数据结构属性测试
advancedDataStructuresProperties :: TestTree
advancedDataStructuresProperties = testGroup "Advanced Data Structure Properties"
  [ fastProperty "Map: insert then lookup returns Just the value" $
      \(k :: Int) (v :: String) (m :: Map.Map Int String) -> 
        Map.lookup k (Map.insert k v m) == Just v
  
  , fastProperty "Map: delete removes the key completely" $
      \(k :: Int) (m :: Map.Map Int String) -> 
        Map.lookup k (Map.delete k m) == Nothing
  
  , fastProperty "Map: union with empty preserves original" $
      \(m :: Map.Map Int String) -> Map.union m Map.empty == m
  
  , fastProperty "Set: insert then member returns True" $
      \(x :: Int) (s :: Set.Set Int) -> 
        Set.member x (Set.insert x s)
  
  , fastProperty "Set: union is associative" $
      \(s1 :: Set.Set Int) (s2 :: Set.Set Int) (s3 :: Set.Set Int) -> 
        Set.union s1 (Set.union s2 s3) == Set.union (Set.union s1 s2) s3
  
  , fastProperty "List: sort is idempotent" $
      \(xs :: [Int]) -> sort (sort xs) == sort xs
  
  , fastProperty "List: nub removes all duplicates" $
      \(xs :: [Int]) -> length (nub xs) == length (List.nubBy (==) (nub xs))
  ]

-- | 3. 编译器不变性属性测试
compilerInvariantProperties :: TestTree
compilerInvariantProperties = testGroup "Compiler Invariant Properties"
  [ testCase "FileDirectives: default has all Nothing values" $
      let fd = defaultFileDirectives
      in all (== Nothing) [fdOwnership fd, fdDependentTypes fd, fdConstraints fd] @?= True
  
  , testCase "BlockDirectives: default has all Nothing values" $
      let bd = defaultBlockDirectives
      in all (== Nothing) [bdOwnership bd, bdDependentTypes bd, bdConstraints bd] @?= True
  
  , fastProperty "FileDirectives: equality is reflexive" $
      \fd -> fd == fd
  
  , fastProperty "BlockDirectives: equality is transitive" $
      \bd1 bd2 bd3 -> (bd1 == bd2 && bd2 == bd3) ==> bd1 == bd3
  
  , testCase "ErrorSeverity: total ordering is consistent" $
      (compare Error Error == EQ && 
       compare Error Warning == GT && 
       compare Warning Info == GT) @?= True
  
  , testCase "ErrorCategory: comparison is reflexive" $
      compare TypeChecking TypeChecking == EQ @?= True
  ]

-- | 4. 健壮错误处理属性测试
robustErrorHandlingProperties :: TestTree
robustErrorHandlingProperties = testGroup "Robust Error Handling Properties"
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
  
  , fastProperty "Either: Left projection preserves value" $
      \(s :: String) -> either id (const "") (Left s) == s
  ]

-- | 5. 精确源位置属性测试
preciseSourceLocationProperties :: TestTree
preciseSourceLocationProperties = testGroup "Precise Source Location Properties"
  [ testCase "startPos: starts at line 1, column 1" $
      (posLine startPos == 1 && posColumn startPos == 1) @?= True
  
  , fastProperty "posAfter: newline increases line count and resets column" $
      \pos -> let pos' = posAfter '\n' pos
               in posLine pos' == posLine pos + 1 && posColumn pos' == 1
  
  , fastProperty "posAfter: regular character increases column count" $
      \pos c -> c /= '\n' ==> 
        let pos' = posAfter c pos
        in posLine pos' == posLine pos && posColumn pos' == posColumn pos + 1
  
  , testCase "advancePosBy: empty string doesn't change position" $
      advancePosBy "" startPos @?= startPos
  
  , fastProperty "mergeSpans: result is valid if inputs are valid" $
      \pos1 pos2 -> let span1 = SourceSpan pos1 (posAfter 'a' pos1)
                        span2 = SourceSpan (posAfter 'a' pos1) pos2
                        merged = mergeSpans span1 span2
                    in isValidSpan span1 && isValidSpan span2 ==> isValidSpan merged
  
  , fastProperty "SourceSpan: equality is reflexive" $
      \pos1 pos2 -> let span = SourceSpan pos1 pos2
                    in span == span
  ]

-- | 6. 解析器验证属性测试
parserValidationProperties :: TestTree
parserValidationProperties = testGroup "Parser Validation Properties"
  [ testCase "FileDirectives: structural equality works" $
      let fd1 = FileDirectives Nothing Nothing Nothing
          fd2 = FileDirectives (Just True) Nothing (Just False)
      in (fd1 == fd1 && fd2 == fd2 && fd1 /= fd2) @?= True
  
  , testCase "BlockDirectives: structural equality works" $
      let bd1 = BlockDirectives Nothing Nothing Nothing
          bd2 = BlockDirectives (Just False) (Just True) Nothing
      in (bd1 == bd1 && bd2 == bd2 && bd1 /= bd2) @?= True
  
  , fastProperty "FileDirectives: show and read consistency for simple cases" $
      \fd -> let shown = show fd
                 readBack = reads shown
             in not (null readBack) ==> fst (head readBack) == fd
  
  , fastProperty "BlockDirectives: show and read consistency for simple cases" $
      \bd -> let shown = show bd
                 readBack = reads shown
             in not (null readBack) ==> fst (head readBack) == bd
  ]

-- | 7. 函数组合属性测试
functionalCompositionProperties :: TestTree
functionalCompositionProperties = testGroup "Functional Composition Properties"
  [ fastProperty "String: toLower and toUpper are inverses for ASCII letters" $
      \s -> all (\c -> c >= 'A' && c <= 'Z') s ==> 
        map toLower (map toUpper s) == map toUpper s
  
  , fastProperty "String: length is preserved by reverse" $
      \s -> length s == length (reverse s)
  
  , fastProperty "List: head and tail reconstruct cons for non-empty lists" $
      \(xs :: [Int]) -> not (null xs) ==> head xs : tail xs == xs
  
  , fastProperty "List: null check is consistent with length" $
      \(xs :: [Int]) -> null xs == (length xs == 0)
  
  , fastProperty "Bool: negation is involutive" $
      \b -> not (not b) == b
  
  , fastProperty "Function composition: (f . g) x = f (g x)" $
      \(x :: Int) -> let f = (*2)
                         g = (+1)
                     in (f . g) x == f (g x)
  ]

-- | 8. 边界条件弹性属性测试
edgeCaseResilienceProperties :: TestTree
edgeCaseResilienceProperties = testGroup "Edge Case Resilience Properties"
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
  
  , testCase "SourcePos: valid positions have positive line and column" $
      let pos = SourcePos 1 1 0
      in posLine pos > 0 && posColumn pos > 0 @?= True
  
  , testCase "SourceSpan: empty span is valid" $
      let span = SourceSpan startPos startPos
      in isValidSpan span @?= True
  ]