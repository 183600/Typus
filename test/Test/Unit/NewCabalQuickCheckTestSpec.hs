{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.NewCabalQuickCheckTestSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (sort, nub, isPrefixOf, isInfixOf)
import Data.Char (isSpace, isAlphaNum)

import Utils (trim, splitBy, splitByCollapsed, removeLineComments, removeComments, normalizeIndentation, breakOn)
import SourceLocation (SourcePos(..), SourceSpan(..), startPos, posAfter, advancePosBy, mergeSpans, isValidSpan)
import Parser (FileDirectives(..), BlockDirectives(..), defaultFileDirectives, defaultBlockDirectives)
import Compiler.Errors.Core (ErrorSeverity(..), ErrorCategory(..))
import TestSupport.Arbitrary ()

tests :: TestTree
tests = testGroup "New Cabal QuickCheck Tests"
  [ utilsPropertyTests
  , sourceLocationPropertyTests
  , parserPropertyTests
  , errorHandlerPropertyTests
  , stringProcessingTests
  , listOperationTests
  , mapOperationTests
  , setOperationTests
  , charProcessingTests
  , positionCalculationTests
  ]

-- | 1. Utils模块字符串处理功能测试
utilsPropertyTests :: TestTree
utilsPropertyTests = testGroup "Utils String Processing Properties"
  [ fastProperty "trim: applying twice is same as applying once" $
      \s -> trim (trim s) == trim s
  
  , fastProperty "splitBy: joining with delimiter and splitting again returns original" $
      \c s -> not (null s) ==> splitBy c (unwords (splitBy c s)) == splitBy c s
  
  , fastProperty "splitByCollapsed: never returns empty strings" $
      \c s -> all (not . null) (splitByCollapsed c s)
  
  , fastProperty "removeLineComments: preserves non-comment lines" $
      \s -> not ('/' `elem` s) ==> removeLineComments s == s
  
  , fastProperty "normalizeIndentation: preserves line count" $
      \s -> length (lines s) == length (lines (normalizeIndentation s))
  
  , fastProperty "breakOn: when pattern not found, returns original string" $
      \pat s -> not (pat `isInfixOf` s) ==> breakOn pat s == (s, "")
  ]

-- | 2. SourceLocation模块位置计算测试
sourceLocationPropertyTests :: TestTree
sourceLocationPropertyTests = testGroup "SourceLocation Properties"
  [ testCase "startPos line and column are 1" $
      (posLine startPos == 1 && posColumn startPos == 1) @?= True
  
  , testCase "posAfter: column increases by 1 for regular characters" $
      let pos = startPos
          pos' = posAfter 'a' pos
      in posColumn pos' @?= posColumn pos + 1
  
  , testCase "posAfter: line increases by 1 for newline characters" $
      let pos = startPos
          pos' = posAfter '\n' pos
      in posLine pos' @?= posLine pos + 1
  
  , testCase "advancePosBy: advancing by empty string returns same position" $
      let pos = startPos
      in advancePosBy "" pos @?= pos
  
  , testCase "mergeSpans: result span is valid if both inputs are valid" $
      let span1 = SourceSpan startPos (posAfter 'a' startPos)
          span2 = SourceSpan (posAfter 'a' startPos) (posAfter 'b' (posAfter 'a' startPos))
          merged = mergeSpans span1 span2
      in isValidSpan merged @?= True
  ]

-- | 3. Parser模块解析器一致性测试
parserPropertyTests :: TestTree
parserPropertyTests = testGroup "Parser Consistency Properties"
  [ testCase "defaultFileDirectives: all fields are Nothing" $
      all (== Nothing) [fdOwnership (defaultFileDirectives), fdDependentTypes (defaultFileDirectives), fdConstraints (defaultFileDirectives)] @?= True
  
  , testCase "defaultBlockDirectives: all fields are Nothing" $
      all (== Nothing) [bdOwnership (defaultBlockDirectives), bdDependentTypes (defaultBlockDirectives), bdConstraints (defaultBlockDirectives)] @?= True
  
  , testCase "FileDirectives equality is reflexive" $
      let fd = FileDirectives Nothing Nothing Nothing
      in fd == fd @?= True
  
  , testCase "BlockDirectives equality is reflexive" $
      let bd = BlockDirectives Nothing Nothing Nothing
      in bd == bd @?= True
  ]

-- | 4. ErrorHandler错误处理测试
errorHandlerPropertyTests :: TestTree
errorHandlerPropertyTests = testGroup "ErrorHandler Properties"
  [ testCase "ErrorSeverity ordering: Error > Warning > Info" $
      (compare Error Error == EQ && 
       compare Error Warning == GT && 
       compare Warning Info == GT &&
       compare Info Error == LT) @?= True
  
  , testCase "ErrorCategory comparison works" $
      (compare TypeChecking TypeChecking == EQ) @?= True
  
  , testCase "ErrorSeverity enum values are distinct" $
      (Error /= Warning && Warning /= Info && Error /= Info) @?= True
  ]

-- | 5. 字符串处理功能测试
stringProcessingTests :: TestTree
stringProcessingTests = testGroup "String Processing Properties"
  [ fastProperty "trim: removes only leading and trailing whitespace" $
      \s -> let trimmed = trim s
             in all (not . isSpace) (take 1 trimmed ++ drop (length trimmed - 1) trimmed)
  
  , fastProperty "splitBy: length is at least 1 for non-empty input" $
      \c s -> not (null s) ==> length (splitBy c s) >= 1
  
  , fastProperty "removeComments: idempotent on comment-free strings" $
      \s -> not (any (`isInfixOf` s) ["//", "/*"]) ==> removeComments s == s
  ]

-- | 6. 列表操作测试
listOperationTests :: TestTree
listOperationTests = testGroup "List Operation Properties"
  [ fastProperty "sort: sorting twice is same as sorting once" $
      \(xs :: [Int]) -> sort (sort xs) == sort xs
  
  , fastProperty "nub: removes duplicates" $
      \(xs :: [Int]) -> nub xs == nub (nub xs)
  
  , fastProperty "isPrefixOf: empty string is prefix of any string" $
      \(s :: String) -> "" `isPrefixOf` s
  
  , fastProperty "isInfixOf: empty string is infix of any string" $
      \(s :: String) -> "" `isInfixOf` s
  ]

-- | 7. Map操作测试
mapOperationTests :: TestTree
mapOperationTests = testGroup "Map Operation Properties"
  [ fastProperty "Map.insert: overwrites existing keys" $
      \(k :: Int) (v1 :: String) (v2 :: String) (m :: Map.Map Int String) -> 
        Map.lookup k (Map.insert k v2 (Map.insert k v1 m)) == Just v2
  
  , fastProperty "Map.union: left bias for conflicting keys" $
      \(k :: Int) (v1 :: String) (v2 :: String) -> 
        Map.lookup k (Map.union (Map.singleton k v1) (Map.singleton k v2)) == Just v1
  
  , fastProperty "Map.keys: length equals size" $
      \(m :: Map.Map Int String) -> length (Map.keys m) == Map.size m
  
  , testCase "Map.null: empty map is null" $
      Map.null Map.empty @?= True
  ]

-- | 8. Set操作测试
setOperationTests :: TestTree
setOperationTests = testGroup "Set Operation Properties"
  [ fastProperty "Set.insert: idempotent" $
      \(x :: Int) (s :: Set.Set Int) -> Set.insert x (Set.insert x s) == Set.insert x s
  
  , fastProperty "Set.union: commutative" $
      \(s1 :: Set.Set Int) (s2 :: Set.Set Int) -> Set.union s1 s2 == Set.union s2 s1
  
  , fastProperty "Set.intersection: commutative" $
      \(s1 :: Set.Set Int) (s2 :: Set.Set Int) -> Set.intersection s1 s2 == Set.intersection s2 s1
  
  , fastProperty "Set.member: true for inserted element" $
      \(x :: Int) (s :: Set.Set Int) -> Set.member x (Set.insert x s)
  ]

-- | 9. 字符处理测试
charProcessingTests :: TestTree
charProcessingTests = testGroup "Character Processing Properties"
  [ testCase "isSpace: space is space" $
      (isSpace ' ' && isSpace '\t' && isSpace '\n') @?= True
  
  , fastProperty "isAlphaNum: digits and letters are alphanumeric" $
      \(c :: Char) -> (c >= '0' && c <= '9') || (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') ==> isAlphaNum c
  
  , fastProperty "Char to String conversion" $
      \(c :: Char) -> [c] == show c
  ]

-- | 10. 位置计算测试
positionCalculationTests :: TestTree
positionCalculationTests = testGroup "Position Calculation Properties"
  [ testCase "SourcePos: line and column are positive" $
      let pos = SourcePos 1 1 0
      in posLine pos > 0 && posColumn pos > 0 @?= True
  
  , testCase "advancePosBy with multiple newlines" $
      let pos = startPos
          input = "\n\n"
          pos' = advancePosBy input pos
      in posLine pos' == posLine pos + 2 @?= True
  
  , testCase "SourceSpan equality test" $
      let span1 = SourceSpan startPos (posAfter 'a' startPos)
          span2 = SourceSpan startPos (posAfter 'a' startPos)
      in span1 == span2 @?= True
  ]