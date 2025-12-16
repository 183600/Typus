#!/usr/bin/env cabal
{- cabal:
build-depends: base, QuickCheck, containers
-}

import Test.QuickCheck
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (sort, nub, group, intercalate, isPrefixOf, isInfixOf, isSuffixOf)
import Data.Char (isSpace, toLower, toUpper)

-- 简单的字符串处理函数
trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

splitBy :: Char -> String -> [String]
splitBy _ [] = [""]
splitBy delim s = case break (== delim) s of
  (a, []) -> [a]
  (a, _:b) -> a : splitBy delim b

splitByCollapsed :: Char -> String -> [String]
splitByCollapsed delim = filter (not . null) . splitBy delim

breakOn :: String -> String -> (String, String)
breakOn needle haystack
  | null needle = ([], haystack)
  | null haystack = (haystack, "")
  | needle `isPrefixOf` haystack = ([], haystack)
  | otherwise = case haystack of
      [] -> (haystack, "")
      (x:xs) -> let (before, after) = breakOn needle xs
                 in (x:before, after)

removeLineComments :: String -> String
removeLineComments = unlines . map (takeWhile (/= '/')) . lines

removeComments :: String -> String
removeComments = removeLineComments

normalizeIndentation :: String -> String
normalizeIndentation = id

-- SourceLocation 类型
data SourcePos = SourcePos Int Int Int deriving (Eq, Show)
data SourceSpan = SourceSpan SourcePos SourcePos deriving (Eq, Show)

posLine :: SourcePos -> Int
posLine (SourcePos l _ _) = l

posColumn :: SourcePos -> Int
posColumn (SourcePos _ c _) = c

posOffset :: SourcePos -> Int
posOffset (SourcePos _ _ o) = o

spanStart :: SourceSpan -> SourcePos
spanStart (SourceSpan start _) = start

spanEnd :: SourceSpan -> SourcePos
spanEnd (SourceSpan _ end) = end

-- Parser Directives 类型
data FileDirectives = FileDirectives 
  { fdOwnership :: Maybe ()
  , fdDependentTypes :: Maybe ()
  , fdConstraints :: Maybe ()
  } deriving (Eq, Show)

data BlockDirectives = BlockDirectives 
  { bdOwnership :: Maybe ()
  , bdDependentTypes :: Maybe ()
  , bdConstraints :: Maybe ()
  } deriving (Eq, Show)

defaultFileDirectives :: FileDirectives
defaultFileDirectives = FileDirectives Nothing Nothing Nothing

defaultBlockDirectives :: BlockDirectives
defaultBlockDirectives = BlockDirectives Nothing Nothing Nothing

-- Type 类型
data Type = TypeName String | TypeFunction [Type] Type | TypeRecord [(String, Type)] | TypeUnion [Type] | UnknownType
  deriving (Eq, Show)

-- 测试属性
prop_trim_idempotent :: String -> Bool
prop_trim_idempotent s = trim (trim s) == trim s

prop_splitBy_preserves_order :: Char -> String -> Bool
prop_splitBy_preserves_order delim s = 
  let parts = splitBy delim s
      joined = intercalate [delim] parts
  in joined == dropWhileEnd (== delim) s
  where
    dropWhileEnd p = reverse . dropWhile p . reverse

prop_map_insert_idempotent :: Int -> String -> Map.Map Int String -> Bool
prop_map_insert_idempotent k v m =
  let m1 = Map.insert k v m
      m2 = Map.insert k v m1
  in m1 == m2

prop_set_union_commutative :: Set.Set Int -> Set.Set Int -> Bool
prop_set_union_commutative s1 s2 = Set.union s1 s2 == Set.union s2 s1

prop_sort_idempotent :: [Int] -> Bool
prop_sort_idempotent xs = let sorted = sort xs in sort sorted == sorted

prop_reverse_twice_identity :: [Int] -> Bool
prop_reverse_twice_identity xs = reverse (reverse xs) == xs

prop_isPrefixOf_reflexive :: String -> Bool
prop_isPrefixOf_reflexive s = s `isPrefixOf` s

prop_isSuffixOf_reflexive :: String -> Bool
prop_isSuffixOf_reflexive s = s `isSuffixOf` s

prop_isInfixOf_reflexive :: String -> Bool
prop_isInfixOf_reflexive s = s `isInfixOf` s

prop_type_reflexive :: String -> Bool
prop_type_reflexive s = 
  let t = TypeName s
  in t == t

main :: IO ()
main = do
  putStrLn "Running QuickCheck tests..."
  quickCheck prop_trim_idempotent
  quickCheck prop_splitBy_preserves_order
  quickCheck prop_map_insert_idempotent
  quickCheck prop_set_union_commutative
  quickCheck prop_sort_idempotent
  quickCheck prop_reverse_twice_identity
  quickCheck prop_isPrefixOf_reflexive
  quickCheck prop_isSuffixOf_reflexive
  quickCheck prop_isInfixOf_reflexive
  quickCheck prop_type_reflexive
  putStrLn "All tests completed!"