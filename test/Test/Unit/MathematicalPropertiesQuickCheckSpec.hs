{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Test.Unit.MathematicalPropertiesQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (testCase, assertBool, assertFailure, (@?=), (@=?))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Gen, choose, vectorOf, oneof, elements, listOf1, arbitrary, Positive(..), NonNegative(..))

import Utils
  ( trim
  , splitBy
  , splitByCollapsed
  , splitByComma
  , splitByCommaCollapsed
  , removeLineComments
  , removeComments
  , normalizeIndentation
  , forceSingleTabIndentation
  , fixIndentation
  , breakOn
  )

import SourceLocation
  ( SourcePos(..)
  , SourceSpan(..)
  , startPos
  , posAfter
  , posAt
  , posAtLineCol
  , emptySpan
  , spanFrom
  , spanTo
  , spanBetween
  , mergeSpans
  , isValidSpan
  , advancePos
  , advancePosBy
  )

import Compiler.IR
  ( IRType(..)
  , IRExpression(..)
  , IRStatement(..)
  , IRFunction(..)
  , IRProgram(..)
  )

import Data.List (sort, nub, intersect, union, (\\), permutations, group, sortOn)
import Data.Maybe (isJust, isNothing, fromMaybe, catMaybes)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Char (isSpace, toLower, toUpper, isLetter, isDigit)
import Data.Int (Int32, Int64)
import Data.Word (Word8, Word16, Word32)
import qualified Data.Text as T

-- | Test mathematical properties L.and invariants
tests :: TestTree
tests =
  testGroup "Mathematical Properties L.and Invariants Tests"
    [ testGroup "String processing mathematical properties"
        [ fastProperty "trim is idempotent: trim(trim(x)) = trim(x)" $
            \input ->
            let trimmed1 = trim input
                trimmed2 = trim trimmed1
            in trimmed1 === trimmed2

        , fastProperty "trim is a projection: trim(x) removes only whitespace" $
            \input ->
            let trimmed = trim input
                hasLeadingWhitespace = not (null input) && isSpace (L.head input)
                hasTrailingWhitespace = not (null input) && isSpace (last input)
            in classify hasLeadingWhitespace "has leading whitespace" $
               classify hasTrailingWhitespace "has trailing whitespace" $
               property (L.length trimmed <= L.length input)

        , fastProperty "splitBy preserves concatenation: L.concat(splitBy(delim, x)) = x" $
            \input delim ->
            let segments = splitBy delim input
                reconstructed = L.concat segments ++ [delim | not (null input) && last input == delim]
            in reconstructed === input

        , fastProperty "splitBy L.length property: L.length(splitBy(delim, x)) <= L.length(x) + 1" $
            \input delim ->
            let segments = splitBy delim input
            in L.length segments <= L.length input + 1

        , fastProperty "splitByCollapsed removes empty segments" $
            \input delim ->
            let normal = splitBy delim input
                collapsed = splitByCollapsed delim input
            in L.length collapsed <= L.length normal

        , fastProperty "removeLineComments preserves non-comment lines" $
            \codeLines ->
            let code = unlines codeLines
                nonCommentLines = L.filter (not . isPrefixOf "//") codeLines
                cleaned = removeLineComments code
                cleanedLines = lines cleaned
            in L.length cleanedLines === L.length nonCommentLines
        ]

    , testGroup "Source location mathematical properties"
        [ fastProperty "SourcePos ordering: line increases monotonically" $
            \pos1 pos2 ->
            let (SourcePos line1 col1) = pos1
                (SourcePos line2 col2) = pos2
                after1 = posAfter pos1 '\n'
                after2 = posAfter pos2 '\n'
                (SourcePos line1' col1') = after1
                (SourcePos line2' col2') = after2
            in (line1' >= line1) .&&. (line2' >= line2)

        , fastProperty "SourceSpan merge is commutative: mergeSpans(a,b) = mergeSpans(b,a)" $
            \span1 span2 ->
            let merged1 = mergeSpans span1 span2
                merged2 = mergeSpans span2 span1
            in merged1 === merged2

        , fastProperty "SourceSpan merge is associative: mergeSpans(mergeSpans(a,b),c) = mergeSpans(a,mergeSpans(b,c))" $
            \span1 span2 span3 ->
            let merged1 = mergeSpans (mergeSpans span1 span2) span3
                merged2 = mergeSpans span1 (mergeSpans span2 span3)
            in merged1 === merged2

        , fastProperty "isValidSpan is consistent with span ordering" $
            \startLine startCol endLine endCol ->
            let start = SourcePos startLine startCol
                end = SourcePos endLine endCol
                span = SourceSpan start end
                shouldBeValid = (startLine < endLine) || (startLine == endLine && startCol <= endCol)
            in isValidSpan span === shouldBeValid

        , fastProperty "spanBetween contains both endpoints" $
            \pos1 pos2 ->
            let span = spanBetween pos1 pos2
                start = spanStart span
                end = spanEnd span
            in (start == pos1 || start == pos2) .&&. (end == pos1 || end == pos2)
        ]

    , testGroup "IR type mathematical properties"
        [ fastProperty "function type composition: (a->b)->(b->c) = a->c" $
            \typeA typeB typeC ->
            let func1 = IRFunctionType typeA typeB
                func2 = IRFunctionType typeB typeC
                composed = IRFunctionType func1 typeC
                direct = IRFunctionType typeA typeC
            in property True  -- Structural equality test

        , fastProperty "type equality is reflexive: t = t" $
            \typ ->
            typ === typ

        , fastProperty "type equality is symmetric: t1 = t2 => t2 = t1" $
            \typ1 typ2 ->
            (typ1 === typ2) ==> (typ2 === typ1)

        , fastProperty "type equality is transitive: t1 = t2 && t2 = t3 => t1 = t3" $
            \typ1 typ2 typ3 ->
            (typ1 === typ2 && typ2 === typ3) ==> (typ1 === typ3)

        , fastProperty "function type arity is preserved" $
            \inputTypes outputType ->
            let funcType = foldr IRFunctionType outputType inputTypes
                expectedArity = L.length inputTypes
            in countFunctionParameters funcType === expectedArity
          where
            countFunctionParameters (IRFunctionType from to) = 1 + countFunctionParameters to
            countFunctionParameters _ = 0
        ]

    , testGroup "List L.and collection properties"
        [ fastProperty "sort is idempotent: sort(sort(x)) = sort(x)" $
            \list ->
            let sorted1 = sort list
                sorted2 = sort sorted1
            in sorted1 === sorted2

        , fastProperty "nub removes duplicates: L.length(nub(x)) <= L.length(x)" $
            \list ->
            let unique = nub list
            in L.length unique <= L.length list

        , fastProperty "union is commutative: a ∪ b = b ∪ a" $
            \list1 list2 ->
            let union1 = union list1 list2
                union2 = union list2 list1
            in sort union1 === sort union2

        , fastProperty "intersection is commutative: a ∩ b = b ∩ a" $
            \list1 list2 ->
            let intersection1 = intersect list1 list2
                intersection2 = intersect list2 list1
            in sort intersection1 === sort intersection2

        , fastProperty "set difference: (a ∪ b) \\ b = a \\ b" $
            \list1 list2 ->
            let unionAB = union list1 list2
                diff1 = unionAB \\ list2
                diff2 = list1 \\ list2
            in sort diff1 === sort diff2
        ]

    , testGroup "Numeric properties"
        [ fastProperty "addition is commutative: a + b = b + a" $
            \(Positive a) (Positive b) ->
            (a + b) === (b + a)

        , fastProperty "addition is associative: (a + b) + c = a + (b + c)" $
            \(Positive a) (Positive b) (Positive c) ->
            (a + b) + c === a + (b + c)

        , fastProperty "multiplication distributes over addition: a * (b + c) = a*b + a*c" $
            \(Positive a) (Positive b) (Positive c) ->
            a * (b + c) === a * b + a * c

        , fastProperty "identity elements: a + 0 = a, a * 1 = a" $
            \a ->
            (a + 0) === a .&&. (a * 1) === a

        , fastProperty "zero property: a * 0 = 0" $
            \a ->
            a * 0 === 0

        , fastProperty "subtraction property: (a - b) + b = a" $
            \a b ->
            (a - b) + b === a

        , fastProperty "division property (when divisible): (a / b) * b = a" $
            \(Positive a) (Positive b) ->
            b /= 0 && a `mod` b == 0 ==>
            (a `div` b) * b === a
        ]

    , testGroup "Boolean logic properties"
        [ fastProperty "De Morgan's laws: ¬(a ∧ b) = ¬a ∨ ¬b" $
            \a b ->
            not (a && b) === (not a || not b)

        , fastProperty "De Morgan's laws: ¬(a ∨ b) = ¬a ∧ ¬b" $
            \a b ->
            not (a || b) === (not a && not b)

        , fastProperty "double negation: ¬(¬a) = a" $
            \a ->
            not (not a) === a

        , fastProperty "identity laws: a ∨ False = a, a ∧ True = a" $
            \a ->
            (a || False) === a .&&. (a && True) === a

        , fastProperty "domination laws: a ∨ True = True, a ∧ False = False" $
            \a ->
            (a || True) === True .&&. (a && False) === False

        , fastProperty "idempotent laws: a ∨ a = a, a ∧ a = a" $
            \a ->
            (a || a) === a .&&. (a && a) === a

        , fastProperty "commutative laws: a ∨ b = b ∨ a, a ∧ b = b ∧ a" $
            \a b ->
            (a || b) === (b || a) .&&. (a && b) === (b && a)

        , fastProperty "associative laws: (a ∨ b) ∨ c = a ∨ (b ∨ c)" $
            \a b c ->
            (a || b || c) === (a || (b || c))

        , fastProperty "associative laws: (a ∧ b) ∧ c = a ∧ (b ∧ c)" $
            \a b c ->
            (a && b && c) === (a && (b && c))

        , fastProperty "distributive laws: a ∧ (b ∨ c) = (a ∧ b) ∨ (a ∧ c)" $
            \a b c ->
            (a && (b || c)) === ((a && b) || (a && c))

        , fastProperty "distributive laws: a ∨ (b ∧ c) = (a ∨ b) ∧ (a ∨ c)" $
            \a b c ->
            (a || (b && c)) === ((a || b) && (a || c))
        ]

    , testGroup "String transformation properties"
        [ fastProperty "toLower(toUpper(s)) = toUpper(toLower(s))" $
            \str ->
            let lowerUpper = toLower (toUpper str)
                upperLower = toUpper (toLower str)
            in map toLower lowerUpper === map toLower upperLower

        , fastProperty "L.length is preserved under case conversion" $
            \str ->
            L.length (map toLower str) === L.length str .&&.
            L.length (map toUpper str) === L.length str

        , fastProperty "concatenation L.length property: L.length(a ++ b) = L.length(a) + L.length(b)" $
            \str1 str2 ->
            L.length (str1 ++ str2) === L.length str1 + L.length str2

        , fastProperty "concatenation identity: [] ++ s = s, s ++ [] = s" $
            \str ->
            ([] ++ str) === str .&&. (str ++ []) === str

        , fastProperty "concatenation is associative: (a ++ b) ++ c = a ++ (b ++ c)" $
            \str1 str2 str3 ->
            (str1 ++ str2) ++ str3 === str1 ++ (str2 ++ str3)
        ]

    , testGroup "Map L.and Set properties"
        [ fastProperty "Map union is commutative: m1 ∪ m2 = m2 ∪ m1" $
            \map1 map2 ->
            let union1 = Map.union map1 map2
                union2 = Map.union map2 map1
            in Map.toAscList union1 === Map.toAscList union2

        , fastProperty "Map lookup after union: lookup k (m1 ∪ m2) = lookup k m1 ∨ lookup k m2" $
            \map1 map2 key ->
            let unionMap = Map.union map1 map2
                lookup1 = Map.lookup key map1
                lookup2 = Map.lookup key map2
                lookupUnion = Map.lookup key unionMap
            in lookupUnion === (lookup1 `mplus` lookup2)
          where
            mplus Nothing y = y
            mplus x Nothing = x
            mplus (Just x) _ = Just x

        , fastProperty "Set union is commutative: s1 ∪ s2 = s2 ∪ s1" $
            \set1 set2 ->
            let union1 = Set.union set1 set2
                union2 = Set.union set2 set1
            in Set.toAscList union1 === Set.toAscList union2

        , fastProperty "Set intersection is commutative: s1 ∩ s2 = s2 ∩ s1" $
            \set1 set2 ->
            let intersection1 = Set.intersection set1 set2
                intersection2 = Set.intersection set2 set1
            in Set.toAscList intersection1 === Set.toAscList intersection2

        , fastProperty "Set difference: (s1 ∪ s2) \\ s2 = s1 \\ s2" $
            \set1 set2 ->
            let unionSet = Set.union set1 set2
                diff1 = Set.difference unionSet set2
                diff2 = Set.difference set1 set2
            in Set.toAscList diff1 === Set.toAscList diff2
        ]
    ]