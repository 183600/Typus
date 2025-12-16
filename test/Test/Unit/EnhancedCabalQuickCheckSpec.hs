{-# LANGUAGE CPP #-}

module Test.Unit.EnhancedCabalQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck
import qualified Data.Map as Map
import Data.List (sort, nub, isPrefixOf)

import Utils (trim, splitBy, removeLineComments, breakOn)
import SourceLocation (SourcePos(..), SourceSpan(..), posLine, posColumn, posOffset)
import Parser (FileDirectives(..), BlockDirectives(..), defaultFileDirectives, defaultBlockDirectives)
import Compiler.TypeChecker (Type(..), TypeEnv(..), unifyTypes, areTypesCompatible, typesEqual)

tests :: TestTree
tests = testGroup "Enhanced Cabal QuickCheck Tests"
  [ parserTests
  , typeCheckerTests
  , utilsTests
  ]

-- ============================================================================
-- Parser Tests (3 properties)
-- ============================================================================

parserTests :: TestTree
parserTests = testGroup "Parser Properties"
  [ fastProperty "defaultFileDirectives has all fields as Nothing" prop_defaultFileDirectives_structure
  , fastProperty "defaultBlockDirectives has all fields as Nothing" prop_defaultBlockDirectives_structure
  , fastProperty "FileDirectives equality is reflexive" prop_fileDirectives_reflexive
  ]

prop_defaultFileDirectives_structure :: Property
prop_defaultFileDirectives_structure =
  let fd = defaultFileDirectives
  in conjoin
    [ counterexample "fdOwnership should be Nothing" $ fdOwnership fd === Nothing
    , counterexample "fdDependentTypes should be Nothing" $ fdDependentTypes fd === Nothing
    , counterexample "fdConstraints should be Nothing" $ fdConstraints fd === Nothing
    ]

prop_defaultBlockDirectives_structure :: Property
prop_defaultBlockDirectives_structure =
  let bd = defaultBlockDirectives
  in conjoin
    [ counterexample "bdOwnership should be Nothing" $ bdOwnership bd === Nothing
    , counterexample "bdDependentTypes should be Nothing" $ bdDependentTypes bd === Nothing
    , counterexample "bdConstraints should be Nothing" $ bdConstraints bd === Nothing
    ]

prop_fileDirectives_reflexive :: Property
prop_fileDirectives_reflexive =
  let fd = defaultFileDirectives
  in fd === fd

-- ============================================================================
-- TypeChecker Tests (3 properties)
-- ============================================================================

typeCheckerTests :: TestTree
typeCheckerTests = testGroup "TypeChecker Properties"
  [ fastProperty "typesEqual is reflexive" prop_typesEqual_reflexive
  , fastProperty "typesEqual is symmetric" prop_typesEqual_symmetric
  , fastProperty "TypeName constructor preserves name" prop_typename_preserves_name
  ]

prop_typesEqual_reflexive :: Property
prop_typesEqual_reflexive = forAll genSimpleType $ \t ->
  counterexample ("Type: " ++ show t) $
    typesEqual t t === True

prop_typesEqual_symmetric :: Property
prop_typesEqual_symmetric = forAll genTwoSimpleTypes $ \(t1, t2) ->
  counterexample ("Type1: " ++ show t1 ++ ", Type2: " ++ show t2) $
    typesEqual t1 t2 === typesEqual t2 t1

prop_typename_preserves_name :: Property
prop_typename_preserves_name = forAll genValidTypeName $ \name ->
  let t = TypeName name
  in counterexample ("Expected TypeName with: " ++ name) $
       case t of
         TypeName n -> n === name
         _ -> property False
  where
    genValidTypeName :: Gen String
    genValidTypeName = do
      firstChar <- elements (['a'..'z'] ++ ['A'..'Z'])
      rest <- listOf $ elements (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_")
      return (firstChar : rest)

genSimpleType :: Gen Type
genSimpleType = oneof
  [ TypeName <$> elements ["int", "string", "bool", "float64"]
  , pure UnknownType
  ]

genTwoSimpleTypes :: Gen (Type, Type)
genTwoSimpleTypes = do
  t1 <- genSimpleType
  t2 <- genSimpleType
  return (t1, t2)

-- ============================================================================
-- Utils Tests (4 properties)
-- ============================================================================

utilsTests :: TestTree
utilsTests = testGroup "Utils Properties"
  [ fastProperty "trim is idempotent" prop_trim_idempotent
  , fastProperty "trim preserves non-whitespace content" prop_trim_preserves_content
  , fastProperty "splitBy preserves element count" prop_splitBy_count
  , fastProperty "breakOn correctly splits on needle" prop_breakOn_correctness
  ]

prop_trim_idempotent :: String -> Property
prop_trim_idempotent s =
  let trimmed = trim s
  in counterexample ("Original: " ++ show s ++ ", Trimmed: " ++ show trimmed) $
       trim trimmed === trimmed

prop_trim_preserves_content :: String -> Property
prop_trim_preserves_content s =
  let trimmed = trim s
      nonWs = filter (not . isWs) s
      trimmedNonWs = filter (not . isWs) trimmed
  in counterexample ("Original non-ws: " ++ show nonWs ++ ", Trimmed non-ws: " ++ show trimmedNonWs) $
       nonWs === trimmedNonWs
  where
    isWs c = c `elem` " \t\n\r\f\v"

prop_splitBy_count :: Char -> String -> Property
prop_splitBy_count delim s =
  delim /= '\0' ==>
  let parts = splitBy delim s
      delimCount = length $ filter (== delim) s
      expectedParts = delimCount + 1
  in counterexample ("String: " ++ show s ++ ", Delim: " ++ show delim ++ ", Parts: " ++ show (length parts) ++ ", Expected: " ++ show expectedParts) $
       length parts === expectedParts

prop_breakOn_correctness :: Property
prop_breakOn_correctness = forAll genNeedleHaystack $ \(needle, haystack) ->
  let (before, after) = breakOn needle haystack
  in counterexample ("Needle: " ++ show needle ++ ", Haystack: " ++ show haystack ++ ", Before: " ++ show before ++ ", After: " ++ show after) $
       if needle `isPrefixOf` haystack
       then before === "" .&&. after === drop (length needle) haystack
       else if needle `isInfixOfCustom` haystack
            then (before ++ needle ++ after) === haystack
            else before === haystack .&&. after === ""
  where
    isInfixOfCustom :: String -> String -> Bool
    isInfixOfCustom [] _ = True
    isInfixOfCustom _ [] = False
    isInfixOfCustom needle haystack@(_:hs)
      | needle `isPrefixOf` haystack = True
      | otherwise = isInfixOfCustom needle hs

genNeedleHaystack :: Gen (String, String)
genNeedleHaystack = do
  needle <- elements ["x", "ab", "123", "::"]
  prefix <- arbitrary `suchThat` (\s -> not (needle `isPrefixOf` s) && length s < 10)
  suffix <- arbitrary `suchThat` (\s -> length s < 10)
  includeNeedle <- arbitrary
  let haystack = if includeNeedle
                 then prefix ++ needle ++ suffix
                 else prefix ++ suffix
  return (needle, haystack)
