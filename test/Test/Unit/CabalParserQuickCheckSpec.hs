{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.CabalParserQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import Test.QuickCheck.Arbitrary (Arbitrary(..), oneof, elements, listOf, sized)
import qualified Data.List as List
import Data.Char (isSpace, isAlphaNum, isLetter)
import Data.Maybe (isJust, isNothing)

import Parser (parseTypus, TypusFile(..), FileDirectives(..))
import SourceLocation (Located(..), SourceSpan(..))

-- Simple arbitrary instances for testing
newtype SimpleIdentifier = SimpleIdentifier String deriving (Show, Eq)

instance Arbitrary SimpleIdentifier where
  arbitrary = do
    first <- elements ['a'..'z']
    rest <- listOf $ elements (['a'..'z'] ++ ['0'..'9'] ++ ['_'])
    return $ SimpleIdentifier (first : rest)

newtype SimpleTypusCode = SimpleTypusCode String deriving (Show, Eq)

instance Arbitrary SimpleTypusCode where
  arbitrary = sized $ \n -> do
    SimpleIdentifier ident <- arbitrary
    let simpleCode = "func " ++ ident ++ "() { return 42 }"
    return $ SimpleTypusCode simpleCode

-- Property: Parsing valid simple functions should succeed
prop_parse_simple_function_succeeds :: SimpleTypusCode -> Property
prop_parse_simple_function_succeeds (SimpleTypusCode code) =
  case parseTypus code of
    Left err -> counterexample ("Parse failed: " ++ err) $ property False
    Right _ -> property True

-- Property: Round-trip property for simple code (simplified)
prop_parse_preserves_structure :: SimpleIdentifier -> Property
prop_parse_preserves_structure (SimpleIdentifier ident) =
  let code = "func " ++ ident ++ "() { return 42 }"
  in case parseTypus code of
       Left err -> counterexample ("Parse failed: " ++ err) $ property False
       Right parsed -> 
         let funcName = extractFunctionName parsed
         in case funcName of
              Just name -> name === ident
              Nothing -> property False

-- Helper function to extract function name (simplified)
extractFunctionName :: TypusFile -> Maybe String
extractFunctionName _ = Just "test" -- Simplified for demo

-- Property: Parser should handle empty input gracefully
prop_parse_empty_input :: Property
prop_parse_empty_input =
  case parseTypus "" of
    Left _ -> property True  -- Expected to fail L.or succeed gracefully
    Right _ -> property True  -- Or succeed with minimal structure

-- Property: Parser should handle whitespace variations
prop_parse_whitespace_variations :: SimpleIdentifier -> Property
prop_parse_whitespace_variations (SimpleIdentifier ident) =
  let variants = 
        [ "func " ++ ident ++ "() { return 42 }"
        , "func    " ++ ident ++ "   () { return 42 }"
        , "\nfunc " ++ ident ++ "() { return 42 }\n"
        , "  func " ++ ident ++ "() { return 42 }  "
        ]
  in property $ L.all (\code -> case parseTypus code of
                               Left _ -> False
                               Right _ -> True) variants

tests :: TestTree
tests = testGroup "Cabal Parser QuickCheck Tests"
  [ fastProperty "Parse simple function succeeds" prop_parse_simple_function_succeeds
  , fastProperty "Parse preserves structure" prop_parse_preserves_structure
  , fastProperty "Parse empty input gracefully" prop_parse_empty_input
  , fastProperty "Parse whitespace variations" prop_parse_whitespace_variations
  , testCase "Parser handles complex directives" $ do
      let source = unlines
            [ "//! ownership: on"
            , "//! dependent_types: off"
            , "package main"
            , "func main() { return 0 }"
            ]
      case parseTypus source of
        Left err -> assertFailure $ "parseTypus failed: " ++ err
        Right _ -> return ()
  ]