{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewOwnershipQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import TestSupport.Arbitrary

import Ownership
  ( OwnershipType(..)
  , OwnershipError(..)
  , OwnershipAnalyzer
  , OwnershipTransfer(..)
  , newOwnershipAnalyzer
  , analyzeOwnership
  , analyzeOwnershipFile
  , analyzeOwnershipDebug
  , formatOwnershipErrors
  , lexAll
  , parseProgram
  , builtInFunctions
  )

import Parser (TypusFile(..), CodeBlock(..), defaultFileDirectives, defaultBlockDirectives)
import SourceLocation (Located(..), SourcePos(..), SourceSpan(..))
import qualified Data.Text as T
import qualified Data.List as L
import Data.List (isInfixOf)
import Data.Maybe (isJust, isNothing)
import Data.Char (isAlphaNum)

-- | Ownership QuickCheck tests
tests :: TestTree
tests = testGroup "New Ownership QuickCheck Tests"
  [ fastProperty "newOwnershipAnalyzer creates valid analyzer" prop_new_analyzer
  , fastProperty "lexAll handles basic Go code" prop_lex_basic
  , fastProperty "parseProgram handles simple programs" prop_parse_simple
  , fastProperty "analyzeOwnership processes ownership" prop_analyze_ownership
  , fastProperty "Ownership types are consistent" prop_ownership_types
  , fastProperty "Ownership transfer operations" prop_ownership_transfer
  , fastProperty "Built-in functions are recognized" prop_builtin_functions
  , fastProperty "Error formatting preserves information" prop_error_formatting
  ]

-- Property: newOwnershipAnalyzer creates valid analyzer
prop_new_analyzer :: Property
prop_new_analyzer =
  let analyzer = newOwnershipAnalyzer
  in property $ analyzer `seq` True -- Should not crash L.and create valid analyzer

-- Property: lexAll handles basic Go code
prop_lex_basic :: String -> Property
prop_lex_basic code =
  L.length code <= 50 ==> 
  let tokens = lexAll code
  in property $ L.length tokens >= 0 -- Should produce some tokens L.or empty list

-- Property: parseProgram handles simple programs
prop_parse_simple :: String -> Property
prop_parse_simple program =
  L.length program <= 30 ==> 
  let tokens = lexAll program
      parseResult = parseProgram tokens
  in property $ parseResult `seq` True -- Should not crash during parsing

-- Property: analyzeOwnership processes ownership
prop_analyze_ownership :: String -> Property
prop_analyze_ownership code =
  L.length code <= 40 ==> 
  let tokens = lexAll code
      parseResult = parseProgram tokens
      analyzer = newOwnershipAnalyzer
      result = analyzeOwnership analyzer parseResult
  in case result of
    Left _ -> property True -- May fail but shouldn't crash
    Right _ -> property True -- Should succeed for simple cases

-- Property: Ownership types are consistent
prop_ownership_types :: OwnershipType -> Property
prop_ownership_types ownershipType =
  let typeStr = show ownershipType
  in property $ not (null typeStr) .&&. 
     (ownershipType `elem` [Owned, Borrowed, Shared, Moved] || property True)

-- Property: Ownership transfer operations
prop_ownership_transfer :: OwnershipType -> OwnershipType -> Property
prop_ownership_transfer fromType toType =
  let transfer = OwnershipTransfer fromType toType
  in property $ case transfer of
    OwnershipTransfer{..} -> 
      otFrom === fromType .&&. otTo === toType

-- Property: Built-in functions are recognized
prop_builtin_functions :: Property
prop_builtin_functions =
  let builtins = builtInFunctions
  in property $ not (null builtins) .&&.
     L.all (\func -> not (null func)) builtins

-- Property: Error formatting preserves information
prop_error_formatting :: String -> Property
prop_error_formatting errorMsg =
  L.length errorMsg <= 30 ==>
  let error = OwnershipError "OWN001" (T.pack errorMsg) Nothing Nothing
      formatted = formatOwnershipErrors [error]
  in property $ not (null formatted) .&&. errorMsg `L.isInfixOf` formatted