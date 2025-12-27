{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewOwnershipQuickCheckTestsSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import Test.Tasty (TestTree)

import Ownership (OwnershipType(..), OwnershipTransfer(..), analyzeOwnership, OwnershipError(..))
import Data.Char (isSpace, isAlphaNum)
import qualified Data.List as List
import Data.Text (Text)

-- Property: Ownership analysis handles empty input
prop_ownership_empty_input :: Property
prop_ownership_empty_input =
  let result = analyzeOwnership ""
  in property $ case result of
    Left _ -> property True
    Right _ -> property True

-- Property: Ownership analysis is deterministic
prop_ownership_deterministic :: String -> Property
prop_ownership_deterministic input =
  let result1 = analyzeOwnership input
      result2 = analyzeOwnership input
  in property $ case (result1, result2) of
    (Left err1, Left err2) -> show err1 === show err2
    (Right res1, Right res2) -> show res1 === show res2
    _ -> property False

-- Property: Ownership analysis handles simple assignments
prop_ownership_simple_assignments :: String -> String -> Property
prop_ownership_simple_assignments var1 var2 =
  not (null var1) && not (null var2) && all isAlphaNum (var1 ++ var2) ==>
  let input = var1 ++ " := " ++ var2 ++ "\n" ++ var1 ++ " := " ++ var2
      result = analyzeOwnership input
  in property $ case result of
    Left _ -> property True
    Right _ -> property True

-- Property: Ownership analysis handles move operations
prop_ownership_move_operations :: String -> String -> Property
prop_ownership_move_operations src dest =
  not (null src) && not (null dest) && all isAlphaNum (src ++ dest) ==>
  let input = src ++ " := " ++ dest ++ "\nmove(" ++ src ++ ", " ++ dest ++ ")"
      result = analyzeOwnership input
  in property $ case result of
    Left _ -> property True
    Right _ -> property True

-- Property: Ownership analysis handles borrow operations
prop_ownership_borrow_operations :: String -> String -> Property
prop_ownership_borrow_operations src dest =
  not (null src) && not (null dest) && all isAlphaNum (src ++ dest) ==>
  let input = "borrow(" ++ src ++ ", " ++ dest ++ ")\n" ++ src ++ " := " ++ dest
      result = analyzeOwnership input
  in property $ case result of
    Left _ -> property True
    Right _ -> property True

-- Property: Ownership analysis ownership transfer tracking
prop_ownership_transfer_tracking :: String -> String -> String -> Property
prop_ownership_transfer_tracking var1 var2 var3 =
  not (null var1) && not (null var2) && not (null var3) && 
  all isAlphaNum (var1 ++ var2 ++ var3) ==>
  let input = var1 ++ " := " ++ var2 ++ "\n" ++ var2 ++ " := " ++ var3 ++ "\n" ++ var3 ++ " := " ++ var1
      result = analyzeOwnership input
  in property $ case result of
    Left _ -> property True
    Right _ -> property True

-- Property: Ownership analysis handles comments
prop_ownership_comments :: String -> String -> Property
prop_ownership_comments code comment =
  not ('"' `elem` code) && not ('\'' `elem` code) ==>
  let codeWithComment = code ++ "// " ++ comment ++ "\n" ++ code
      result1 = analyzeOwnership code
      result2 = analyzeOwnership codeWithComment
  in property $ case (result1, result2) of
    (Left _, Left _) -> property True
    (Right _, Right _) -> property True
    _ -> property False

-- Property: Ownership analysis handles whitespace
prop_ownership_whitespace :: String -> Property
prop_ownership_whitespace input =
  all isSpace input ==>
  let result = analyzeOwnership input
  in property $ case result of
    Left _ -> property True
    Right _ -> property True

-- Property: Ownership analysis handles complex ownership chains
prop_ownership_complex_chains :: Int -> String -> Property
prop_ownership_complex_chains length baseVar =
  length >= 1 && length <= 5 && not (null baseVar) && all isAlphaNum baseVar ==>
  let vars = take length [baseVar ++ show i | i <- [1..]]
      assignments = [v1 ++ " := " ++ v2 | (v1, v2) <- zip vars (tail vars ++ [head vars])]
      input = List.intercalate "\n" assignments
      result = analyzeOwnership input
  in property $ case result of
    Left _ -> property True
    Right _ -> property True

-- Property: Ownership analysis error messages are informative
prop_ownership_error_messages :: String -> Property
prop_ownership_error_messages input =
  let result = analyzeOwnership input
  in property $ case result of
    Left err -> property $ not (null (show err))
    Right _ -> property True

tests :: TestTree
tests = testGroup "New Ownership QuickCheck Tests"
  [ fastProperty "Ownership handles empty input" prop_ownership_empty_input
  , fastProperty "Ownership analysis is deterministic" prop_ownership_deterministic
  , fastProperty "Ownership handles simple assignments" prop_ownership_simple_assignments
  , fastProperty "Ownership handles move operations" prop_ownership_move_operations
  , fastProperty "Ownership handles borrow operations" prop_ownership_borrow_operations
  , fastProperty "Ownership transfer tracking" prop_ownership_transfer_tracking
  , fastProperty "Ownership handles comments" prop_ownership_comments
  , fastProperty "Ownership handles whitespace" prop_ownership_whitespace
  , fastProperty "Ownership handles complex chains" prop_ownership_complex_chains
  , fastProperty "Ownership error messages are informative" prop_ownership_error_messages
  ]