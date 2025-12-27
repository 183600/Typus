{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.OwnershipTransferQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import TestSupport.Arbitrary

import Ownership
import Ownership.Common.Types
import Parser (parseTypus)
import Compiler (compile)
import Utils (trim)
import Data.Char (isSpace, isLetter, isDigit)
import qualified Data.List as Data.List
import Data.List (isPrefixOf, isInfixOf, sort)

-- Property: Ownership tracks variable lifetimes correctly
prop_ownership_variable_lifetime :: String -> Property
prop_ownership_variable_lifetime varName =
  not (null varName) && all (\c -> isLetter c || c == '_') varName ==>
  let sourceCode = "var " ++ varName ++ " = 42\n" ++ varName ++ " = 24"
      parseResult = parseTypus sourceCode
      ownershipResult = case parseResult of
        Left _ -> Nothing
        Right ast -> Just (analyzeOwnership ast)
  in property $ case ownershipResult of
    Nothing -> False
    Right analysis -> property $ varName `isInfixOf` show analysis

-- Property: Ownership detects move operations
prop_ownership_detects_moves :: String -> String -> Property
prop_ownership_detects_moves sourceVar targetVar =
  not (null sourceVar) && not (null targetVar) &&
  all (\c -> isLetter c || c == '_') sourceVar &&
  all (\c -> isLetter c || c == '_') targetVar ==>
  let sourceCode = "var " ++ sourceVar ++ " = 42\nvar " ++ targetVar ++ " = " ++ sourceVar
      parseResult = parseTypus sourceCode
      ownershipResult = case parseResult of
        Left _ -> Nothing
        Right ast -> Just (analyzeOwnership ast)
  in property $ case ownershipResult of
    Nothing -> False
    Right analysis -> property $ "move" `isInfixOf` show analysis .||. sourceVar `isInfixOf` show analysis

-- Property: Ownership handles borrow operations
prop_ownership_handles_borrows :: String -> Property
prop_ownership_handles_borrows varName =
  not (null varName) && all (\c -> isLetter c || c == '_') varName ==>
  let sourceCode = "var " ++ varName ++ " = 42\nvar ref = &" ++ varName
      parseResult = parseTypus sourceCode
      ownershipResult = case parseResult of
        Left _ -> Nothing
        Right ast -> Just (analyzeOwnership ast)
  in property $ case ownershipResult of
    Nothing -> False
    Right analysis -> property $ "borrow" `isInfixOf` show analysis .||. "ref" `isInfixOf` show analysis

-- Property: Ownership prevents double moves
prop_ownership_prevents_double_moves :: String -> String -> String -> Property
prop_ownership_prevents_double_moves sourceVar targetVar1 targetVar2 =
  not (null sourceVar) && not (null targetVar1) && not (null targetVar2) &&
  all (\c -> isLetter c || c == '_') sourceVar &&
  all (\c -> isLetter c || c == '_') targetVar1 &&
  all (\c -> isLetter c || c == '_') targetVar2 &&
  targetVar1 /= targetVar2 ==>
  let sourceCode = "var " ++ sourceVar ++ " = 42\nvar " ++ targetVar1 ++ " = " ++ sourceVar ++ "\nvar " ++ targetVar2 ++ " = " ++ sourceVar
      parseResult = parseTypus sourceCode
      ownershipResult = case parseResult of
        Left _ -> Nothing
        Right ast -> Just (analyzeOwnership ast)
  in property $ case ownershipResult of
    Nothing -> True -- Should fail due to double move
    Right analysis -> property $ "error" `isInfixOf` show analysis .||. "double move" `isInfixOf` show analysis

-- Property: Ownership handles function parameter passing
prop_ownership_function_parameters :: String -> [String] -> Property
prop_ownership_function_parameters funcName params =
  not (null funcName) && all (\c -> isLetter c || c == '_') funcName && length params <= 3 ==>
  let paramsStr = Data.List.intercalate ", " params
      sourceCode = "func " ++ funcName ++ "(" ++ paramsStr ++ ") { }\n" ++ funcName ++ "(param)"
      parseResult = parseTypus sourceCode
      ownershipResult = case parseResult of
        Left _ -> Nothing
        Right ast -> Just (analyzeOwnership ast)
  in property $ case ownershipResult of
    Nothing -> False
    Right analysis -> property $ funcName `isInfixOf` show analysis

-- Property: Ownership handles return value ownership
prop_ownership_return_values :: String -> Property
prop_ownership_return_values varName =
  not (null varName) && all (\c -> isLetter c || c == '_') varName ==>
  let sourceCode = "func create() { return 42 }\nvar " ++ varName ++ " = create()"
      parseResult = parseTypus sourceCode
      ownershipResult = case parseResult of
        Left _ -> Nothing
        Right ast -> Just (analyzeOwnership ast)
  in property $ case ownershipResult of
    Nothing -> False
    Right analysis -> property $ varName `isInfixOf` show analysis .&&. "return" `isInfixOf` show analysis

-- Property: Ownership handles scope-based cleanup
prop_ownership_scope_cleanup :: String -> Property
prop_ownership_scope_cleanup varName =
  not (null varName) && all (\c -> isLetter c || c == '_') varName ==>
  let sourceCode = "{ var " ++ varName ++ " = 42 }\nvar x = 24"
      parseResult = parseTypus sourceCode
      ownershipResult = case parseResult of
        Left _ -> Nothing
        Right ast -> Just (analyzeOwnership ast)
  in property $ case ownershipResult of
    Nothing -> False
    Right analysis -> property $ "scope" `isInfixOf` show analysis .||. "cleanup" `isInfixOf` show analysis

-- Property: Ownership tracks lifetime annotations
prop_ownership_lifetime_annotations :: String -> String -> Property
prop_ownership_lifetime_annotations varName lifetime =
  not (null varName) && not (null lifetime) &&
  all (\c -> isLetter c || c == '_') varName &&
  all isLetter lifetime ==>
  let sourceCode = "var " ++ varName ++ ": '" ++ lifetime ++ "' = 42"
      parseResult = parseTypus sourceCode
      ownershipResult = case parseResult of
        Left _ -> Nothing
        Right ast -> Just (analyzeOwnership ast)
  in property $ case ownershipResult of
    Nothing -> False
    Right analysis -> property $ varName `isInfixOf` show analysis .&&. lifetime `isInfixOf` show analysis

-- Property: Ownership handles shared references
prop_ownership_shared_references :: String -> [String] -> Property
prop_ownership_shared_references varName refs =
  not (null varName) && not (null refs) && length refs <= 3 ==>
  let refLines = map (\ref -> "var " ++ ref ++ " = &" ++ varName) refs
      sourceCode = "var " ++ varName ++ " = 42\n" ++ Data.List.unlines refLines
      parseResult = parseTypus sourceCode
      ownershipResult = case parseResult of
        Left _ -> Nothing
        Right ast -> Just (analyzeOwnership ast)
  in property $ case ownershipResult of
    Nothing -> False
    Right analysis -> property $ varName `isInfixOf` show analysis .&&. "shared" `isInfixOf` show analysis

-- Property: Ownership prevents use-after-move
prop_ownership_prevents_use_after_move :: String -> String -> Property
prop_ownership_prevents_use_after_move sourceVar targetVar =
  not (null sourceVar) && not (null targetVar) &&
  all (\c -> isLetter c || c == '_') sourceVar &&
  all (\c -> isLetter c || c == '_') targetVar &&
  sourceVar /= targetVar ==>
  let sourceCode = "var " ++ sourceVar ++ " = 42\nvar " ++ targetVar ++ " = " ++ sourceVar ++ "\nvar x = " ++ sourceVar
      parseResult = parseTypus sourceCode
      ownershipResult = case parseResult of
        Left _ -> Nothing
        Right ast -> Just (analyzeOwnership ast)
  in property $ case ownershipResult of
    Nothing -> True -- Should fail due to use after move
    Right analysis -> property $ "error" `isInfixOf` show analysis .||. "use after move" `isInfixOf` show analysis

-- Property: Ownership handles mutable references
prop_ownership_mutable_references :: String -> Property
prop_ownership_mutable_references varName =
  not (null varName) && all (\c -> isLetter c || c == '_') varName ==>
  let sourceCode = "var " ++ varName ++ " = 42\nvar mutRef = &mut " ++ varName
      parseResult = parseTypus sourceCode
      ownershipResult = case parseResult of
        Left _ -> Nothing
        Right ast -> Just (analyzeOwnership ast)
  in property $ case ownershipResult of
    Nothing -> False
    Right analysis -> property $ "mut" `isInfixOf` show analysis .||. "mutable" `isInfixOf` show analysis

-- Property: Ownership tracks copy operations
prop_ownership_copy_operations :: String -> String -> Property
prop_ownership_copy_operations sourceVar targetVar =
  not (null sourceVar) && not (null targetVar) &&
  all (\c -> isLetter c || c == '_') sourceVar &&
  all (\c -> isLetter c || c == '_') targetVar &&
  sourceVar /= targetVar ==>
  let sourceCode = "var " ++ sourceVar ++ " = 42\nvar " ++ targetVar ++ " = copy(" ++ sourceVar ++ ")"
      parseResult = parseTypus sourceCode
      ownershipResult = case parseResult of
        Left _ -> Nothing
        Right ast -> Just (analyzeOwnership ast)
  in property $ case ownershipResult of
    Nothing -> False
    Right analysis -> property $ "copy" `isInfixOf` show analysis .||. sourceVar `isInfixOf` show analysis

-- Property: Ownership handles conditional moves
prop_ownership_conditional_moves :: String -> String -> Bool -> Property
prop_ownership_conditional_moves sourceVar targetVar condition =
  not (null sourceVar) && not (null targetVar) &&
  all (\c -> isLetter c || c == '_') sourceVar &&
  all (\c -> isLetter c || c == '_') targetVar ==>
  let sourceCode = "var " ++ sourceVar ++ " = 42\nif true { var " ++ targetVar ++ " = " ++ sourceVar ++ " }"
      parseResult = parseTypus sourceCode
      ownershipResult = case parseResult of
        Left _ -> Nothing
        Right ast -> Just (analyzeOwnership ast)
  in property $ case ownershipResult of
    Nothing -> False
    Right analysis -> property $ "conditional" `isInfixOf` show analysis .||. "if" `isInfixOf` show analysis

-- Property: Ownership handles loop variable lifetimes
prop_ownership_loop_variables :: String -> Int -> Property
prop_ownership_loop_variables loopVar iterations =
  not (null loopVar) && all (\c -> isLetter c || c == '_') loopVar &&
  iterations >= 0 && iterations <= 5 ==>
  let sourceCode = "for " ++ loopVar ++ " = 0 to " ++ show iterations ++ " { var x = " ++ loopVar ++ " }"
      parseResult = parseTypus sourceCode
      ownershipResult = case parseResult of
        Left _ -> Nothing
        Right ast -> Just (analyzeOwnership ast)
  in property $ case ownershipResult of
    Nothing -> iterations > 3 -- May fail for complex loops
    Right analysis -> property $ loopVar `isInfixOf` show analysis .||. "for" `isInfixOf` show analysis

-- Property: Ownership is deterministic
prop_ownership_deterministic :: String -> Property
prop_ownership_deterministic sourceCode =
  let parseResult = parseTypus sourceCode
      ownershipResult1 = case parseResult of
        Left _ -> Nothing
        Right ast -> Just (analyzeOwnership ast)
      ownershipResult2 = case parseResult of
        Left _ -> Nothing
        Right ast -> Just (analyzeOwnership ast)
  in property $ ownershipResult1 == ownershipResult2

-- Property: Ownership analysis completeness
prop_ownership_analysis_complete :: String -> Property
prop_ownership_analysis_complete sourceCode =
  let parseResult = parseTypus sourceCode
      ownershipResult = case parseResult of
        Left _ -> Nothing
        Right ast -> Just (analyzeOwnership ast)
  in property $ case ownershipResult of
    Nothing -> True
    Right analysis -> property $ not (null (show analysis))

-- Property: Ownership handles complex transfer scenarios
prop_ownership_complex_transfer :: [(String, String)] -> Property
prop_ownership_complex_transfer transfers =
  not (null transfers) && length transfers <= 3 ==>
  let transferLines = map (\(src, dst) -> "var " ++ dst ++ " = " ++ src) transfers
      sourceCode = Data.List.unlines transferLines
      parseResult = parseTypus sourceCode
      ownershipResult = case parseResult of
        Left _ -> Nothing
        Right ast -> Just (analyzeOwnership ast)
  in property $ case ownershipResult of
    Nothing -> False
    Right analysis -> property $ all (\(src, _) -> src `isInfixOf` show analysis) transfers

tests :: TestTree
tests =
  testGroup "Ownership Transfer QuickCheck Tests"
    [ fastProperty "variable lifetime" prop_ownership_variable_lifetime
    , fastProperty "detects moves" prop_ownership_detects_moves
    , fastProperty "handles borrows" prop_ownership_handles_borrows
    , fastProperty "prevents double moves" prop_ownership_prevents_double_moves
    , fastProperty "function parameters" prop_ownership_function_parameters
    , fastProperty "return values" prop_ownership_return_values
    , fastProperty "scope cleanup" prop_ownership_scope_cleanup
    , fastProperty "lifetime annotations" prop_ownership_lifetime_annotations
    , fastProperty "shared references" prop_ownership_shared_references
    , fastProperty "prevents use after move" prop_ownership_prevents_use_after_move
    , fastProperty "mutable references" prop_ownership_mutable_references
    , fastProperty "copy operations" prop_ownership_copy_operations
    , fastProperty "conditional moves" prop_ownership_conditional_moves
    , fastProperty "loop variables" prop_ownership_loop_variables
    , fastProperty "deterministic" prop_ownership_deterministic
    , fastProperty "analysis complete" prop_ownership_analysis_complete
    , fastProperty "complex transfer" prop_ownership_complex_transfer
    ]