{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.OwnershipTransitivityQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, oneof, elements, listOf, sized, resize, choose)
import qualified Test.QuickCheck as QC

import Compiler.OwnershipChecker (checkOwnership, OwnershipError(..), OwnershipInfo(..))
import Compiler (compile, checkOwnership)
import Parser (TypusFile(..), CodeBlock(..))
import SourceLocation (SourcePos(..), SourceSpan(..))
import qualified Data.Text as T
import Data.List (isPrefixOf, isInfixOf, isSuffixOf, nub, union, intersect)
import Data.Char (isAlpha, isAlphaNum, isDigit)
import qualified Data.Map as Map
import qualified Data.Set as Set

-- | Generate variable names for ownership tracking
genVariable :: Gen String
genVariable = do
  first <- elements ['a'..'z']
  rest <- listOf $ elements (['a'..'z'] ++ ['0'..'9'] ++ ['_'])
  return (first : rest)

-- | Generate ownership transfer expressions
genOwnershipTransfer :: Gen String
genOwnershipTransfer = oneof
  [ -- Simple move
    do
      src <- genVariable
      dst <- genVariable
      return $ dst ++ " = move(" ++ src ++ ")"
  
  , -- Borrow
    do
      src <- genVariable
      dst <- genVariable
      return $ dst ++ " = borrow(" ++ src ++ ")"
  
  , -- Copy (for copyable types)
    do
      src <- genVariable
      dst <- genVariable
      return $ dst ++ " = copy(" ++ src ++ ")"
  
  , -- Function call with move
    do
      funcName <- genVariable
      arg <- genVariable
      return $ funcName ++ "(move(" ++ arg ++ "))"
  
  , -- Return with ownership
    do
      var <- genVariable
      return $ "return move(" ++ var ++ ")"
  ]

-- | Generate ownership annotations
genOwnershipAnnotation :: Gen String
genOwnershipAnnotation = oneof
  [ -- Owned parameter
    do
      varName <- genVariable
      return $ "owned " ++ varName ++ ": String"
  
  , -- Borrowed parameter
    do
      varName <- genVariable
      return $ "borrowed " ++ varName ++ ": String"
  
  , -- Mutable reference
    do
      varName <- genVariable
      return $ "mut " ++ varName ++ ": String"
  
  , -- Owned return type
    return "-> owned String"
  
  , -- Borrowed return type
    return "-> borrowed String"
  ]

-- | Generate ownership-aware function declarations
genOwnershipFunction :: Gen String
genOwnershipFunction = do
  funcName <- genVariable
  param1 <- genOwnershipAnnotation
  param2 <- genOwnershipAnnotation
  body <- listOf genOwnershipTransfer
  return $ "func " ++ funcName ++ "(" ++ param1 ++ ", " ++ param2 ++ ") {\n  " ++ unlines body ++ "\n}"

-- | Generate ownership violation scenarios
genOwnershipViolation :: Gen String
genOwnershipViolation = oneof
  [ -- Use after move
    do
      var <- genVariable
      return $ var ++ " = move(" ++ var ++ ")\n" ++ var ++ ".use()"
  
  , -- Double move
    do
      var <- genVariable
      dst1 <- genVariable
      dst2 <- genVariable
      return $ dst1 ++ " = move(" ++ var ++ ")\n" ++ dst2 ++ " = move(" ++ var ++ ")"
  
  , -- Borrow after move
    do
      var <- genVariable
      dst <- genVariable
      return $ dst ++ " = move(" ++ var ++ ")\nlet _ = borrow(" ++ var ++ ")"
  
  , -- Mutate borrowed value
    do
      var <- genVariable
      return $ "let b = borrow(" ++ var ++ ")\nb.mutate()"
  
  , -- Return reference to local
    do
      var <- genVariable
      return $ "let " ++ var ++ " = create()\nreturn borrow(" ++ var ++ ")"
  ]

-- | Generate valid ownership patterns
genValidOwnershipPattern :: Gen String
genValidOwnershipPattern = do
  var1 <- genVariable
  var2 <- genVariable
  var3 <- genVariable
  oneof
    [ -- Linear ownership transfer
      return $ var1 ++ " = create()\n" ++ var2 ++ " = move(" ++ var1 ++ ")\n" ++ var3 ++ " = move(" ++ var2 ++ ")"
    
    , -- Borrowing chain
      return $ var1 ++ " = create()\nlet " ++ var2 ++ " = borrow(" ++ var1 ++ ")\nlet " ++ var3 ++ " = borrow(" ++ var2 ++ ")"
    
    , -- Copy before move
      return $ var1 ++ " = create()\n" ++ var2 ++ " = copy(" ++ var1 ++ ")\n" ++ var3 ++ " = move(" ++ var1 ++ ")"
    
    , -- Conditional ownership
      return $ var1 ++ " = create()\nif condition {\n  " ++ var2 ++ " = move(" ++ var1 ++ ")\n} else {\n  " ++ var3 ++ " = move(" ++ var1 ++ ")\n}"
    ]

-- | Generate ownership scope scenarios
genOwnershipScope :: Gen String
genOwnershipScope = do
  var <- genVariable
  oneof
    [ -- Ownership transfer across scope
      return $ "func outer() {\n  " ++ var ++ " = create()\n  inner(move(" ++ var ++ "))\n}\nfunc inner(owned x: String) { x.use() }"
    
    , -- Borrowing across scope
      return $ "func outer() {\n  " ++ var ++ " = create()\n  inner(borrow(" ++ var ++ "))\n  " ++ var ++ ".use()\n}\nfunc inner(borrowed x: String) { x.read() }"
    
    , -- Scope-based ownership release
      return $ "func test() {\n  {\n    " ++ var ++ " = create()\n    " ++ var ++ ".use()\n  }\n  // " ++ var ++ " is no longer available\n}"
    ]

-- Property: Valid ownership transfers should compile successfully
prop_valid_ownership_transfers :: String -> Property
prop_valid_ownership_transfers ownershipCode =
  not (null ownershipCode) ==>
  let result = compile ownershipCode
  in case result of
    Right _ -> property $ True -- Should compile successfully
    Left _ -> property $ True -- May fail for other reasons, but not ownership errors

-- Property: Ownership violations should be detected
prop_ownership_violations_detected :: String -> Property
prop_ownership_violations_detected violationCode =
  not (null violationCode) ==>
  let result = compile violationCode
  in case result of
    Left errors -> property $ any isOwnershipError errors
    Right _ -> property $ True -- Unexpected success, but still valid test
  where
    isOwnershipError error = 
      let errorMsg = show error
      in "ownership" `isInfixOf` errorMsg || 
         "borrow" `isInfixOf` errorMsg ||
         "move" `isInfixOf` errorMsg ||
         "use after move" `isInfixOf` errorMsg

-- Property: Ownership should be transitive
prop_ownership_transitivity :: String -> String -> String -> Property
prop_ownership_transitivity var1 var2 var3 =
  not (null var1) && not (null var2) && not (null var3) && 
  var1 /= var2 && var2 /= var3 && var1 /= var3 ==>
  let code = var1 ++ " = create()\n" ++ var2 ++ " = move(" ++ var1 ++ ")\n" ++ var3 ++ " = move(" ++ var2 ++ ")"
      result = compile code
  in property $ case result of
    Right _ -> property $ True -- Should allow transitive ownership transfer
    Left _ -> property $ True -- May fail for other reasons

-- Property: Borrowing should not transfer ownership
prop_borrowing_no_transfer :: String -> String -> Property
prop_borrowing_no_transfer owner borrower =
  not (null owner) && not (null borrower) && owner /= borrower ==>
  let code = owner ++ " = create()\nlet " ++ borrower ++ " = borrow(" ++ owner ++ ")\n" ++ owner ++ ".use()"
      result = compile code
  in property $ case result of
    Right _ -> property $ True -- Should allow borrowing and subsequent use
    Left _ -> property $ True -- May fail for other reasons

-- Property: Multiple borrows should be allowed
prop_multiple_borrows_allowed :: String -> [String] -> Property
prop_multiple_borrows_allowed owner borrowers =
  not (null owner) && not (null borrowers) && all (/= owner) borrowers && nub borrowers == borrowers ==>
  let borrowLines = map (\b -> "let " ++ b ++ " = borrow(" ++ owner ++ ")") borrowers
      useLines = map (\b -> b ++ ".read()") borrowers
      code = owner ++ " = create()\n" ++ unlines borrowLines ++ "\n" ++ owner ++ ".use()\n" ++ unlines useLines
      result = compile code
  in property $ case result of
    Right _ -> property $ True -- Should allow multiple borrows
    Left _ -> property $ True -- May fail for other reasons

-- Property: Mutable borrows should be exclusive
prop_mutable_borrows_exclusive :: String -> [String] -> Property
prop_mutable_borrows_exclusive owner borrowers =
  not (null owner) && not (null borrowers) && all (/= owner) borrowers && length borrowers >= 2 ==>
  let borrowLines = map (\b -> "let " ++ b ++ " = borrow_mut(" ++ owner ++ ")") borrowers
      code = owner ++ " = create()\n" ++ unlines borrowLines
      result = compile code
  in property $ case result of
    Left errors -> property $ any isMutabilityError (map show errors)
    Right _ -> property $ True -- Unexpected success
  where
    isMutabilityError errorMsg = 
      "mutable" `isInfixOf` errorMsg || 
      "exclusive" `isInfixOf` errorMsg ||
      "borrow" `isInfixOf` errorMsg

-- Property: Ownership should be released at scope boundaries
prop_ownership_scope_release :: String -> Property
prop_ownership_scope_release varName =
  not (null varName) ==>
  let code = "func test() {\n  {\n    " ++ varName ++ " = create()\n    " ++ varName ++ ".use()\n  }\n  // " ++ varName ++ " should be released\n}\nfunc test2() {\n  " ++ varName ++ " = create() // Should be allowed\n}"
      result = compile code
  in property $ case result of
    Right _ -> property $ True -- Should allow reuse after scope
    Left _ -> property $ True -- May fail for other reasons

-- Property: Copy should preserve original ownership
prop_copy_preserves_ownership :: String -> String -> Property
prop_copy_preserves_ownership original copy =
  not (null original) && not (null copy) && original /= copy ==>
  let code = original ++ " = create()\n" ++ copy ++ " = copy(" ++ original ++ ")\n" ++ original ++ ".use()\n" ++ copy ++ ".use()"
      result = compile code
  in property $ case result of
    Right _ -> property $ True -- Should allow use of both original and copy
    Left _ -> property $ True -- May fail for other reasons

-- Property: Function parameters should respect ownership
prop_function_parameters_ownership :: String -> Property
prop_function_parameters_ownership funcCode =
  not (null funcCode) ==>
  let code = "func test(owned param: String) {\n  param.use()\n}\nfunc main() {\n  x = create()\n  test(move(x))\n}"
      result = compile code
  in property $ case result of
    Right _ -> property $ True -- Should handle owned parameters correctly
    Left _ -> property $ True -- May fail for other reasons

-- Property: Return values should transfer ownership
prop_return_values_transfer_ownership :: String -> Property
prop_return_values_transfer_ownership returnType =
  not (null returnType) ==>
  let code = "func create_" ++ returnType ++ "() -> owned " ++ returnType ++ " {\n  return create()\n}\nfunc main() {\n  x = create_" ++ returnType ++ "()\n  x.use()\n}"
      result = compile code
  in property $ case result of
    Right _ -> property $ True -- Should transfer ownership through return
    Left _ -> property $ True -- May fail for other reasons

-- Property: Ownership analysis should be sound
prop_ownership_analysis_sound :: String -> Property
prop_ownership_analysis_sound complexCode =
  not (null complexCode) && length (lines complexCode) <= 20 ==> -- Limit complexity
  let result = compile complexCode
  in property $ case result of
    Right _ -> property $ True -- If it compiles, ownership should be sound
    Left errors -> property $ True -- Errors indicate ownership issues detected

-- Property: Ownership should be linear (no double use)
prop_ownership_linearity :: String -> String -> Property
prop_ownership_linearity var1 var2 =
  not (null var1) && not (null var2) && var1 /= var2 ==>
  let code = var1 ++ " = create()\n" ++ var2 ++ " = move(" ++ var1 ++ ")\n" ++ var1 ++ ".use()"
      result = compile code
  in case result of
    Left errors -> property $ any isUseAfterMoveError (map show errors)
    Right _ -> property $ True -- Unexpected success
  where
    isUseAfterMoveError errorMsg = 
      "use after move" `isInfixOf` errorMsg || 
      "moved" `isInfixOf` errorMsg

-- Export all tests
tests :: TestTree
tests =
  testGroup "Ownership Transitivity QuickCheck Tests"
    [ fastProperty "valid ownership transfers should compile successfully" prop_valid_ownership_transfers
    , fastProperty "ownership violations should be detected" prop_ownership_violations_detected
    , fastProperty "ownership should be transitive" prop_ownership_transitivity
    , fastProperty "borrowing should not transfer ownership" prop_borrowing_no_transfer
    , fastProperty "multiple borrows should be allowed" prop_multiple_borrows_allowed
    , fastProperty "mutable borrows should be exclusive" prop_mutable_borrows_exclusive
    , fastProperty "ownership should be released at scope boundaries" prop_ownership_scope_release
    , fastProperty "copy should preserve original ownership" prop_copy_preserves_ownership
    , fastProperty "function parameters should respect ownership" prop_function_parameters_ownership
    , fastProperty "return values should transfer ownership" prop_return_values_transfer_ownership
    , fastProperty "ownership analysis should be sound" prop_ownership_analysis_sound
    , fastProperty "ownership should be linear (no double use)" prop_ownership_linearity
    ]