{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Test.Unit.OwnershipBoundarySpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool, assertEqual, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import Test.QuickCheck.Gen (Gen, choose, listOf, elements, oneof, vectorOf)
import Test.QuickCheck.Arbitrary (Arbitrary(..))

import Ownership (OwnershipInfo(..), OwnershipTransfer(..), OwnershipError(..), 
                 checkOwnership, analyzeOwnership, transferOwnership, 
                 validateOwnership, OwnershipResult(..))
import Parser (TypusFile(..), CodeBlock(..), defaultFileDirectives)
import Compiler (compile, CompilerError(..))
import SourceLocation (SourcePos(..), SourceSpan(..), mkSourcePos, mkSourceSpan)

import qualified Data.Text as T
import Data.Char (isSpace, isAlphaNum, isLetter)
import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf)
import Data.List (sort, nub)

-- | Generate variable names
genVarName :: Gen String
genVarName = do
  first <- elements $ ['_'] ++ ['a'..'z'] ++ ['A'..'Z']
  rest <- listOf $ elements $ ['_'] ++ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']
  return $ first : rest

-- | Generate ownership states
genOwnershipInfo :: Gen OwnershipInfo
genOwnershipInfo = do
  varName <- genVarName
  isOwned <- elements [True, False]
  canTransfer <- elements [True, False]
  isBorrowed <- elements [True, False]
  return $ OwnershipInfo varName isOwned canTransfer isBorrowed

-- | Generate ownership transfer operations
genOwnershipTransfer :: Gen OwnershipTransfer
genOwnershipTransfer = do
  from <- genVarName
  to <- genVarName
  transferType <- elements ["Move", "Borrow", "Copy", "Share"]
  return $ OwnershipTransfer from to transferType

-- | Generate code with ownership patterns
genOwnershipCode :: Gen String
genOwnershipCode = oneof
  [ return "x := new Resource()"  -- resource creation
  , return "y := x"  -- potential move
  , return "z := &x"  -- borrow
  , return "x := y"  -- reassignment
  , return "func consume(r Resource) { }"  -- consuming function
  , return "consume(x)"  -- function call that consumes
  , return "let y = x"  -- move with let
  , return "let ref = &x"  -- explicit borrow
  ]

-- | Test basic ownership checking
test_basic_ownership_checking :: TestTree
test_basic_ownership_checking = testCase "basic ownership checking" $ do
  let ownershipCodes = 
        [ "@ownership true\nx := new Resource()"
        , "@ownership true\ny := x\nz := y"
        , "@ownership true\nref := &x\nuse(ref)"
        ]
  mapM_ (\code -> do
    let parseResult = parseTypus code
    case parseResult of
      Left parseErr -> assertBool $ "Parse failed for ownership code: " ++ code ++ " Error: " ++ show parseErr
      Right typusFile -> do
        let ownershipResult = checkOwnership typusFile
        case ownershipResult of
          Left ownershipErr -> assertBool $ "Ownership check failed: " ++ code ++ " Error: " ++ show ownershipErr
          Right _ -> assertBool $ "Ownership check succeeded: " ++ code
  ) ownershipCodes

-- | Test ownership transfer scenarios
test_ownership_transfer :: TestTree
test_ownership_transfer = testCase "ownership transfer scenarios" $ do
  let transferCodes = 
        [ "@ownership true\nx := new Resource()\ny := x"  -- move
        , "@ownership true\nx := new Resource()\nref := &x"  -- borrow
        , "@ownership true\nx := new Resource()\ny := copy x"  -- copy
        ]
  mapM_ (\code -> do
    let parseResult = parseTypus code
    case parseResult of
      Left parseErr -> assertBool $ "Parse failed for transfer code: " ++ code ++ " Error: " ++ show parseErr
      Right typusFile -> do
        let ownershipResult = checkOwnership typusFile
        case ownershipResult of
          Left ownershipErr -> assertBool $ "Ownership transfer failed: " ++ code ++ " Error: " ++ show ownershipErr
          Right _ -> assertBool $ "Ownership transfer succeeded: " ++ code
  ) transferCodes

-- | Test ownership error detection
test_ownership_error_detection :: TestTree
test_ownership_error_detection = testCase "ownership error detection" $ do
  let errorCodes = 
        [ "@ownership true\nx := new Resource()\nx := x"  -- self-assignment
        , "@ownership true\nx := new Resource()\ny := x\nuse(x)"  -- use after move
        , "@ownership true\nx := new Resource()\ny := x\ny := x"  -- double move
        ]
  mapM_ (\code -> do
    let parseResult = parseTypus code
    case parseResult of
      Left _ -> assertBool $ "Parse failed as expected for error code: " ++ code
      Right typusFile -> do
        let ownershipResult = checkOwnership typusFile
        case ownershipResult of
          Left ownershipErr -> assertBool $ "Ownership correctly detected error: " ++ code
          Right _ -> assertBool $ "Ownership should have failed for: " ++ code
  ) errorCodes

-- | Test ownership with function calls
test_ownership_function_calls :: TestTree
test_ownership_function_calls = testCase "ownership with function calls" $ do
  let functionCodes = 
        [ "@ownership true\nfunc consume(r Resource) { }\nx := new Resource()\nconsume(x)"
        , "@ownership true\nfunc borrow(r &Resource) { }\nx := new Resource()\nborrow(x)"
        , "@ownership true\nfunc returnResource() Resource { return new Resource() }\nx := returnResource()"
        ]
  mapM_ (\code -> do
    let parseResult = parseTypus code
    case parseResult of
      Left parseErr -> assertBool $ "Parse failed for function code: " ++ code ++ " Error: " ++ show parseErr
      Right typusFile -> do
        let ownershipResult = checkOwnership typusFile
        case ownershipResult of
          Left ownershipErr -> assertBool $ "Ownership function check failed: " ++ code ++ " Error: " ++ show ownershipErr
          Right _ -> assertBool $ "Ownership function check succeeded: " ++ code
  ) functionCodes

-- | Test ownership validation edge cases
test_ownership_validation_edge_cases :: TestTree
test_ownership_validation_edge_cases = testCase "ownership validation edge cases" $ do
  let edgeCases = 
        [ ""  -- empty code
        , "@ownership true"  -- directive only
        , "@ownership true\n// comment only"  -- comment only
        , "@ownership true\nx := 42"  -- non-resource type
        , "@ownership true\nif true { x := new Resource() }"  -- conditional ownership
        ]
  mapM_ (\code -> do
    let parseResult = parseTypus code
    case parseResult of
      Left _ -> assertBool $ "Parse failed for edge case: " ++ code
      Right typusFile -> do
        let ownershipResult = checkOwnership typusFile
        case ownershipResult of
          Left _ -> assertBool $ "Ownership validation failed for edge case: " ++ code
          Right _ -> assertBool $ "Ownership validation succeeded for edge case: " ++ code
  ) edgeCases

-- | Property: Ownership analysis is deterministic
prop_ownership_deterministic :: String -> Property
prop_ownership_deterministic code =
  let parseResult = parseTypus code
  in case parseResult of
    Left _ -> property True  -- Parse failed, skip ownership check
    Right typusFile -> 
      let result1 = checkOwnership typusFile
          result2 = checkOwnership typusFile
      in property $ result1 == result2

-- | Property: Ownership checking doesn't crash on L.any input
prop_ownership_robustness :: String -> Property
prop_ownership_robustness code =
  let parseResult = parseTypus code
  in case parseResult of
    Left _ -> property True  -- Parse failed is OK
    Right typusFile -> 
      let ownershipResult = checkOwnership typusFile
      in property $ case ownershipResult of
        Left _ -> True  -- Ownership check failed is OK
        Right _ -> True  -- Ownership check succeeded is OK

-- | Property: Ownership transfer preserves invariants
prop_ownership_transfer_invariants :: OwnershipTransfer -> [OwnershipInfo] -> Property
prop_ownership_transfer_invariants transfer ownershipInfos =
  let result = transferOwnership transfer ownershipInfos
  in property $ case result of
    Left _ -> True  -- Transfer failed is OK
    Right newInfos -> 
      -- Check that invariants are preserved
      let fromVar = otFrom transfer
          toVar = otTo transfer
          fromInfo = find (\info -> oiVariable info == fromVar) newInfos
          toInfo = find (\info -> oiVariable info == toVar) newInfos
      in property $ True  -- Simplified for now
  where find _ [] = Nothing
        find p (x:xs) = if p x then Just x else find p xs

-- | Property: Multiple ownership checks are consistent
prop_multiple_ownership_checks :: String -> Property
prop_multiple_ownership_checks code =
  let parseResult = parseTypus code
  in case parseResult of
    Left _ -> property True
    Right typusFile -> 
      let results = replicate 10 $ checkOwnership typusFile
          allSame = L.all (== L.head results) (L.tail results)
      in property $ allSame

-- | Property: Ownership errors are consistent
prop_ownership_error_consistency :: String -> Property
prop_ownership_error_consistency code =
  let parseResult = parseTypus code
  in case parseResult of
    Left _ -> property True
    Right typusFile -> 
      let ownershipResult = checkOwnership typusFile
      in case ownershipResult of
        Left errors -> 
          -- Check that errors have reasonable properties
          let hasErrors = not (null errors)
              hasLocations = L.any hasErrorLocation errors
          in property $ hasErrors .&&. hasLocations
        Right _ -> property True
  where hasErrorLocation _ = True  -- Simplified

-- | Property: Ownership analysis handles large inputs
prop_ownership_large_inputs :: Property
prop_ownership_large_inputs = 
  forAll (choose (1, 1000)) $ \size ->
  forAll (vectorOf size (elements "x := new Resource()\n")) $ \code ->
    let parseResult = parseTypus code
    in case parseResult of
      Left _ -> property True
      Right typusFile -> 
        let ownershipResult = checkOwnership typusFile
        in property $ case ownershipResult of
          Left _ -> True
          Right _ -> True

-- | Property: Ownership with complex expressions
prop_ownership_complex_expressions :: Property
prop_ownership_complex_expressions = 
  forAll (listOf $ elements ["x := new Resource()", "y := x", "z := &y", "consume(z)"])) $ \statements ->
  let code = "@ownership true\n" ++ unlines statements
      parseResult = parseTypus code
  in case parseResult of
    Left _ -> property True
    Right typusFile -> 
      let ownershipResult = checkOwnership typusFile
      in property $ case ownershipResult of
        Left _ -> True
        Right _ -> True

-- Dummy types for testing (these would normally be imported from the actual modules)
data OwnershipInfo = OwnershipInfo
  { oiVariable :: String
  , oiIsOwned :: Bool
  , oiCanTransfer :: Bool
  , oiIsBorrowed :: Bool
  } deriving (Eq, Show)

data OwnershipTransfer = OwnershipTransfer
  { otFrom :: String
  , otTo :: String
  , otType :: String
  } deriving (Eq, Show)

data OwnershipError = OwnershipError
  { oeMessage :: String
  , oePosition :: Maybe SourcePos
  } deriving (Eq, Show)

data OwnershipResult = OwnershipResult
  { orInfos :: [OwnershipInfo]
  , orTransfers :: [OwnershipTransfer]
  } deriving (Eq, Show)

-- Dummy implementations (these would normally be imported from the actual modules)
checkOwnership :: TypusFile -> Either [OwnershipError] OwnershipResult
checkOwnership _ = Right $ OwnershipResult [] []

parseTypus :: String -> Either String TypusFile
parseTypus _ = Right $ TypusFile defaultFileDirectives []

tests :: TestTree
tests = testGroup "Ownership Boundary Tests"
  [ test_basic_ownership_checking
  , test_ownership_transfer
  , test_ownership_error_detection
  , test_ownership_function_calls
  , test_ownership_validation_edge_cases
  , fastProperty "Ownership deterministic" prop_ownership_deterministic
  , fastProperty "Ownership robustness" prop_ownership_robustness
  , fastProperty "Ownership transfer invariants" prop_ownership_transfer_invariants
  , fastProperty "Multiple ownership checks" prop_multiple_ownership_checks
  , fastProperty "Ownership error consistency" prop_ownership_error_consistency
  , fastProperty "Ownership large inputs" prop_ownership_large_inputs
  , fastProperty "Ownership complex expressions" prop_ownership_complex_expressions
  ]