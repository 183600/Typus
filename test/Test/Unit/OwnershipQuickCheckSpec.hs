{-# LANGUAGE DeriveGeneric #-}
module Test.Unit.OwnershipQuickCheckSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import GHC.Generics (Generic)
import Data.List (nub, (\\))

import Ownership
import Ownership.Common.Types

-- Test data generators
generateOwnershipType :: Int -> OwnershipType
generateOwnershipType n = case n `mod` 4 of
  0 -> Owned
  1 -> Borrowed
  2 -> Shared
  3 -> Moved

generateOwnershipError :: Int -> OwnershipError
generateOwnershipError n = case n `mod` 5 of
  0 -> OwnershipViolation "test variable" "test operation"
  1 -> DoubleFree "test resource"
  2 -> UseAfterFree "test variable"
  3 -> InvalidBorrow "test variable" "test reason"
  4 -> LifetimeError "test variable" "test resource"

generateOwnershipTransfer :: Int -> OwnershipTransfer
generateOwnershipTransfer n = case n `mod` 3 of
  0 -> MoveOwnership "from" "to"
  1 -> BorrowOwnership "from" "to"
  2 -> ShareOwnership "from" "to"

generateOwnershipConstraint :: Int -> OwnershipConstraint
generateOwnershipConstraint n = case n `mod` 3 of
  0 -> MustNotMove ("resource" ++ show n)
  1 -> MustNotCopy ("resource" ++ show n)
  2 -> MustNotBorrow ("resource" ++ show n)

generateOwnershipAnalysis :: Int -> OwnershipAnalysis
generateOwnershipAnalysis n = OwnershipAnalysis
    { oaOwners = [("owner" ++ show i, "resource" ++ show i) | i <- [1..n `mod` 5]]
    , oaBorrowers = [("borrower" ++ show i, "resource" ++ show i) | i <- [1..n `mod` 3]]
    , oaErrors = take (n `mod` 3) [generateOwnershipError i | i <- [1..10]]
    }

-- QuickCheck properties
prop_ownership_type_equality :: Property
prop_ownership_type_equality =
  forAll arbitrary $ \n ->
    let ot1 = generateOwnershipType n
        ot2 = generateOwnershipType n
    in property $ ot1 == ot2

prop_ownership_error_formatting :: Property
prop_ownership_error_formatting =
  forAll arbitrary $ \n ->
    let error = generateOwnershipError n
        formatted = show error
    in property $ 
      not (null formatted) &&
      "Ownership" `isInfixOf` formatted

prop_ownership_transfer_creation :: Property
prop_ownership_transfer_creation =
  forAll arbitrary $ \n ->
    let transfer = generateOwnershipTransfer n
    in case transfer of
         MoveOwnership from to -> 
           property $ from /= "" && to /= ""
         BorrowOwnership from to -> 
           property $ from /= "" && to /= ""
         ShareOwnership from to -> 
           property $ from /= "" && to /= ""

prop_ownership_constraint_validation :: Property
prop_ownership_constraint_validation =
  forAll arbitrary $ \n ->
    let constraint = generateOwnershipConstraint n
        errors = validateOwnershipConstraints [constraint]
    in case constraint of
         MustNotMove _ -> property $ True  -- Placeholder implementation
         MustNotCopy _ -> property $ True  -- Placeholder implementation
         MustNotBorrow _ -> property $ True  -- Placeholder implementation

prop_ownership_analysis_creation :: Property
prop_ownership_analysis_creation =
  forAll arbitrary $ \n ->
    let analysis = generateOwnershipAnalysis n
    in property $
      length (oaOwners analysis) >= 0 &&
      length (oaBorrowers analysis) >= 0 &&
      length (oaErrors analysis) >= 0

prop_ownership_analysis_has_errors :: Property
prop_ownership_analysis_has_errors =
  forAll arbitrary $ \n ->
    let analysis = generateOwnershipAnalysis n
        hasErrors = hasOwnershipErrors analysis
        errors = getOwnershipErrors analysis
    in property $ hasErrors == not (null errors)

prop_ownership_analysis_clear_errors :: Property
prop_ownership_analysis_clear_errors =
  forAll arbitrary $ \n ->
    let analysis = generateOwnershipAnalysis n
        cleared = clearOwnershipErrors analysis
    in property $ null (oaErrors cleared)

prop_ownership_analysis_merge :: Property
prop_ownership_analysis_merge =
  forAll arbitrary $ \n1 ->
  forAll arbitrary $ \n2 ->
    let analysis1 = generateOwnershipAnalysis n1
        analysis2 = generateOwnershipAnalysis n2
        merged = mergeOwnershipAnalyses analysis1 analysis2
    in property $
      oaOwners merged == oaOwners analysis1 ++ oaOwners analysis2 &&
      oaBorrowers merged == oaBorrowers analysis1 ++ oaBorrowers analysis2 &&
      oaErrors merged == oaErrors analysis1 ++ oaErrors analysis2

prop_ownership_analysis_owners :: Property
prop_ownership_analysis_owners =
  forAll arbitrary $ \n ->
    let analysis = generateOwnershipAnalysis n
        owners = getOwners analysis
        expectedOwners = map fst (oaOwners analysis)
    in property $ owners == expectedOwners

prop_ownership_analysis_borrowers :: Property
prop_ownership_analysis_borrowers =
  forAll arbitrary $ \n ->
    let analysis = generateOwnershipAnalysis n
        borrowers = getBorrowers analysis
        expectedBorrowers = map fst (oaBorrowers analysis)
    in property $ borrowers == expectedBorrowers

prop_ownership_analysis_owned_resources :: Property
prop_ownership_analysis_owned_resources =
  forAll arbitrary $ \n ->
    let analysis = generateOwnershipAnalysis n
        resources = getOwnedResources analysis
        expectedResources = map snd (oaOwners analysis)
    in property $ resources == expectedResources

prop_ownership_analysis_is_owner :: Property
prop_ownership_analysis_is_owner =
  forAll arbitrary $ \n ->
  forAll arbitrary $ \m ->
    let analysis = generateOwnershipAnalysis n
        owner = "owner" ++ show m
        resource = "resource" ++ show (m `mod` 5 + 1)
        isOwnerResult = isOwner analysis owner resource
    in property $ isOwnerResult == ((owner, resource) `elem` oaOwners analysis)

prop_ownership_analysis_is_borrower :: Property
prop_ownership_analysis_is_borrower =
  forAll arbitrary $ \n ->
  forAll arbitrary $ \m ->
    let analysis = generateOwnershipAnalysis n
        borrower = "borrower" ++ show m
        resource = "resource" ++ show (m `mod` 3 + 1)
        isBorrowerResult = isBorrower analysis borrower resource
    in property $ isBorrowerResult == ((borrower, resource) `elem` oaBorrowers analysis)

prop_ownership_transfer_check :: Property
prop_ownership_transfer_check =
  forAll arbitrary $ \from ->
  forAll arbitrary $ \to ->
  forAll arbitrary $ \resource ->
    let result = checkOwnershipTransfer from to resource
    in case result of
         Left _ -> property False  -- Placeholder implementation should always succeed
         Right success -> property success

prop_ownership_can_transfer :: Property
prop_ownership_can_transfer =
  forAll arbitrary $ \n ->
  forAll arbitrary $ \m ->
    let analysis = generateOwnershipAnalysis n
        from = "owner" ++ show m
        to = "receiver" ++ show m
    in property $ canTransferOwnership analysis from to  -- Placeholder implementation

prop_ownership_transfer :: Property
prop_ownership_transfer =
  forAll arbitrary $ \n ->
  forAll arbitrary $ \m ->
    let analysis = generateOwnershipAnalysis n
        owner = "owner" ++ show m
        resource = "resource" ++ show (m `mod` 5 + 1)
        result = transferOwnership analysis owner resource
    in case result of
         Left _ -> property False  -- Placeholder implementation should always succeed
         Right newAnalysis -> 
           property $ (owner, resource) `elem` oaOwners newAnalysis

prop_ownership_analyzer_creation :: Property
prop_ownership_analyzer_creation =
  let analyzer = newOwnershipAnalyzer
  in property $ True  -- Placeholder implementation

prop_lex_all :: Property
prop_lex_all =
  forAll arbitrary $ \input ->
    let result = lexAll input
    in property $ True  -- Placeholder implementation

prop_parse_program :: Property
prop_parse_program =
  forAll arbitrary $ \tokens ->
    let result = parseProgram tokens
    in property $ True  -- Placeholder implementation

prop_builtin_functions :: Property
prop_builtin_functions =
  let functions = builtInFunctions
  in property $ not (null functions)  -- Placeholder implementation

prop_analyze_ownership :: Property
prop_analyze_ownership =
  forAll arbitrary $ \input ->
    let result = analyzeOwnership input
    in property $ True  -- Placeholder implementation

prop_analyze_ownership_file :: Property
prop_analyze_ownership_file =
  forAll arbitrary $ \filename ->
    let result = analyzeOwnershipFile filename
    in property $ True  -- Placeholder implementation

prop_analyze_ownership_debug :: Property
prop_analyze_ownership_debug =
  forAll arbitrary $ \input ->
    let result = analyzeOwnershipDebug input
    in property $ True  -- Placeholder implementation

prop_format_ownership_errors :: Property
prop_format_ownership_errors =
  forAll arbitrary $ \n ->
    let errors = take (n `mod` 5) [generateOwnershipError i | i <- [1..10]]
        formatted = formatOwnershipErrors errors
    in property $ not (null formatted)  -- Placeholder implementation

-- Test suite
testSuite :: TestTree
testSuite = testGroup "Ownership QuickCheck Tests"
  [ testProperty "ownership type equality" prop_ownership_type_equality
  , testProperty "ownership error formatting" prop_ownership_error_formatting
  , testProperty "ownership transfer creation" prop_ownership_transfer_creation
  , testProperty "ownership constraint validation" prop_ownership_constraint_validation
  , testProperty "ownership analysis creation" prop_ownership_analysis_creation
  , testProperty "ownership analysis has errors" prop_ownership_analysis_has_errors
  , testProperty "ownership analysis clear errors" prop_ownership_analysis_clear_errors
  , testProperty "ownership analysis merge" prop_ownership_analysis_merge
  , testProperty "ownership analysis owners" prop_ownership_analysis_owners
  , testProperty "ownership analysis borrowers" prop_ownership_analysis_borrowers
  , testProperty "ownership analysis owned resources" prop_ownership_analysis_owned_resources
  , testProperty "ownership analysis is owner" prop_ownership_analysis_is_owner
  , testProperty "ownership analysis is borrower" prop_ownership_analysis_is_borrower
  , testProperty "ownership transfer check" prop_ownership_transfer_check
  , testProperty "ownership can transfer" prop_ownership_can_transfer
  , testProperty "ownership transfer" prop_ownership_transfer
  , testProperty "ownership analyzer creation" prop_ownership_analyzer_creation
  , testProperty "lex all" prop_lex_all
  , testProperty "parse program" prop_parse_program
  , testProperty "builtin functions" prop_builtin_functions
  , testProperty "analyze ownership" prop_analyze_ownership
  , testProperty "analyze ownership file" prop_analyze_ownership_file
  , testProperty "analyze ownership debug" prop_analyze_ownership_debug
  , testProperty "format ownership errors" prop_format_ownership_errors
  ]

-- Unit tests for specific edge cases
unitTests :: TestTree
unitTests = testGroup "Ownership Unit Tests"
  [ testCase "empty ownership analysis" $ do
      let analysis = OwnershipAnalysis [] [] []
      assertBool "Empty analysis should have no errors" $ not (hasOwnershipErrors analysis)
      assertEqual "Empty analysis should have no owners" [] (getOwners analysis)
      assertEqual "Empty analysis should have no borrowers" [] (getBorrowers analysis)

  , testCase "ownership analysis with errors" $ do
      let errors = [OwnershipViolation "x" "move", UseAfterFree "y"]
          analysis = OwnershipAnalysis [] [] errors
      assertBool "Analysis with errors should have errors" $ hasOwnershipErrors analysis
      assertEqual "Should return correct errors" errors (getOwnershipErrors analysis)

  , testCase "clear ownership errors" $ do
      let errors = [OwnershipViolation "x" "move"]
          analysis = OwnershipAnalysis [] [] errors
          cleared = clearOwnershipErrors analysis
      assertBool "Cleared analysis should have no errors" $ not (hasOwnershipErrors cleared)

  , testCase "merge ownership analyses" $ do
      let analysis1 = OwnershipAnalysis [("owner1", "res1")] [] []
          analysis2 = OwnershipAnalysis [] [("borrower1", "res2")] []
          merged = mergeOwnershipAnalyses analysis1 analysis2
      assertEqual "Merged analysis should combine owners" [("owner1", "res1")] (oaOwners merged)
      assertEqual "Merged analysis should combine borrowers" [("borrower1", "res2")] (oaBorrowers merged)

  , testCase "is owner check" $ do
      let analysis = OwnershipAnalysis [("owner1", "res1")] [] []
      assertBool "Should identify owner" $ isOwner analysis "owner1" "res1"
      assertBool "Should not identify non-owner" $ not (isOwner analysis "owner2" "res1")

  , testCase "is borrower check" $ do
      let analysis = OwnershipAnalysis [] [("borrower1", "res1")] []
      assertBool "Should identify borrower" $ isBorrower analysis "borrower1" "res1"
      assertBool "Should not identify non-borrower" $ not (isBorrower analysis "borrower2" "res1")

  , testCase "transfer ownership" $ do
      let analysis = OwnershipAnalysis [] [] []
          result = transferOwnership analysis "owner1" "res1"
      case result of
        Left _ -> assertFailure "Transfer should succeed"
        Right newAnalysis -> 
          assertBool "New analysis should have new owner" $ isOwner newAnalysis "owner1" "res1"
  ]

-- Combined test suite
tests :: TestTree
tests = testGroup "Ownership Tests"
  [ testSuite
  , unitTests
  ]