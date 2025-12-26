{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.NewOwnershipMemorySafetySpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertEqual, assertBool, assertFailure)
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property, (===), forAll, elements, suchThat)
import qualified Data.Text as T
import Data.Maybe (isJust, isNothing, catMaybes)
import Data.List (isInfixOf, nub)

import Ownership (OwnershipType(..), OwnershipError(..), OwnershipAnalyzer, OwnershipTransfer(..), 
                 analyzeOwnership, newOwnershipAnalyzer, formatOwnershipErrors)
import Parser (parseTypus, TypusFile(..))
import Compiler (compile)
import SourceLocation (SourcePos(..), SourceSpan(..), Located(..))

-- | Test ownership memory safety functionality
tests :: TestTree
tests =
  testGroup "New Ownership Memory Safety Tests"
    [ basicOwnershipTests
    , memorySafetyTests
    , ownershipTransferTests
    , borrowCheckerTests
    , lifetimeAnalysisTests
    , errorDetectionTests
    , quickCheckProperties
    ]

-- | Basic ownership functionality tests
basicOwnershipTests :: TestTree
basicOwnershipTests =
  testGroup "Basic Ownership Tests"
    [ testCase "Track simple ownership moves" $
        let input = "// @ownership: true\nlet x = 5\nlet y = x"
            result = compile "test.typus" input
        in case result of
             Left errs -> do
               let ownershipErrs = filter isOwnershipError errs
               assertBool "Should detect ownership move" (any isMoveError ownershipErrs)
               assertBool "Should track x as moved" (any tracksVariableMove "x" ownershipErrs)
             Right _ -> assertFailure "Should have failed with ownership error"

    , testCase "Allow copying of copyable types" $
        let input = "// @ownership: true\nlet x = 42\nlet y = x\nlet z = x"  -- int is copyable
            result = compile "test.typus" input
        in case result of
             Left errs -> do
               let ownershipErrs = filter isOwnershipError errs
               assertBool "Should not error on copyable types" (null ownershipErrs)
             Right _ -> assertBool "Should succeed with copyable types" True

    , testCase "Detect use after move" $
        let input = "// @ownership: true\nlet x = \"hello\"\nlet y = x\nlet len = length(x)"
            result = compile "test.typus" input
        in case result of
             Left errs -> do
               let ownershipErrs = filter isOwnershipError errs
               assertBool "Should detect use after move" (any isUseAfterMove ownershipErrs)
               assertBool "Should identify moved variable" (any mentionsVariable "x" ownershipErrs)
             Right _ -> assertFailure "Should have failed with use after move error"
    ]

-- | Memory safety tests
memorySafetyTests :: TestTree
memorySafetyTests =
  testGroup "Memory Safety Tests"
    [ testCase "Prevent double free scenarios" $
        let input = "// @ownership: true\nlet x = allocate()\nfree(x)\nfree(x)"
            result = compile "test.typus" input
        in case result of
             Left errs -> do
               let ownershipErrs = filter isOwnershipError errs
               assertBool "Should prevent double free" (any isDoubleFreeError ownershipErrs)
               assertBool "Should track freed resources" (any tracksFreedResource "x" ownershipErrs)
             Right _ -> assertFailure "Should have failed with double free error"

    , testCase "Detect dangling references" $
        let input = "// @ownership: true\nlet ref = &get_temp()\nlet value = *ref"
            result = compile "test.typus" input
        in case result of
             Left errs -> do
               let ownershipErrs = filter isOwnershipError errs
               assertBool "Should detect dangling reference" (any isDanglingReference ownershipErrs)
               assertBool "Should explain lifetime issue" (any explainsLifetimeIssue ownershipErrs)
             Right _ -> assertFailure "Should have failed with dangling reference error"

    , testCase "Validate resource cleanup" $
        let input = "// @ownership: true\n{\n  let x = allocate()\n  process(x)\n}  // x should be freed here"
            result = compile "test.typus" input
        in case result of
             Left errs -> do
               let ownershipErrs = filter isOwnershipError errs
               assertBool "Should ensure proper cleanup" (any ensuresCleanup ownershipErrs)
               assertBool "Should track scope boundaries" (any tracksScopeBoundaries ownershipErrs)
             Right _ -> assertBool "Should succeed with proper cleanup" True
    ]

-- | Ownership transfer tests
ownershipTransferTests :: TestTree
ownershipTransferTests =
  testGroup "Ownership Transfer Tests"
    [ testCase "Track function parameter ownership" $
        let input = "// @ownership: true\nfunc consume(data String) { process(data) }\nlet x = \"hello\"\nconsume(x)\nlet len = length(x)"
            result = compile "test.typus" input
        in case result of
             Left errs -> do
               let ownershipErrs = filter isOwnershipError errs
               assertBool "Should track parameter transfer" (any isParameterTransfer ownershipErrs)
               assertBool "Should detect post-use error" (any isUseAfterTransfer ownershipErrs)
             Right _ -> assertFailure "Should have failed with ownership transfer error"

    , testCase "Handle return value ownership" $
        let input = "// @ownership: true\nfunc create() String { return \"hello\" }\nlet x = create()\nlet y = x"
            result = compile "test.typus" input
        in case result of
             Left errs -> do
               let ownershipErrs = filter isOwnershipError errs
               assertBool "Should handle return value transfer" (any handlesReturnValueTransfer ownershipErrs)
             Right _ -> assertBool "Should succeed with return value transfer" True

    , testCase "Validate move semantics in assignments" $
        let input = "// @ownership: true\nlet x = String(\"hello\")\nlet y = x\nlet z = y"
            result = compile "test.typus" input
        in case result of
             Left errs -> do
               let ownershipErrs = filter isOwnershipError errs
               assertBool "Should validate move chain" (any validatesMoveChain ownershipErrs)
               assertBool "Should track all moves" (hasCorrectMoveCount 2 ownershipErrs)
             Right _ -> assertFailure "Should have failed with move chain error"
    ]

-- | Borrow checker tests
borrowCheckerTests :: TestTree
borrowCheckerTests =
  testGroup "Borrow Checker Tests"
    [ testCase "Allow immutable borrows" $
        let input = "// @ownership: true\nlet x = 42\nlet y = &x\nlet z = &x\nlet val = *y + *z"
            result = compile "test.typus" input
        in case result of
             Left errs -> do
               let ownershipErrs = filter isOwnershipError errs
               assertBool "Should allow multiple immutable borrows" (null ownershipErrs)
             Right _ -> assertBool "Should succeed with immutable borrows" True

    , testCase "Prevent mutable borrow conflicts" $
        let input = "// @ownership: true\nlet x = 42\nlet y = &mut x\nlet z = &x"
            result = compile "test.typus" input
        in case result of
             Left errs -> do
               let ownershipErrs = filter isOwnershipError errs
               assertBool "Should prevent mutable borrow conflict" (any isBorrowConflict ownershipErrs)
               assertBool "Should explain borrow rules" (any explainsBorrowRules ownershipErrs)
             Right _ -> assertFailure "Should have failed with borrow conflict error"

    , testCase "Track borrow lifetimes" $
        let input = "// @ownership: true\nlet x = 42\nlet y = {\n  let z = &x\n  *z\n}"
            result = compile "test.typus" input
        in case result of
             Left errs -> do
               let ownershipErrs = filter isOwnershipError errs
               assertBool "Should handle scoped borrows correctly" (any handlesScopedBorrows ownershipErrs)
             Right _ -> assertBool "Should succeed with scoped borrows" True
    ]

-- | Lifetime analysis tests
lifetimeAnalysisTests :: TestTree
lifetimeAnalysisTests =
  testGroup "Lifetime Analysis Tests"
    [ testCase "Detect lifetime mismatches" $
        let input = "// @ownership: true\nfunc longest<'a>(x: &'a str, y: &'a str) -> &'a str {\n  if len(x) > len(y) { x } else { y }\n}\nlet result = longest(&String(\"hello\"), &\"world\")"
            result = compile "test.typus" input
        in case result of
             Left errs -> do
               let ownershipErrs = filter isOwnershipError errs
               assertBool "Should detect lifetime mismatch" (any isLifetimeMismatch ownershipErrs)
               assertBool "Should explain lifetime requirements" (any explainsLifetimeRequirements ownershipErrs)
             Right _ -> assertFailure "Should have failed with lifetime mismatch error"

    , testCase "Handle struct lifetime annotations" $
        let input = "// @ownership: true\nstruct Ref<'a> { value: &'a int }\nlet x = 42\nlet r = Ref { value: &x }"
            result = compile "test.typus" input
        in case result of
             Left errs -> do
               let ownershipErrs = filter isOwnershipError errs
               assertBool "Should handle struct lifetimes" (any handlesStructLifetimes ownershipErrs)
             Right _ -> assertBool "Should succeed with struct lifetimes" True

    , testCase "Validate return reference safety" $
        let input = "// @ownership: true\nfunc bad_reference() -> &int {\n  let x = 42\n  &x  // Returning reference to local variable\n}"
            result = compile "test.typus" input
        in case result of
             Left errs -> do
               let ownershipErrs = filter isOwnershipError errs
               assertBool "Should prevent returning local reference" (any preventsLocalReferenceReturn ownershipErrs)
               assertBool "Should explain stack frame issue" (any explainsStackFrameIssue ownershipErrs)
             Right _ -> assertFailure "Should have failed with local reference return error"
    ]

-- | Error detection tests
errorDetectionTests :: TestTree
errorDetectionTests =
  testGroup "Error Detection Tests"
    [ testCase "Provide detailed ownership error messages" $
        let input = "// @ownership: true\nlet x = String(\"hello\")\nlet y = x\nlet len = length(x)"
            result = compile "test.typus" input
        in case result of
             Left errs -> do
               let ownershipErrs = filter isOwnershipError errs
               let formatted = formatOwnershipErrors ownershipErrs
               assertBool "Should include moved variable location" ("moved" `isInfixOf` formatted)
               assertBool "Should include current use location" ("used" `isInfixOf` formatted)
               assertBool "Should suggest fix" ("fix" `isInfixOf` formatted || "solution" `isInfixOf` formatted)
             Right _ -> assertFailure "Should have failed with ownership error"

    , testCase "Track ownership across function calls" $
        let input = "// @ownership: true\nfunc transfer(data: String) -> String { data }\nlet x = String(\"hello\")\nlet y = transfer(x)\nlet z = x"
            result = compile "test.typus" input
        in case result of
             Left errs -> do
               let ownershipErrs = filter isOwnershipError errs
               assertBool "Should track through function calls" (any tracksThroughFunctionCalls ownershipErrs)
               assertBool "Should identify transfer point" (any identifiesTransferPoint ownershipErrs)
             Right _ -> assertFailure "Should have failed with ownership tracking error"

    , testCase "Handle complex ownership scenarios" $
        let input = "// @ownership: true\nlet x = String(\"hello\")\nlet y = {\n  let z = x\n  String(\"world\") + z\n}"
            result = compile "test.typus" input
        in case result of
             Left errs -> do
               let ownershipErrs = filter isOwnershipError errs
               assertBool "Should handle complex scenarios" (any handlesComplexScenarios ownershipErrs)
               assertBool "Should provide clear error chain" (any providesClearErrorChain ownershipErrs)
             Right _ -> assertFailure "Should have failed with complex ownership error"
    ]

-- | QuickCheck properties for ownership memory safety
quickCheckProperties :: TestTree
quickCheckProperties =
  testGroup "QuickCheck Properties"
    [ testProperty "Ownership moves are tracked correctly" $
        forAll genOwnershipCode $ \code ->
            case compile "test.typus" code of
              Left errs -> 
                let ownershipErrs = filter isOwnershipError errs
                in property $ all tracksOwnershipCorrectly ownershipErrs
              Right _ -> property True

    , testProperty "Memory safety violations are detected" $
        forAll genUnsafeCode $ \code ->
            case compile "test.typus" code of
              Left errs -> 
                let ownershipErrs = filter isOwnershipError errs
                in property $ any detectsMemorySafetyViolation ownershipErrs
              Right _ -> property False  -- Should not succeed with unsafe code

    , testProperty "Borrow checker prevents conflicts" $
        forAll genBorrowCode $ \code ->
            case compile "test.typus" code of
              Left errs -> 
                let ownershipErrs = filter isOwnershipError errs
                in property $ borrowConflictsDetected ownershipErrs
              Right _ -> property True  -- Safe borrow code should succeed
    ]

-- | Helper functions for ownership error detection
isOwnershipError :: CompilerError -> Bool
isOwnershipError (CompilerError OwnershipError _ _ _) = True
isOwnershipError _ = False

isMoveError :: CompilerError -> Bool
isMoveError (CompilerError OwnershipError _ msg _) = "move" `isInfixOf` msg
isMoveError _ = False

tracksVariableMove :: String -> [CompilerError] -> Bool
tracksVariableMove var errs = any mentionsVariable var errs

mentionsVariable :: String -> CompilerError -> Bool
mentionsVariable var (CompilerError _ _ msg _) = var `isInfixOf` msg
mentionsVariable _ _ = False

isUseAfterMove :: CompilerError -> Bool
isUseAfterMove (CompilerError OwnershipError _ msg _) = "use after move" `isInfixOf` msg
isUseAfterMove _ = False

isDoubleFreeError :: CompilerError -> Bool
isDoubleFreeError (CompilerError OwnershipError _ msg _) = "double free" `isInfixOf` msg
isDoubleFreeError _ = False

tracksFreedResource :: String -> [CompilerError] -> Bool
tracksFreedResource var errs = any mentionsVariable var errs

isDanglingReference :: CompilerError -> Bool
isDanglingReference (CompilerError OwnershipError _ msg _) = "dangling" `isInfixOf` msg
isDanglingReference _ = False

explainsLifetimeIssue :: CompilerError -> Bool
explainsLifetimeIssue (CompilerError _ _ msg _) = "lifetime" `isInfixOf` msg
explainsLifetimeIssue _ = False

ensuresCleanup :: CompilerError -> Bool
ensuresCleanup (CompilerError OwnershipError _ msg _) = "cleanup" `isInfixOf` msg || "scope" `isInfixOf` msg
ensuresCleanup _ = False

tracksScopeBoundaries :: CompilerError -> Bool
tracksScopeBoundaries (CompilerError _ _ msg _) = "scope" `isInfixOf` msg
tracksScopeBoundaries _ = False

isParameterTransfer :: CompilerError -> Bool
isParameterTransfer (CompilerError OwnershipError _ msg _) = "parameter" `isInfixOf` msg && "transfer" `isInfixOf` msg
isParameterTransfer _ = False

isUseAfterTransfer :: CompilerError -> Bool
isUseAfterTransfer (CompilerError OwnershipError _ msg _) = "after transfer" `isInfixOf` msg
isUseAfterTransfer _ = False

handlesReturnValueTransfer :: CompilerError -> Bool
handlesReturnValueTransfer (CompilerError OwnershipError _ msg _) = "return" `isInfixOf` msg && "transfer" `isInfixOf` msg
handlesReturnValueTransfer _ = False

validatesMoveChain :: CompilerError -> Bool
validatesMoveChain (CompilerError OwnershipError _ msg _) = "move chain" `isInfixOf` msg
validatesMoveChain _ = False

hasCorrectMoveCount :: Int -> [CompilerError] -> Bool
hasCorrectMoveCount expected errs = length (filter isMoveError errs) >= expected

isBorrowConflict :: CompilerError -> Bool
isBorrowConflict (CompilerError OwnershipError _ msg _) = "borrow" `isInfixOf` msg && "conflict" `isInfixOf` msg
isBorrowConflict _ = False

explainsBorrowRules :: CompilerError -> Bool
explainsBorrowRules (CompilerError _ _ msg _) = "immutable" `isInfixOf` msg || "mutable" `isInfixOf` msg
explainsBorrowRules _ = False

handlesScopedBorrows :: CompilerError -> Bool
handlesScopedBorrows (CompilerError OwnershipError _ msg _) = "scope" `isInfixOf` msg && "borrow" `isInfixOf` msg
handlesScopedBorrows _ = False

isLifetimeMismatch :: CompilerError -> Bool
isLifetimeMismatch (CompilerError OwnershipError _ msg _) = "lifetime" `isInfixOf` msg && "mismatch" `isInfixOf` msg
isLifetimeMismatch _ = False

explainsLifetimeRequirements :: CompilerError -> Bool
explainsLifetimeRequirements (CompilerError _ _ msg _) = "requirement" `isInfixOf` msg || "constraint" `isInfixOf` msg
explainsLifetimeRequirements _ = False

handlesStructLifetimes :: CompilerError -> Bool
handlesStructLifetimes (CompilerError OwnershipError _ msg _) = "struct" `isInfixOf` msg && "lifetime" `isInfixOf` msg
handlesStructLifetimes _ = False

preventsLocalReferenceReturn :: CompilerError -> Bool
preventsLocalReferenceReturn (CompilerError OwnershipError _ msg _) = "local" `isInfixOf` msg && "reference" `isInfixOf` msg
preventsLocalReferenceReturn _ = False

explainsStackFrameIssue :: CompilerError -> Bool
explainsStackFrameIssue (CompilerError _ _ msg _) = "stack" `isInfixOf` msg || "frame" `isInfixOf` msg
explainsStackFrameIssue _ = False

tracksThroughFunctionCalls :: CompilerError -> Bool
tracksThroughFunctionCalls (CompilerError OwnershipError _ msg _) = "function" `isInfixOf` msg && "track" `isInfixOf` msg
tracksThroughFunctionCalls _ = False

identifiesTransferPoint :: CompilerError -> Bool
identifiesTransferPoint (CompilerError _ _ msg _) = "transfer" `isInfixOf` msg && "point" `isInfixOf` msg
identifiesTransferPoint _ = False

handlesComplexScenarios :: CompilerError -> Bool
handlesComplexScenarios (CompilerError OwnershipError _ msg _) = "complex" `isInfixOf` msg || "scenario" `isInfixOf` msg
handlesComplexScenarios _ = False

providesClearErrorChain :: CompilerError -> Bool
providesClearErrorChain (CompilerError _ _ msg _) = "chain" `isInfixOf` msg || "sequence" `isInfixOf` msg
providesClearErrorChain _ = False

tracksOwnershipCorrectly :: CompilerError -> Bool
tracksOwnershipCorrectly (CompilerError OwnershipError _ msg _) = "move" `isInfixOf` msg || "borrow" `isInfixOf` msg
tracksOwnershipCorrectly _ = False

detectsMemorySafetyViolation :: CompilerError -> Bool
detectsMemorySafetyViolation (CompilerError OwnershipError _ msg _) = any (`isInfixOf` msg) ["dangling", "double free", "use after move"]
detectsMemorySafetyViolation _ = False

borrowConflictsDetected :: [CompilerError] -> Bool
borrowConflictsDetected errs = any isBorrowConflict errs || all (not . isOwnershipError) errs

-- | Generators for QuickCheck testing
genOwnershipCode :: Gen String
genOwnershipCode = elements
  [ "// @ownership: true\nlet x = String(\"hello\")\nlet y = x"
  , "// @ownership: true\nlet x = 42\nlet y = x\nlet z = x"
  , "// @ownership: true\nfunc consume(data String) { process(data) }\nlet x = String(\"test\")\nconsume(x)"
  , "// @ownership: true\nlet x = String(\"hello\")\nlet y = &x"
  ]

genUnsafeCode :: Gen String
genUnsafeCode = elements
  [ "// @ownership: true\nlet x = allocate()\nfree(x)\nfree(x)"
  , "// @ownership: true\nlet x = String(\"hello\")\nlet y = x\nlet len = length(x)"
  , "// @ownership: true\nfunc bad_ref() -> &int { let x = 42; &x }"
  , "// @ownership: true\nlet x = 42\nlet y = &mut x\nlet z = &x"
  ]

genBorrowCode :: Gen String
genBorrowCode = elements
  [ "// @ownership: true\nlet x = 42\nlet y = &x\nlet z = &x"
  , "// @ownership: true\nlet x = String(\"hello\")\nlet y = &x"
  , "// @ownership: true\nlet x = 42\nlet y = { let z = &x; *z }"
  , "// @ownership: true\nstruct Ref<'a> { value: &'a int }\nlet x = 42\nlet r = Ref { value: &x }"
  ]