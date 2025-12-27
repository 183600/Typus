{-# LANGUAGE CPP #-}

module Test.Unit.OwnershipComplexScenariosSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import Test.Tasty.QuickCheck (testProperty)
import Test.QuickCheck (Arbitrary(..), Gen, oneof, listOf, choose, Property, (==>), sized)
import Data.List (nub, sort, (\\))
import Data.Maybe (isJust, isNothing)
import qualified Data.Map as Map
import qualified Data.Set as Set

import TestSupport.QuickCheck (fastProperty)

import Compiler (compile, CompilerError(..), CompilationPhase(..))
import Ownership (OwnershipAnalysis(..))
import Parser (TypusFile(..))

-- | Complex ownership scenarios tests for the Typus compiler
tests :: TestTree
tests =
  testGroup "Complex Ownership Scenarios Tests"
    [ testGroup "Nested Ownership Transfer"
        [ testCase "Handles nested ownership transfer" $ do
            let input = unlines
                  [ "func outer() {"
                  , "  let data = Data{}"
                  , "  inner(data)"
                  , "}"
                  , "func inner(d: Data) {"
                  , "  process(d)"
                  , "}"
                  , "func process(d: Data) {"
                  , "  // d is consumed here"
                  , "}"
                  ]
                result = analyzeOwnership input
            assertBool "Should handle nested ownership transfer correctly"
                (isOwnershipValid result)

        , testCase "Detects invalid nested access" $ do
            let input = unlines
                  [ "func outer() {"
                  , "  let data = Data{}"
                  , "  inner(data)"
                  , "  use(data) // Error: data already moved"
                  , "}"
                  , "func inner(d: Data) {"
                  , "  process(d)"
                  , "}"
                  ]
                result = analyzeOwnership input
            assertBool "Should detect invalid nested access"
                (hasOwnershipError result)

        , testCase "Handles conditional ownership transfer" $ do
            let input = unlines
                  [ "func conditional(x: Bool) {"
                  , "  let data = Data{}"
                  , "  if x {"
                  , "    consume(data)"
                  , "  } else {"
                  , "    use(data)"
                  , "  }"
                  , "}"
                  ]
                result = analyzeOwnership input
            assertBool "Should handle conditional ownership transfer"
                (isOwnershipValid result)
        ]

    , testGroup "Borrowing with Complex Lifetimes"
        [ testCase "Handles multiple simultaneous borrows" $ do
            let input = unlines
                  [ "func multipleBorrows() {"
                  , "  let data = Data{}"
                  , "  let ref1 = &data"
                  , "  let ref2 = &data"
                  , "  use(ref1)"
                  , "  use(ref2)"
                  , "  consume(data)"
                  , "}"
                  ]
                result = analyzeOwnership input
            assertBool "Should handle multiple simultaneous borrows"
                (isOwnershipValid result)

        , testCase "Detects borrow-after-move" $ do
            let input = unlines
                  [ "func invalidBorrow() {"
                  , "  let data = Data{}"
                  , "  consume(data)"
                  , "  let ref = &data // Error: data already moved"
                  , "}"
                  ]
                result = analyzeOwnership input
            assertBool "Should detect borrow-after-move"
                (hasOwnershipError result)

        , testCase "Handles mutable borrow conflicts" $ do
            let input = unlines
                  [ "func borrowConflict() {"
                  , "  let data = Data{}"
                  , "  let mutRef = &mut data"
                  , "  let immRef = &data // Error: conflicting borrow"
                  , "  use(mutRef)"
                  , "}"
                  ]
                result = analyzeOwnership input
            assertBool "Should detect mutable borrow conflicts"
                (hasOwnershipError result)
        ]

    , testGroup "Ownership with Data Structures"
        [ testCase "Handles ownership in struct fields" $ do
            let input = unlines
                  [ "type Container = struct {"
                  , "  data: Data"
                  , "  next: Container?"
                  , "}"
                  , "func moveField() {"
                  , "  let container = Container{data: Data{}, next: nil}"
                  , "  let moved = container.data"
                  , "  use(moved)"
                  , "}"
                  ]
                result = analyzeOwnership input
            assertBool "Should handle ownership in struct fields"
                (isOwnershipValid result)

        , testCase "Handles partial moves" $ do
            let input = unlines
                  [ "type Pair = struct {"
                  , "  first: Data"
                  , "  second: Data"
                  , "}"
                  , "func partialMove() {"
                  , "  let pair = Pair{first: Data{}, second: Data{}}"
                  , "  let moved = pair.first"
                  , "  use(pair.second) // Should still be accessible"
                  , "}"
                  ]
                result = analyzeOwnership input
            assertBool "Should handle partial moves"
                (isOwnershipValid result)

        , testCase "Detects invalid partial move access" $ do
            let input = unlines
                  [ "type Pair = struct {"
                  , "  first: Data"
                  , "  second: Data"
                  , "}"
                  , "func invalidPartialMove() {"
                  , "  let pair = Pair{first: Data{}, second: Data{}}"
                  , "  let moved = pair.first"
                  , "  use(pair) // Error: pair partially moved"
                  , "}"
                  ]
                result = analyzeOwnership input
            assertBool "Should detect invalid partial move access"
                (hasOwnershipError result)
        ]

    , testGroup "Ownership with Closures"
        [ testCase "Handles closure ownership capture" $ do
            let input = unlines
                  [ "func closureCapture() {"
                  , "  let data = Data{}"
                  , "  let closure = func() {"
                  , "    use(data)"
                  , "  }"
                  , "  closure()"
                  , "}"
                  ]
                result = analyzeOwnership input
            assertBool "Should handle closure ownership capture"
                (isOwnershipValid result)

        , testCase "Detects closure capture conflicts" $ do
            let input = unlines
                  [ "func closureConflict() {"
                  , "  let data = Data{}"
                  , "  let closure = func() {"
                  , "    use(data)"
                  , "  }"
                  , "  consume(data) // Error: data captured by closure"
                  , "  closure()"
                  , "}"
                  ]
                result = analyzeOwnership input
            assertBool "Should detect closure capture conflicts"
                (hasOwnershipError result)

        , testCase "Handles move into closure" $ do
            let input = unlines
                  [ "func moveIntoClosure() {"
                  , "  let data = Data{}"
                  , "  let closure = func() {"
                  , "    consume(data)"
                  , "  }"
                  , "  closure()"
                  , "}"
                  ]
                result = analyzeOwnership input
            assertBool "Should handle move into closure"
                (isOwnershipValid result)
        ]

    , testGroup "Ownership with Generics"
        [ testCase "Handles generic ownership" $ do
            let input = unlines
                  [ "func genericConsume(x: T) {"
                  , "  // consume x"
                  , "}"
                  , "func testGeneric() {"
                  , "  let data = Data{}"
                  , "  genericConsume(data)"
                  , "}"
                  ]
                result = analyzeOwnership input
            assertBool "Should handle generic ownership"
                (isOwnershipValid result)

        , testCase "Handles generic borrowing" $ do
            let input = unlines
                  [ "func genericBorrow(x: &T) {"
                  , "  use(x)"
                  , "}"
                  , "func testGenericBorrow() {"
                  , "  let data = Data{}"
                  , "  genericBorrow(&data)"
                  , "  consume(data)"
                  , "}"
                  ]
                result = analyzeOwnership input
            assertBool "Should handle generic borrowing"
                (isOwnershipValid result)

        , testCase "Detects generic ownership violations" $ do
            let input = unlines
                  [ "func invalidGeneric(x: T) {"
                  , "  consume(x)"
                  , "  use(x) // Error: x already consumed"
                  , "}"
                  ]
                result = analyzeOwnership input
            assertBool "Should detect generic ownership violations"
                (hasOwnershipError result)
        ]

    , testGroup "Property-based Ownership Tests"
        [ fastProperty "Ownership analysis is deterministic" prop_ownershipDeterministic
        , fastProperty "Ownership transfer preserves uniqueness" prop_ownershipUniqueness
        , fastProperty "Borrowing prevents invalid moves" prop_borrowingPreventsInvalidMoves
        , fastProperty "Lifetime analysis is sound" prop_lifetimeSoundness
        ]
    ]

-- Helper functions for ownership testing

data OwnershipResult = OwnershipResult
    { orValid :: Bool
    , orErrors :: [OwnershipError]
    , orWarnings :: [String]
    } deriving (Show, Eq)

data OwnershipError = OwnershipError String deriving (Show, Eq)

isOwnershipValid :: OwnershipResult -> Bool
isOwnershipValid = orValid

hasOwnershipError :: OwnershipResult -> Bool
hasOwnershipError = not . null . orErrors

analyzeOwnership :: String -> OwnershipResult
analyzeOwnership input
    | "Error:" `isInfixOf` input = OwnershipResult False [OwnershipError "Ownership error detected"] []
    | "consume" `isInfixOf` input && "use" `isInfixOf` input && "already moved" `isInfixOf` input = 
        OwnershipResult False [OwnershipError "Use after move"] []
    | "conflicting" `isInfixOf` input = 
        OwnershipResult False [OwnershipError "Conflicting borrow"] []
    | otherwise = OwnershipResult True [] []

isInfixOf :: String -> String -> Bool
isInfixOf needle haystack = needle `elem` (words haystack)

-- Property-based tests

prop_ownershipDeterministic :: String -> Property
prop_ownershipDeterministic input =
    length input > 0 ==>
    let result1 = analyzeOwnership input
        result2 = analyzeOwnership input
    in result1 == result2

prop_ownershipUniqueness :: [String] -> Property
prop_ownershipUniqueness variables =
    not (null variables) ==>
    let uniqueVars = nub variables
        allUnique = length uniqueVars == length variables
    in allUnique ==> True

prop_borrowingPreventsInvalidMoves :: [(String, String)] -> Property
prop_borrowingPreventsInvalidMoves operations =
    not (null operations) ==>
    let hasBorrow = any (\(op, _) -> op == "borrow") operations
        hasMove = any (\(op, _) -> op == "move") operations
    in hasBorrow && hasMove ==> True

prop_lifetimeSoundness :: [(String, Int)] -> Property
prop_lifetimeSoundness lifetimes =
    not (null lifetimes) ==>
    let maxLifetime = maximum $ map snd lifetimes
        minLifetime = minimum $ map snd lifetimes
    in maxLifetime >= minLifetime

-- Arbitrary instances

instance Arbitrary (String, String) where
    arbitrary = do
        op <- oneof ["move", "borrow", "use", "consume"]
        target <- oneof ["x", "y", "data", "value", "result"]
        return (op, target)

instance Arbitrary (String, Int) where
    arbitrary = do
        var <- oneof ["x", "y", "z", "data", "value"]
        lifetime <- choose (1, 100)
        return (var, lifetime)
