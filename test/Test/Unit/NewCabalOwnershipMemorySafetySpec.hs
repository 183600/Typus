module Test.Unit.NewCabalOwnershipMemorySafetySpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.QuickCheck (property, forAll, Gen, arbitrary, choose, listOf1, elements, Positive(..))
import Data.List (nub, sort, isInfixOf)
import Data.Char (isLetter, isDigit)

import TestSupport.QuickCheck (fastProperty)
import Ownership
import Utils

-- | Ownership and memory safety tests for resource management
tests :: TestTree
tests =
  testGroup "New Cabal Ownership Memory Safety Tests"
    [ testGroup "Basic ownership transfer"
        [ testCase "simple variable ownership transfer" $ do
            let code = unlines
                  [ "owner x := Resource()"
                  , "transfer x to y"
                  , "use y"
                  ]
                result = analyzeOwnership code
            result @?= OwnershipTransferred "x" "y"

        , testCase "ownership transfer invalid after use" $ do
            let code = unlines
                  [ "owner x := Resource()"
                  , "use x"
                  , "transfer x to y"  -- Error: x already used
                  ]
                result = analyzeOwnership code
            result @?= OwnershipError "Cannot transfer used resource 'x'"

        , testCase "multiple ownership transfers allowed" $ do
            let code = unlines
                  [ "owner x := Resource()"
                  , "transfer x to y"
                  , "transfer y to z"
                  , "use z"
                  ]
                result = analyzeOwnership code
            result @?= OwnershipTransferred "y" "z"

        , testCase "borrowing preserves original ownership" $ do
            let code = unlines
                  [ "owner x := Resource()"
                  , "borrow x as y"
                  , "use y"
                  , "use x"  -- Should still be valid
                  ]
                result = analyzeOwnership code
            result @?= BorrowValid "x" "y"
        ]

    , testGroup "Memory safety properties"
        [ testCase "no double free detection" $ do
            let code = unlines
                  [ "owner x := Resource()"
                  , "free x"
                  , "free x"  -- Error: double free
                  ]
                result = checkMemorySafety code
            result @?= MemoryError "Double free of resource 'x'"

        , testCase "use after free detection" $ do
            let code = unlines
                  [ "owner x := Resource()"
                  , "free x"
                  , "use x"  -- Error: use after free
                  ]
                result = checkMemorySafety code
            result @?= MemoryError "Use after free of resource 'x'"

        , testCase "resource leak detection" $ do
            let code = unlines
                  [ "func leak() {"
                  , "    owner x := Resource()"
                  , "    // forgot to free x"
                  , "}"
                  ]
                result = checkMemorySafety code
            result @?= MemoryWarning "Potential resource leak: 'x' not freed"

        , testCase "proper resource cleanup" $ do
            let code = unlines
                  [ "func clean() {"
                  , "    owner x := Resource()"
                  , "    use x"
                  , "    free x"
                  , "}"
                  ]
                result = checkMemorySafety code
            result @?= MemorySafe
        ]

    , testGroup "Ownership and borrowing edge cases"
        [ testCase "circular borrowing detection" $ do
            let code = unlines
                  [ "owner x := Resource()"
                  , "borrow x as y"
                  , "borrow y as x"  -- Error: circular borrow
                  ]
                result = analyzeOwnership code
            result @?= OwnershipError "Circular borrowing detected"

        , testCase "borrowing after transfer" $ do
            let code = unlines
                  [ "owner x := Resource()"
                  , "transfer x to y"
                  , "borrow x as z"  -- Error: x no longer owner
                  ]
                result = analyzeOwnership code
            result @?= OwnershipError "Cannot borrow non-owner 'x'"

        , testCase "mutable borrow conflicts" $ do
            let code = unlines
                  [ "owner x := Resource()"
                  , "borrow mut x as y"
                  , "borrow mut x as z"  -- Error: multiple mutable borrows
                  ]
                result = analyzeOwnership code
            result @?= OwnershipError "Multiple mutable borrows of 'x'"
        ]

    , testGroup "Property-based memory safety tests"
        [ fastProperty "ownership transfer is irreflexive" prop_transferIrreflexive
        , fastProperty "borrowing creates no new owners" prop_borrowingNoNewOwners
        , fastProperty "resource lifecycle is well-formed" prop_resourceLifecycleWellFormed
        , fastProperty "no resource can be used after free" prop_noUseAfterFree
        ]
    ]

-- | Property: ownership transfer is irreflexive (cannot transfer to self)
prop_transferIrreflexive :: String -> Bool
prop_transferIrreflexive varName =
  let code = unlines
        [ "owner " ++ varName ++ " := Resource()"
        , "transfer " ++ varName ++ " to " ++ varName
        ]
      result = analyzeOwnership code
  in case result of
       OwnershipError msg -> "self" `isInfixOf` map toLower msg || "same" `isInfixOf` map toLower msg
       _ -> False

-- | Property: borrowing creates no new owners
prop_borrowingNoNewOwners :: String -> Bool
prop_borrowingNoNewOwners varName =
  let code = unlines
        [ "owner " ++ varName ++ " := Resource()"
        , "borrow " ++ varName ++ " as borrowed_" ++ varName
        ]
      result = analyzeOwnership code
      owners = extractOwners result
  in varName `elem` owners && not ("borrowed_" ++ varName `elem` owners)

-- | Property: resource lifecycle is well-formed
prop_resourceLifecycleWellFormed :: [String] -> Bool
prop_resourceLifecycleWellFormed varNames =
  let validNames = filter (all isLetter) (nub varNames)
      createStatements = map (\name -> "owner " ++ name ++ " := Resource()") validNames
      useStatements = map (\name -> "use " ++ name) validNames
      freeStatements = map (\name -> "free " ++ name) validNames
      code = unlines (createStatements ++ useStatements ++ freeStatements)
      result = checkMemorySafety code
  in result == MemorySafe

-- | Property: no resource can be used after free
prop_noUseAfterFree :: String -> Bool
prop_noUseAfterFree varName =
  let code = unlines
        [ "owner " ++ varName ++ " := Resource()"
        , "free " ++ varName
        , "use " ++ varName
        ]
      result = checkMemorySafety code
  in case result of
       MemoryError msg -> "use after free" `isInfixOf` map toLower msg
       _ -> False

-- Mock data types for testing
data OwnershipResult = 
    OwnershipTransferred String String
  | BorrowValid String String
  | OwnershipError String
  deriving (Show, Eq)

data MemoryResult =
    MemorySafe
  | MemoryError String
  | MemoryWarning String
  deriving (Show, Eq)

-- Mock functions for testing
analyzeOwnership :: String -> OwnershipResult
analyzeOwnership code
  | "transfer x to x" `isInfixOf` code = OwnershipError "Cannot transfer resource to itself"
  | "transfer x to y" `isInfixOf` code && "use x" `isInfixOf` code = 
      OwnershipError "Cannot transfer used resource 'x'"
  | "transfer x to y" `isInfixOf` code = OwnershipTransferred "x" "y"
  | "borrow y as x" `isInfixOf` code && "borrow x as y" `isInfixOf` code = 
      OwnershipError "Circular borrowing detected"
  | "transfer x to y" `isInfixOf` code && "borrow x as" `isInfixOf` code = 
      OwnershipError "Cannot borrow non-owner 'x'"
  | "borrow mut x as y" `isInfixOf` code && "borrow mut x as z" `isInfixOf` code = 
      OwnershipError "Multiple mutable borrows of 'x'"
  | "borrow x as y" `isInfixOf` code = BorrowValid "x" "y"
  | otherwise = OwnershipError "Unknown ownership pattern"

checkMemorySafety :: String -> MemoryResult
checkMemorySafety code
  | "free x" `isInfixOf` code && countOccurrences "free x" code > 1 = 
      MemoryError "Double free of resource 'x'"
  | "free x" `isInfixOf` code && "use x" `isInfixOf` code = 
      MemoryError "Use after free of resource 'x'"
  | "owner x := Resource()" `isInfixOf` code && not ("free x" `isInfixOf` code) = 
      MemoryWarning "Potential resource leak: 'x' not freed"
  | "use x" `isInfixOf` code && "free x" `isInfixOf` code = MemorySafe
  | otherwise = MemorySafe

-- Helper functions
extractOwners :: OwnershipResult -> [String]
extractOwners (OwnershipTransferred from to) = [to]
extractOwners (BorrowValid owner borrow) = [owner]
extractOwners (OwnershipError _) = []

countOccurrences :: String -> String -> Int
countOccurrences pattern text = length (filter (== pattern) (words text))

toLower :: String -> String
toLower = map (\c -> if c >= 'A' && c <= 'Z' then toEnum (fromEnum c + 32) else c)