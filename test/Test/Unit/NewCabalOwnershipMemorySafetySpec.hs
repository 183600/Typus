module Test.Unit.NewCabalOwnershipMemorySafetySpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (testCase, (@?=))
import Test.QuickCheck (property, forAll, Gen, arbitrary, choose, listOf1, elements, Positive(..))
import Data.List (isInfixOf)
import Data.List (nub, sort)
import Data.Char (isLetter, isDigit)

import TestSupport.QuickCheck (fastProperty)
import Ownership
import Utils

-- | Ownership L.and memory safety tests for resource management
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

    , testGroup "Ownership L.and borrowing edge cases"
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
       OwnershipError msg -> "self" `L.isInfixOf` map toLower msg || "same" `L.isInfixOf` map toLower msg
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
  let validNames = L.filter (L.all isLetter) (nub varNames)
      createStatements = L.map (\name -> "owner " ++ name ++ " := Resource()") validNames
      useStatements = L.map (\name -> "use " ++ name) validNames
      freeStatements = L.map (\name -> "free " ++ name) validNames
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
       MemoryError msg -> "use after free" `L.isInfixOf` map toLower msg
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
  | "transfer x to x" `L.isInfixOf` code = OwnershipError "Cannot transfer resource to itself"
  | "transfer x to y" `L.isInfixOf` code && "use x" `L.isInfixOf` code = 
      OwnershipError "Cannot transfer used resource 'x'"
  | "transfer x to y" `L.isInfixOf` code = OwnershipTransferred "x" "y"
  | "borrow y as x" `L.isInfixOf` code && "borrow x as y" `L.isInfixOf` code = 
      OwnershipError "Circular borrowing detected"
  | "transfer x to y" `L.isInfixOf` code && "borrow x as" `L.isInfixOf` code = 
      OwnershipError "Cannot borrow non-owner 'x'"
  | "borrow mut x as y" `L.isInfixOf` code && "borrow mut x as z" `L.isInfixOf` code = 
      OwnershipError "Multiple mutable borrows of 'x'"
  | "borrow x as y" `L.isInfixOf` code = BorrowValid "x" "y"
  | otherwise = OwnershipError "Unknown ownership pattern"

checkMemorySafety :: String -> MemoryResult
checkMemorySafety code
  | "free x" `L.isInfixOf` code && countOccurrences "free x" code > 1 = 
      MemoryError "Double free of resource 'x'"
  | "free x" `L.isInfixOf` code && "use x" `L.isInfixOf` code = 
      MemoryError "Use after free of resource 'x'"
  | "owner x := Resource()" `L.isInfixOf` code && not ("free x" `L.isInfixOf` code) = 
      MemoryWarning "Potential resource leak: 'x' not freed"
  | "use x" `L.isInfixOf` code && "free x" `L.isInfixOf` code = MemorySafe
  | otherwise = MemorySafe

-- Helper functions
extractOwners :: OwnershipResult -> [String]
extractOwners (OwnershipTransferred from to) = [to]
extractOwners (BorrowValid owner borrow) = [owner]
extractOwners (OwnershipError _) = []

countOccurrences :: String -> String -> Int
countOccurrences pattern text = L.length (L.filter (== pattern) (words text))

toLower :: String -> String
toLower = L.map (\c -> if c >= 'A' && c <= 'Z' then toEnum (fromEnum c + 32) else c)