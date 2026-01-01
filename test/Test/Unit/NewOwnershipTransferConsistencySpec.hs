{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewOwnershipTransferConsistencySpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import TestSupport.Arbitrary

import Ownership
  ( OwnershipType(..)
  , OwnershipError(..)
  , OwnershipTransfer(..)
  , OwnershipAnalyzer
  , newOwnershipAnalyzer
  , analyzeOwnership
  , analyzeOwnershipDebug
  , formatOwnershipErrors
  )
import qualified Data.Map.Strict as Map
import Data.List (sort, nub)
import Data.Maybe (isJust, isNothing)

-- | Ownership transfer consistency tests
tests :: TestTree
tests =
  testGroup "New Ownership Transfer Consistency Tests"
    [ testGroup "Basic ownership transfer invariants"
        [ testCase "owned to owned transfer maintains consistency" $ do
            let analyzer = newOwnershipAnalyzer
                code = unlines
                  [ "func main() {"
                  , "    x := 42"
                  , "    y := x"  -- transfer ownership
                  , "    z := y"  -- another transfer
                  , "}"
                  ]
                result = analyzeOwnership analyzer code
            case result of
              (errors, []) -> assertFailure $ "Expected analysis results, got errors: " ++ show errors
              (errors, transfers) -> do
                -- Should have 2 ownership transfers
                L.length transfers @?= 2
                -- Transfers should form a consistent chain
                let fromVars = sort $ map transferFrom transfers
                    toVars = sort $ map transferTo transfers
                fromVars @?= ["x", "y"]
                toVars @?= ["y", "z"]
                
        , testCase "borrow transfer maintains source reference" $ do
            let analyzer = newOwnershipAnalyzer
                code = unlines
                  [ "func main() {"
                  , "    x := 42"
                  , "    y := &x"  -- borrow
                  , "    z := y"   -- transfer borrow
                  , "}"
                  ]
                result = analyzeOwnership analyzer code
            case result of
              (errors, []) -> assertFailure $ "Expected analysis results, got errors: " ++ show errors
              (errors, transfers) -> do
                L.length transfers @?= 1  -- Only z := y is a transfer
                transferFrom (L.head transfers) @?= "y"
                transferTo (L.head transfers) @?= "z"
        ]
        
    , testGroup "Ownership transfer cycles detection"
        [ testCase "detects simple ownership cycles" $ do
            let analyzer = newOwnershipAnalyzer
                code = unlines
                  [ "func main() {"
                  , "    x := 42"
                  , "    y := x"
                  , "    x := y"  -- creates potential cycle
                  , "}"
                  ]
                result = analyzeOwnership analyzer code
            case result of
              (errors, transfers) -> do
                -- Should detect ownership inconsistency
                let cycleErrors = filter isCycleError errors
                L.length cycleErrors @>= 1
          where
            isCycleError (UseAfterMove _) = True
            isCycleError (DoubleMove _ _) = True
            isCycleError _ = False
            
        , testCase "detects complex ownership cycles" $ do
            let analyzer = newOwnershipAnalyzer
                code = unlines
                  [ "func main() {"
                  , "    a := 1"
                  , "    b := a"
                  , "    c := b"
                  , "    d := c"
                  , "    a := d"  -- creates cycle: a->b->c->d->a
                  , "}"
                  ]
                result = analyzeOwnership analyzer code
            case result of
              (errors, transfers) -> do
                -- Should detect multiple ownership issues
                let moveErrors = filter isMoveError errors
                L.length moveErrors @>= 1
          where
            isMoveError (UseAfterMove _) = True
            isMoveError (DoubleMove _ _) = True
            isMoveError _ = False
        ]
        
    , testGroup "Borrow transfer consistency"
        [ testCase "immutable borrow transfers preserve read-only access" $ do
            let analyzer = newOwnershipAnalyzer
                code = unlines
                  [ "func main() {"
                  , "    x := 42"
                  , "    y := &x"     -- immutable borrow
                  , "    z := y"      -- transfer borrow
                  , "    println(*z)" -- should be allowed
                  , "}"
                  ]
                result = analyzeOwnership analyzer code
            case result of
              (errors, transfers) -> do
                -- Should not have borrow-related errors
                let borrowErrors = filter isBorrowError errors
                L.length borrowErrors @?= 0
                L.length transfers @?= 1  -- z := y transfer
          where
            isBorrowError (BorrowWhileMoved _) = True
            isBorrowError (MutBorrowWhileBorrowed _) = True
            isBorrowError (BorrowWhileMutBorrowed _) = True
            isBorrowError (MultipleMutBorrows _) = True
            isBorrowError _ = False
            
        , testCase "mutable borrow transfers enforce exclusivity" $ do
            let analyzer = newOwnershipAnalyzer
                code = unlines
                  [ "func main() {"
                  , "    x := 42"
                  , "    y := &mut x"  -- mutable borrow
                  , "    z := y"       -- transfer mutable borrow
                  , "    *z = 100"      -- modify through transferred borrow
                  , "}"
                  ]
                result = analyzeOwnership analyzer code
            case result of
              (errors, transfers) -> do
                -- Should allow mutable operations through transferred borrow
                let mutBorrowErrors = filter isMutBorrowError errors
                L.length mutBorrowErrors @?= 0
                L.length transfers @?= 1
          where
            isMutBorrowError (MutBorrowWhileBorrowed _) = True
            isMutBorrowError (UseWhileMutBorrowed _) = True
            isMutBorrowError _ = False
        ]
        
    , testGroup "Cross-scope transfer consistency"
        [ testCase "ownership transfer across function boundaries" $ do
            let analyzer = newOwnershipAnalyzer
                code = unlines
                  [ "func consume(value int) {"
                  , "    println(value)"
                  , "}"
                  , ""
                  , "func main() {"
                  , "    x := 42"
                  , "    consume(x)"  -- transfer to function
                  , "}"
                  ]
                result = analyzeOwnership analyzer code
            case result of
              (errors, transfers) -> do
                -- Should detect cross-function transfer
                let crossFunctionErrors = filter isCrossFunctionError errors
                L.length crossFunctionErrors @>= 1
          where
            isCrossFunctionError (CrossFunctionMove _ _) = True
            isCrossFunctionError (ParameterMoveMismatch _) = True
            isCrossFunctionError _ = False
            
        , testCase "borrow transfer across function boundaries" $ do
            let analyzer = newOwnershipAnalyzer
                code = unlines
                  [ "func read_ref(ref &int) {"
                  , "    println(*ref)"
                  , "}"
                  , ""
                  , "func main() {"
                  , "    x := 42"
                  , "    read_ref(&x)"  -- borrow transfer to function
                  , "    println(x)"    -- should still be usable
                  , "}"
                  ]
                result = analyzeOwnership analyzer code
            case result of
              (errors, transfers) -> do
                -- Should allow borrow transfer without ownership loss
                let borrowErrors = filter isBorrowError errors
                L.length borrowErrors @?= 0
        ]
        
    , testGroup "Transfer consistency with control flow"
        [ testCase "ownership transfer in conditional branches" $ do
            let analyzer = newOwnershipAnalyzer
                code = unlines
                  [ "func main() {"
                  , "    x := 42"
                  , "    if condition {"
                  , "        y := x"  -- conditional transfer
                  , "        println(y)"
                  , "    } else {"
                  , "        z := x"  -- alternative transfer
                  , "        println(z)"
                  , "    }"
                  , "}"
                  ]
                result = analyzeOwnership analyzer code
            case result of
              (errors, transfers) -> do
                -- Should handle conditional transfers correctly
                let controlFlowErrors = filter isControlFlowError errors
                L.length controlFlowErrors @?= 0
          where
            isControlFlowError (ControlFlowError _) = True
            isControlFlowError _ = False
            
        , testCase "ownership transfer in loops" $ do
            let analyzer = newOwnershipAnalyzer
                code = unlines
                  [ "func main() {"
                  , "    items := []int{1, 2, 3}"
                  , "    for i, item := range items {"
                  , "        processed := item"  -- transfer in loop
                  , "        println(processed)"
                  , "    }"
                  , "}"
                  ]
                result = analyzeOwnership analyzer code
            case result of
              (errors, transfers) -> do
                -- Should handle loop transfers correctly
                let loopErrors = filter isLoopError errors
                L.length loopErrors @?= 0
                -- Should have transfers for each iteration
                L.length transfers @>= 1
          where
            isLoopError (LoopOwnershipError _) = True
            isLoopError _ = False
        ]
        
    , testGroup "Transfer consistency validation"
        [ testCase "transfer graph is acyclic" $ do
            let analyzer = newOwnershipAnalyzer
                code = unlines
                  [ "func main() {"
                  , "    a := 1"
                  , "    b := a"
                  , "    c := b"
                  , "    d := c"
                  , "}"
                  ]
                result = analyzeOwnership analyzer code
            case result of
              (errors, transfers) -> do
                -- Build transfer graph L.and check for cycles
                let graph = buildTransferGraph transfers
                    hasCycles = detectGraphCycles graph
                hasCycles @?= False
                
        , testCase "transfer chain preserves ownership semantics" $ do
            let analyzer = newOwnershipAnalyzer
                code = unlines
                  [ "func main() {"
                  , "    original := 42"
                  , "    first := original"
                  , "    second := first"
                  , "    third := second"
                  , "    println(third)"  -- final use
                  , "}"
                  ]
                result = analyzeOwnership analyzer code
            case result of
              (errors, transfers) -> do
                -- Should maintain consistent ownership chain
                L.length transfers @?= 3
                let chain = buildTransferChain transfers
                validateOwnershipChain chain @?= True
        ]
    ]

-- Helper functions for transfer analysis
buildTransferGraph :: [OwnershipTransfer] -> Map.Map String [String]
buildTransferGraph transfers = 
    Map.fromListWith (++) $ L.map (\t -> (transferFrom t, [transferTo t])) transfers

detectGraphCycles :: Map.Map String [String] -> Bool
detectGraphCycles graph = 
    let visited = []
        recStack = []
    in hasCycle' visited recStack (Map.keys graph)
  where
    hasCycle' _ _ [] = False
    hasCycle' visited recStack (v:vs) =
        if v `elem` recStack then True
        else if v `elem` visited then hasCycle' visited recStack vs
        else
            let neighbors = Map.findWithDefault [] v graph
            in hasCycle' (v:visited) (v:recStack) neighbors || 
               hasCycle' visited recStack vs

buildTransferChain :: [OwnershipTransfer] -> [(String, String)]
buildTransferChain transfers = 
    L.map (\t -> (transferFrom t, transferTo t)) transfers

validateOwnershipChain :: [(String, String)] -> Bool
validateOwnershipChain chain = 
    let sources = map fst chain
        targets = map snd chain
        -- Check that each target appears as source at most once
        targetCounts = L.map (\t -> L.length $ L.filter (== t) targets) (nub targets)
    in L.all (<= 1) targetCounts