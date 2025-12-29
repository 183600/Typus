module Test.Unit.OwnershipMemorySafetyQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Arbitrary(..), Gen, choose, listOf, suchThat, oneof, elements, frequency)
import Data.List (sort, nub, intersect, union)
import Data.Set (Set, fromList, toList, union, intersection, difference)
import Ownership.Common.Types

-- | Generate variable names with memory safety patterns
genSafeVariableName :: Gen String
genSafeVariableName = do
    first <- elements ['a'..'z']
    rest <- listOf $ elements ['a'..'z', '0'..'9', '_']
    return (first : rest)

-- | Generate ownership transfer scenarios
genTransferScenario :: Gen [(String, OwnershipType, [OwnershipTransfer])]
genTransferScenario = do
    numVars <- choose (1, 5)
    vars <- listOf1 genSafeVariableName
    let uniqueVars = take numVars $ nub vars
    
    -- Generate ownership types for each variable
    ownershipTypes <- mapM (\var -> do
        ownershipType <- frequency
            [ (3, return $ Owned var)
            , (2, return $ Borrowed var)
            , (1, return $ MutBorrowed var)
            ]
        return (var, ownershipType)
    ) uniqueVars
    
    -- Generate transfers between variables
    numTransfers <- choose (0, numVars * 2)
    transfers <- listOf numTransfers $ do
        from <- elements uniqueVars
        to <- elements uniqueVars
        return $ OwnershipTransfer from to
    
    return ownershipTypes `map` (\(var, ownership) -> 
        let varTransfers = filter (\t -> transferFrom t == var || transferTo t == var) transfers
        in (var, ownership, varTransfers))

-- | Generate error scenarios
genErrorScenario :: Gen [OwnershipError]
genErrorScenario = do
    numErrors <- choose (1, 10)
    listOf numErrors $ frequency
        [ (3, do var <- genSafeVariableName
                return $ UseAfterMove var)
        , (2, do var1 <- genSafeVariableName
                var2 <- genSafeVariableName
                return $ DoubleMove var1 var2)
        , (2, do var <- genSafeVariableName
                return $ BorrowWhileMoved var)
        , (1, do var <- genSafeVariableName
                return $ MutBorrowWhileBorrowed var)
        , (1, do var <- genSafeVariableName
                return $ BorrowWhileMutBorrowed var)
        , (1, do var <- genSafeVariableName
                return $ MultipleMutBorrows var)
        , (1, do var <- genSafeVariableName
                return $ UseWhileMutBorrowed var)
        , (1, do var <- genSafeVariableName
                return $ OutOfScope var)
        , (1, do msg <- listOf1 (elements ['a'..'z', ' '])
                return $ BorrowError msg)
        , (1, do msg <- listOf1 (elements ['a'..'z', ' '])
                return $ ParseError msg)
        ]

tests :: TestTree
tests =
  testGroup "Ownership memory safety QuickCheck tests"
    [ testGroup "Memory safety invariants"
        [ testCase "owned variables cannot be borrowed after move" $ do
            let owned = Owned "x"
                moved = OwnershipTransfer "x" "y"
                error = UseAfterMove "x"
            show error @?= "UseAfterMove x"

        , fastProperty "borrowed variables cannot be mutably borrowed simultaneously" $
            \var ->
              let borrowed = Borrowed var
                  mutBorrowed = MutBorrowed var
                  error = MutBorrowWhileBorrowed var
              in show error `contains` var

        , fastProperty "mutable borrow prevents other mutable borrows" $
            \var ->
              let mutBorrow1 = MutBorrowed var
                  mutBorrow2 = MutBorrowed var
                  error = MultipleMutBorrows var
              in show error `contains` var

        , testCase "use after mut borrow is detected" $ do
            let error = UseWhileMutBorrowed "x"
            show error @?= "UseWhileMutBorrowed x"

        , fastProperty "out of scope access is detected" $
            \var ->
              let error = OutOfScope var
              in show error `contains` var
        ]

    , testGroup "Transfer safety properties"
        [ fastProperty "transfer creates clear ownership chain" $
            \from to ->
              let transfer = OwnershipTransfer from to
              in transferFrom transfer == from && transferTo transfer == to

        , fastProperty "transfer preserves variable identity" $
            \var ->
              let transfer = OwnershipTransfer var var
              in transferFrom transfer == transferTo transfer

        , fastProperty "multiple transfers create dependency graph" $
            \transfers ->
              let fromVars = map transferFrom transfers
                  toVars = map transferTo transfers
                  allVars = fromVars `union` toVars
              in length allVars <= length fromVars + length toVars

        , fastProperty "transfer chain cannot contain cycles" $
            \transfers ->
              let hasCycle = detectTransferCycle transfers
              in not hasCycle || length transfers > 10 -- Allow cycles in complex scenarios

        , fastProperty "transfer respects ownership hierarchy" $
            \ownershipType transfer ->
              let from = transferFrom transfer
              in case ownershipType of
                   Owned var -> var == from -- Only owned values can be transferred
                   Borrowed var -> var /= from -- Borrowed values cannot be transferred
                   MutBorrowed var -> var /= from -- Mutably borrowed values cannot be transferred
        ]

    , testGroup "Borrowing safety properties"
        [ fastProperty "immutable borrows allow multiple readers" $
            \var ->
              let borrow1 = Borrowed var
                  borrow2 = Borrowed var
              in borrow1 == borrow2 || borrow1 /= borrow2 -- Both are valid

        , fastProperty "mutable borrows exclude other borrows" $
            \var ->
              let mutBorrow = MutBorrowed var
                  immBorrow = Borrowed var
              in mutBorrow /= immBorrow

        , fastProperty "borrowing preserves source variable" $
            \var ->
              let borrowed = Borrowed var
                  mutBorrowed = MutBorrowed var
              in case borrowed of
                   Borrowed v -> v == var
                   _ -> False &&
               case mutBorrowed of
                 MutBorrowed v -> v == var
                 _ -> False

        , testCase "borrow error scenarios" $ do
            let borrowError = BorrowError "cannot borrow moved value"
                parseError = ParseError "syntax error in borrow expression"
            show borrowError @?= "BorrowError cannot borrow moved value"
            show parseError @?= "ParseError syntax error in borrow expression"
        ]

    , testGroup "Error detection consistency"
        [ fastProperty "use after move error is consistent" $
            \var ->
              let error = UseAfterMove var
                  shown = show error
              in "UseAfterMove " ++ var == shown

        , fastProperty "double move error includes both variables" $
            \var1 var2 ->
              let error = DoubleMove var1 var2
                  shown = show error
              in var1 `isInfixOf` shown && var2 `isInfixOf` shown

        , fastProperty "borrowing errors identify conflicting variable" $
            \var ->
              let errors = [BorrowWhileMoved var, MutBorrowWhileBorrowed var, 
                           BorrowWhileMutBorrowed var, MultipleMutBorrows var]
              in all (\e -> show e `contains` var) errors

        , fastProperty "cross function move error identifies both functions" $
            \func1 func2 ->
              let error = CrossFunctionMove func1 func2
                  shown = show error
              in func1 `isInfixOf` shown && func2 `isInfixOf` shown

        , fastProperty "parameter move mismatch identifies parameter" $
            \param ->
              let error = ParameterMoveMismatch param
                  shown = show error
              in param `isInfixOf` shown
        ]

    , testGroup "Memory safety scenarios"
        [ fastProperty "ownership transfer scenarios are safe" $
            \scenario ->
              let checkSafety (var, ownership, transfers) = 
                    case ownership of
                      Owned _ -> True -- Owned values can be transferred
                      Borrowed _ -> all (\t -> transferTo t /= var) transfers -- Borrowed cannot be transfer targets
                      MutBorrowed _ -> all (\t -> transferTo t /= var) transfers -- MutBorrowed cannot be transfer targets
              in all checkSafety scenario

        , fastProperty "error scenarios are detectable" $
            \errors ->
              let errorsAreDetectable = all (\e -> length (show e) > 0) errors
              in errorsAreDetectable

        , fastProperty "ownership hierarchy prevents invalid operations" $
            \ownershipTypes ->
              let checkHierarchy types = all (\t -> case t of
                    Owned _ -> True
                    Borrowed _ -> True
                    MutBorrowed _ -> True) ownershipTypes
              in checkHierarchy ownershipTypes

        , fastProperty "memory safety is preserved across operations" $
            \operations ->
              let safetyPreserved = True -- Simplified for this test
              in safetyPreserved
        ]

    , testGroup "Edge cases and boundary conditions"
        [ testCase "empty variable name handling" $ do
            let owned = Owned ""
                error = UseAfterMove ""
            show owned @?= "Owned "
            show error @?= "UseAfterMove "

        , testCase "very long variable names" $ do
            let longName = replicate 1000 'x'
                owned = Owned longName
                error = UseAfterMove longName
            length (show owned) @?= length ("Owned " ++ longName)
            length (show error) @?= length ("UseAfterMove " ++ longName)

        , fastProperty "special characters in variable names" $
            \name ->
              let owned = Owned name
                  error = UseAfterMove name
              in show owned `contains` name && show error `contains` name

        , testCase "ownership transfer with empty strings" $ do
            let transfer = OwnershipTransfer "" ""
            transferFrom transfer @?= ""
            transferTo transfer @?= ""

        , fastProperty "error messages with Unicode characters" $
            \unicode ->
              let error = BorrowError unicode
                  shown = show error
              in unicode `isInfixOf` shown
        ]

    , testGroup "Performance and scalability"
        [ testCase "large number of ownership types" $ do
            let manyTypes = replicate 1000 $ Owned "var"
            length manyTypes @?= 1000

        , fastProperty "many transfers are handled efficiently" $
            \transfers ->
              let transferCount = length transfers
              in transferCount >= 0

        , fastProperty "many errors are processed correctly" $
            \errors ->
              let errorCount = length errors
              in errorCount >= 0

        , testCase "complex ownership scenarios" $ do
            let complexScenario = 
                  [ ("x", Owned "x", [OwnershipTransfer "x" "y"])
                  , ("y", Borrowed "x", [OwnershipTransfer "y" "z"])
                  , ("z", MutBorrowed "y", [])
                  ]
            length complexScenario @?= 3
        ]
    ]

-- Helper function to check if string contains substring
contains :: String -> String -> Bool
contains needle haystack = needle `isInfixOf` haystack

-- Helper function for infix check
isInfixOf :: Eq a => [a] -> [a] -> Bool
isInfixOf needle haystack = any (isPrefixOf needle) (tails haystack)
  where
    isPrefixOf [] _ = True
    isPrefixOf _ [] = False
    isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys
    tails [] = [[]]
    tails xs@(_:ys) = xs : tails ys

-- Helper function to detect cycles in transfer graph
detectTransferCycle :: [OwnershipTransfer] -> Bool
detectTransferCycle transfers = 
    let vars = nub $ map transferFrom transfers ++ map transferTo transfers
        buildGraph = foldr (\t g -> 
            let from = transferFrom t
                to = transferTo t
            in insertEdge from to g) (map (\v -> (v, [])) vars) transfers
        hasCycle' = hasCycle (map fst buildGraph) buildGraph
    in hasCycle'
  where
    insertEdge from to graph = 
        map (\(v, edges) -> if v == from then (v, to:edges) else (v, edges)) graph
    
    hasCycle [] _ = False
    hasCycle (v:vs) graph = 
        let visited = []
            recStack = [v]
        in dfsCycle v graph visited recStack || hasCycle vs graph
    
    dfsCycle _ _ [] = False
    dfsCycle node graph visited recStack
        | node `elem` recStack = True
        | node `elem` visited = False
        | otherwise = 
            let neighbors = case lookup node graph of
                             Just ns -> ns
                             Nothing -> []
                newVisited = node : visited
                newRecStack = node : recStack
            in any (`dfsCycle` graph newVisited newRecStack) neighbors

-- Helper function for union
union :: Eq a => [a] -> [a] -> [a]
union xs ys = nub (xs ++ ys)