module Test.Unit.NewCompactOwnershipSpec where
import Test.QuickCheck 
import Test.Tasty.HUnit (testCase, assertFailure, assertBool, (@?=)), assertBool
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property, (===), forAll, choose, elements)
import Ownership
import SourceLocation (SourcePos(..), Located(..), locatedAt)
import Data.Set 
              len <- choose (1, 10)
  first <- elements ['a'..'z']
  rest <- choose (0, len-1) >>= \n -> sequence [elements ['a'..'z'..'0'..'9'] | _ <- [1..n]]
  return (first : rest)
-- Arbitrary instance for SourcePos
instance Arbitrary SourcePos where
  arbitrary = do
    line <- choose (1, 100)
    column <- choose (1, 100)
    offset <- choose (0, 1000)
    return $ SourcePos line column offset

-- Arbitrary instance for SourceSpan
instance Arbitrary SourceSpan where
  arbitrary = do
    start <- arbitrary
    end <- arbitrary
    return $ SourceSpan start end


-- | 
instance Arbitrary OwnershipState where
                                              arbitrary = do
              varCount <- choose (1, 5)
    varNames <- sequence [genVarName | _ <- [1..varCount]]
    ownedVars <- choose (0, varCount) >>= \n -> elements (take n varNames)
    let ownedSet = Set.fromList ownedVars
    return $ OwnershipState ownedSet Map.empty

-- | 
testOwnershipBasicProperties :: TestTree
testOwnershipBasicProperties = testGroup ""
  [             testCase "" $
      let state = emptyOwnershipState
                                        owned = getOwnedVariables state
      in Set.null owned @?= True
    
    ,             testCase "" $
      let state = emptyOwnershipState
                                        var = "x"
          state' = acquireOwnership state var
                                        owned = getOwnedVariables state'
      in assertBool "x" (Set.member var owned)
    
    ,             testCase "" $
      let state = emptyOwnershipState
                                        var = "x"
          state' = acquireOwnership state var
          state'' = releaseOwnership state' var
                                        owned = getOwnedVariables state''
      in assertBool "x" (not $ Set.member var owned)
  ]

-- | 
testOwnershipTransfer :: TestTree
testOwnershipTransfer = testGroup ""
  [             testCase "" $
      let state = emptyOwnershipState
          state' = acquireOwnership state "x"
                                        result = transferOwnership state' "x" "y"
      in case result of
        Left err -> assertBool (": " ++ err) False
        Right state'' -> 
          let owned = getOwnedVariables state''
          in assertBool "yx" (Set.member "y" owned && not (Set.member "x" owned)
    
    ,             testCase "" $
      let state = emptyOwnershipState
                                        result = transferOwnership state "nonexistent" "y"
      in case result of
        Left _ -> assertBool "" True
        Right _ -> assertBool "" False
    
    ,             testCase "" $
      let state = emptyOwnershipState
          state' = acquireOwnership (acquireOwnership state "x") "y"
                                        result = transferOwnership state' "x" "y"
      in case result of
        Left _ -> assertBool "" True
        Right _ -> assertBool "" False
  ]

-- | 
testOwnershipBorrowing :: TestTree
testOwnershipBorrowing = testGroup ""
  [             testCase "" $
      let state = emptyOwnershipState
          state' = acquireOwnership state "x"
                                        result = borrowOwnership state' "x" Immutable
      in case result of
        Left err -> assertBool (": " ++ err) False
        Right (state'', _) ->
          let owned = getOwnedVariables state''
          in assertBool "x" (Set.member "x" owned)
    
    ,             testCase "" $
      let state = emptyOwnershipState
          state' = acquireOwnership state "x"
                                        result = borrowOwnership state' "x" Mutable
      in case result of
        Left err -> assertBool (": " ++ err) False
        Right (state'', _) ->
          let owned = getOwnedVariables state''
          in assertBool "x" (Set.member "x" owned)
    
    ,             testCase "" $
      let state = emptyOwnershipState
          state' = acquireOwnership state "x"
                                        result1 = borrowOwnership state' "x" Immutable
      in case result1 of
        Left err -> assertBool (": " ++ err) False
        Right (state'', _) ->
          let result2 = borrowOwnership state'' "x" Immutable
          in case result2 of
            Left _ -> assertBool "" False
            Right _ -> assertBool "" True
    
    ,             testCase "" $
      let state = emptyOwnershipState
          state' = acquireOwnership state "x"
                                        result1 = borrowOwnership state' "x" Mutable
      in case result1 of
        Left err -> assertBool (": " ++ err) False
        Right (state'', _) ->
          let result2 = borrowOwnership state'' "x" Immutable
          in case result2 of
            Left _ -> assertBool "" True
            Right _ -> assertBool "" False
  ]

-- | 
testOwnershipLifetime :: TestTree
testOwnershipLifetime = testGroup ""
  [             testCase "" $
      let state = emptyOwnershipState
          state' = acquireOwnership state "x"
          state'' = enterScope state'
          state''' = acquireOwnership state'' "y"
          state'''' = exitScope state'''
                                        owned = getOwnedVariables state''''
      in assertBool "y" (Set.member "x" owned && not (Set.member "y" owned)
    
    ,             testCase "" $
      let state = emptyOwnershipState
                                        state1 = acquireOwnership state "x"
                                        state2 = enterScope state1
                                        state3 = acquireOwnership state2 "y"
                                        state4 = enterScope state3
                                        state5 = acquireOwnership state4 "z"
                                        state6 = exitScope state5
                                        state7 = exitScope state6
                                        owned = getOwnedVariables state7
      in assertBool "" (Set.member "x" owned && Set.member "y" owned && not (Set.member "z" owned)
  ]

-- | QuickCheck
testOwnershipProperties :: TestTree
testOwnershipProperties = testGroup ""
  [             testProperty "" $
      \state var ->
        let state' = acquireOwnership state var
                                          owned = getOwnedVariables state'
        in Set.member var owned
  
  ,             testProperty "" $
      \state var ->
        let state' = acquireOwnership state var
            state'' = releaseOwnership state' var
                                          owned = getOwnedVariables state''
        in not (Set.member var owned)
  
  ,             testProperty "" $
      \state fromVar toVar ->
        let state' = acquireOwnership state fromVar
                                          beforeCount = Set.size (getOwnedVariables state')
                                          result = transferOwnership state' fromVar toVar
        in case result of
          Left _ -> True
          Right state'' -> 
            let afterCount = Set.size (getOwnedVariables state'')
            in                               beforeCount === afterCount
  
  ,             testProperty "" $
      \state var ->
        let state' = acquireOwnership state var
                                          beforeOwned = getOwnedVariables state'
                                          result = borrowOwnership state' var Immutable
        in case result of
          Left _ -> True
          Right (state'', _) -> getOwnedVariables state'' === beforeOwned
  ]

-- | 
testOwnershipValidation :: TestTree
testOwnershipValidation = testGroup ""
  [             testCase "" $
      let state = emptyOwnershipState
          state' = acquireOwnership (acquireOwnership state) "x" "y"
                                        violations = validateOwnershipState state'
      in null violations @?= True
    
    ,             testCase "" $
      let state = emptyOwnershipState
          state' = acquireOwnership state "x"
          state'' = releaseOwnership state' "x"
                                        violations = validateOwnershipState state''
      in L.length violations @?= 1
    
    ,             testCase "" $
      let state = emptyOwnershipState
          state' = acquireOwnership state "x"
          state'' = releaseOwnership state' "x"
          state''' = releaseOwnership state'' "x"
                                        violations = validateOwnershipState state'''
      in L.length violations >= 1 @?= True
  ]

-- | 
testBoundaryConditions :: TestTree
testBoundaryConditions = testGroup ""
  [             testCase "" $
      let state = emptyOwnershipState
                                        result = transferOwnership state "x" "y"
      in case result of
        Left _ -> assertBool "" True
        Right _ -> assertBool "" False
    
    ,             testCase "" $
      let state = emptyOwnershipState
          state' = acquireOwnership state "x"
                                        result = transferOwnership state' "x" "x"
      in case result of
        Left _ -> assertBool "" True
        Right _ -> assertBool "" False
    
    ,             testCase "" $
      let vars = L.map (\i -> "var" ++ show i) [1..100]
                                        state = foldl acquireOwnership emptyOwnershipState vars
                                        owned = getOwnedVariables state
      in Set.size owned @?= 100
  ]

-- | 
testPerformanceProperties :: TestTree
testPerformanceProperties = testGroup ""
  [             testProperty "" $
      \n ->
        let numOps = min 1000 (max 1 n)
                                          vars = L.map (\i -> "var" ++ show i) [1..numOps]
                                          state = foldl acquireOwnership emptyOwnershipState vars
                                          transfers = zip vars (L.tail vars ++ ["final"])
                                          finalState = L.foldl (\s (from, to) -> 
              case transferOwnership s from to of
                Left _ -> s
                Right s' -> s') state transfers
        in Set.size (getOwnedVariables finalState) >= 0
  ]

-- | 
tests :: TestTree
tests =   testGroup "Ownership"
  [ testOwnershipBasicProperties
  , testOwnershipTransfer
  , testOwnershipBorrowing
  , testOwnershipLifetime
  , testOwnershipProperties
  , testOwnershipValidation
  , testBoundaryConditions
  , testPerformanceProperties
  ]

-- Ownership
data                               OwnershipState = OwnershipState 
  { ownedVars :: Set String
  , borrowMap :: Map String BorrowInfo
  } deriving (Show, Eq)

data                               BorrowType = Immutable | Mutable deriving (Show, Eq)
data                               BorrowInfo = BorrowInfo 
  { borrowType :: BorrowType
  , borrowCount :: Int
  } deriving (Show, Eq)

emptyOwnershipState :: OwnershipState
                              emptyOwnershipState = OwnershipState Set.empty Map.empty

getOwnedVariables :: OwnershipState -> Set String
                              getOwnedVariables = ownedVars
acquireOwnership :: OwnershipState -> String -> OwnershipState
acquireOwnership state                               var = state {                               ownedVars = Set.insert var (ownedVars state) }

releaseOwnership :: OwnershipState -> String -> OwnershipState
releaseOwnership state                               var = state {                               ownedVars = Set.delete var (ownedVars state) }

transferOwnership :: OwnershipState -> String -> String -> Either String OwnershipState
transferOwnership state from                               to = 
if Set.member from (ownedVars state)
  then Right $ state {                               ownedVars = Set.insert to (Set.delete from (ownedVars state) }
  else Left "Variable not owned"

borrowOwnership :: OwnershipState -> String -> BorrowType -> Either String (OwnershipState, Int)
borrowOwnership state var                               borrowType = 
  if Set.member var (ownedVars state)
  then Right (state, 1)  -- 
  else Left "Variable not owned"

enterScope :: OwnershipState -> OwnershipState
enterScope                               state = state  -- 

exitScope :: OwnershipState -> OwnershipState
exitScope                               state = state  -- 
validateOwnershipState :: OwnershipState -> [String]
validateOwnershipState                               state = []  -- 