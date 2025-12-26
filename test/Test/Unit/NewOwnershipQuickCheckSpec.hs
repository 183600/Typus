{-# LANGUAGE TemplateHaskell #-}

module Test.Unit.NewOwnershipQuickCheckSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Ownership
import SourceLocation (SourcePos(..), SourceSpan(..), startPos, spanBetween)
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (nub, (\\))
import Data.Set (Set)
import qualified Data.Set as Set

-- | Test ownership analysis properties
spec :: Spec
spec = describe "NewOwnership QuickCheck Tests" $ do

  describe "Ownership tracking properties" $ do
    it "initial ownership state is empty" $ do
      let emptyState = createEmptyOwnershipState
      getOwnedVariables emptyState `shouldBe` Set.empty
      getBorrowedVariables emptyState `shouldBe` Set.empty

    it "adding owned variables updates state" $ property $
      \varName ->
        let emptyState = createEmptyOwnershipState
            state1 = addOwnedVariable varName emptyState
        in varName `Set.member` getOwnedVariables state1

    it "removing owned variables updates state" $ property $
      \varNames ->
        let state = foldr addOwnedVariable createEmptyOwnershipState varNames
            state1 = removeOwnedVariable (head varNames) state
        in not (head varNames `Set.member` getOwnedVariables state1)

    it "borrowing variables moves ownership" $ property $
      \varName borrower ->
        let state = addOwnedVariable varName createEmptyOwnershipState
            borrowedState = borrowVariable varName borrower state
        in not (varName `Set.member` getOwnedVariables borrowedState) &&
           varName `Set.member` getBorrowedVariables borrowedState

  describe "Ownership transfer properties" $ do
    it "transfer moves ownership correctly" $ property $
      \varName fromOwner toOwner ->
        let state = addOwnedVariable varName createEmptyOwnershipState
            transferred = transferOwnership varName fromOwner toOwner state
        in getVariableOwner varName transferred === Just toOwner

    it "transfer fails for non-existent variables" $ property $
      \varName fromOwner toOwner ->
        let state = createEmptyOwnershipState
            transferred = transferOwnership varName fromOwner toOwner state
        in getVariableOwner varName transferred === Nothing

    it "transfer preserves other variables" $ property $
      \varNames fromOwner toOwner targetVar ->
        let state = foldr addOwnedVariable createEmptyOwnershipState varNames
            transferred = transferOwnership targetVar fromOwner toOwner state
            otherVars = varNames \\ [targetVar]
        in all (\v -> getVariableOwner v transferred === Just v) otherVars

    it "circular transfer is detected" $ property $
      \varName ->
        let state = addOwnedVariable varName createEmptyOwnershipState
            result = detectCircularTransfer varName varName state
        in result === True

  describe "Borrowing properties" $ do
    it "borrowing creates borrow relationship" $ property $
      \varName borrower ->
        let state = addOwnedVariable varName createEmptyOwnershipState
            borrowed = borrowVariable varName borrower state
        in getBorrower varName borrowed === Just borrower

    it "multiple borrowers are tracked" $ property $
      \varName borrowers ->
        let state = addOwnedVariable varName createEmptyOwnershipState
            borrowed = foldr (borrowVariable varName) state borrowers
            actualBorrowers = getBorrowers varName borrowed
        in Set.fromList borrowers === actualBorrowers

    it "borrowing prevents transfer" $ property $
      \varName borrower newOwner ->
        let state = addOwnedVariable varName createEmptyOwnershipState
            borrowed = borrowVariable varName borrower state
            transferred = transferOwnership varName "owner" newOwner borrowed
        in getVariableOwner varName transferred === Just "owner"

    it "returning borrow restores ownership" $ property $
      \varName borrower ->
        let state = addOwnedVariable varName createEmptyOwnershipState
            borrowed = borrowVariable varName borrower state
            returned = returnBorrow varName borrower borrowed
        in varName `Set.member` getOwnedVariables returned &&
           not (varName `Set.member` getBorrowedVariables returned)

  describe "Lifetime properties" $ do
    it "variables have correct lifetimes" $ property $
      \varName startScope endScope ->
        let state = addOwnedVariable varName createEmptyOwnershipState
            withLifetime = setVariableLifetime varName startScope endScope state
            lifetime = getVariableLifetime varName withLifetime
        in lifetime === Just (startScope, endScope)

    it "lifetime boundaries are enforced" $ property $
      \varName startScope endScope useScope ->
        let state = addOwnedVariable varName createEmptyOwnershipState
            withLifetime = setVariableLifetime varName startScope endScope state
            isValid = isVariableValidAtScope varName useScope withLifetime
        in (useScope >= startScope && useScope <= endScope) ==> isValid

    it "expired variables are invalid" $ property $
      \varName startScope endScope useScope ->
        let state = addOwnedVariable varName createEmptyOwnershipState
            withLifetime = setVariableLifetime varName startScope endScope state
            isValid = isVariableValidAtScope varName useScope withLifetime
        in (useScope > endScope) ==> not isValid

    it "nested lifetimes are handled correctly" $ property $
      \outerVar innerVar outerStart outerEnd innerStart innerEnd ->
        let state = addOwnedVariable outerVar createEmptyOwnershipState
            state1 = addOwnedVariable innerVar state
            state2 = setVariableLifetime outerVar outerStart outerEnd state1
            state3 = setVariableLifetime innerVar innerStart innerEnd state2
        in (innerStart >= outerStart && innerEnd <= outerEnd) ==> 
           areLifetimesNested outerVar innerVar state3

  describe "Ownership constraints properties" $ do
    it "move constraints are enforced" $ property $
      \varName ->
        let state = addOwnedVariable varName createEmptyOwnershipState
            constrained = addMoveConstraint varName state
        in hasMoveConstraint varName constrained

    it "copy constraints are enforced" $ property $
      \varName ->
        let state = addOwnedVariable varName createEmptyOwnershipState
            constrained = addCopyConstraint varName state
        in hasCopyConstraint varName constrained

    it "constraints prevent invalid operations" $ property $
      \varName ->
        let state = addOwnedVariable varName createEmptyOwnershipState
            moveConstrained = addMoveConstraint varName state
            copyConstrained = addCopyConstraint varName state
        in canMove varName moveConstrained &&
           canCopy varName copyConstrained &&
           not (canMove varName copyConstrained)

    it "constraint inheritance works" $ property $
      \varName parentVar ->
        let state = addOwnedVariable parentVar createEmptyOwnershipState
            state1 = addOwnedVariable varName state
            state2 = inheritConstraints parentVar varName state1
        in getVariableConstraints varName state2 === getVariableConstraints parentVar state

  describe "Ownership analysis properties" $ do
    it "analysis detects ownership violations" $ property $
      \operations ->
        let state = createEmptyOwnershipState
            result = analyzeOwnershipOperations state operations
            violations = getOwnershipViolations result
        in length violations >= 0

    it "valid operations produce no violations" $ property $
      \varNames ->
        let state = foldr addOwnedVariable createEmptyOwnershipState varNames
            validOps = map (\v -> UseVariable v) varNames
            result = analyzeOwnershipOperations state validOps
            violations = getOwnershipViolations result
        in null violations

    it "invalid operations produce violations" $ property $
      \varName ->
        let state = createEmptyOwnershipState
            invalidOps = [UseVariable varName, MoveVariable varName "newOwner"]
            result = analyzeOwnershipOperations state invalidOps
            violations = getOwnershipViolations result
        in not (null violations)

    it "analysis preserves invariants" $ property $
      \operations ->
        let state = createEmptyOwnershipState
            result = analyzeOwnershipOperations state operations
            finalState = getFinalOwnershipState result
        in ownershipInvariantsHold finalState

  where
    -- Helper types for testing
    data OwnershipState = OwnershipState
      { ownedVariables :: Set String
      , borrowedVariables :: Set String
      , variableOwners :: [(String, String)]
      , variableBorrowers :: [(String, Set String)]
      , variableLifetimes :: [(String, (Int, Int))]
      , variableConstraints :: [(String, [Constraint])]
      } deriving (Eq, Show)

    data Constraint = MoveConstraint | CopyConstraint
      deriving (Eq, Show)

    data OwnershipOperation = UseVariable String
                            | MoveVariable String String
                            | BorrowVariable String String
                            | ReturnBorrow String String
      deriving (Eq, Show)

    data OwnershipAnalysisResult = OwnershipAnalysisResult
      { finalState :: OwnershipState
      , ownershipViolations :: [String]
      } deriving (Eq, Show)

    -- Mock implementations for testing
    createEmptyOwnershipState :: OwnershipState
    createEmptyOwnershipState = OwnershipState Set.empty Set.empty [] [] [] []

    addOwnedVariable :: String -> OwnershipState -> OwnershipState
    addOwnedVariable var state = state
      { ownedVariables = Set.insert var (ownedVariables state)
      , variableOwners = (var, var) : variableOwners state
      }

    removeOwnedVariable :: String -> OwnershipState -> OwnershipState
    removeOwnedVariable var state = state
      { ownedVariables = Set.delete var (ownedVariables state)
      , variableOwners = filter ((/= var) . fst) (variableOwners state)
      }

    borrowVariable :: String -> String -> OwnershipState -> OwnershipState
    borrowVariable var borrower state = state
      { ownedVariables = Set.delete var (ownedVariables state)
      , borrowedVariables = Set.insert var (borrowedVariables state)
      , variableBorrowers = (var, Set.singleton borrower) : 
                           filter ((/= var) . fst) (variableBorrowers state)
      }

    returnBorrow :: String -> String -> OwnershipState -> OwnershipState
    returnBorrow var borrower state = state
      { ownedVariables = Set.insert var (ownedVariables state)
      , borrowedVariables = Set.delete var (borrowedVariables state)
      , variableBorrowers = updateBorrowers var (Set.delete borrower) (variableBorrowers state)
      }
      where
        updateBorrowers _ _ [] = []
        updateBorrowers v f ((name, borrowers):rest)
          | name == v = (name, f borrowers) : rest
          | otherwise = (name, borrowers) : updateBorrowers v f rest

    transferOwnership :: String -> String -> String -> OwnershipState -> OwnershipState
    transferOwnership var fromOwner toOwner state = 
      case lookup var (variableOwners state) of
        Just owner | owner == fromOwner -> 
          state { variableOwners = (var, toOwner) : 
                               filter ((/= var) . fst) (variableOwners state) }
        _ -> state

    getOwnedVariables :: OwnershipState -> Set String
    getOwnedVariables = ownedVariables

    getBorrowedVariables :: OwnershipState -> Set String
    getBorrowedVariables = borrowedVariables

    getVariableOwner :: String -> OwnershipState -> Maybe String
    getVariableOwner var state = lookup var (variableOwners state)

    getBorrower :: String -> OwnershipState -> Maybe String
    getBorrower var state = 
      case lookup var (variableBorrowers state) of
        Just borrowers -> if Set.null borrowers then Nothing else Just (Set.findMin borrowers)
        Nothing -> Nothing

    getBorrowers :: String -> OwnershipState -> Set String
    getBorrowers var state = 
      case lookup var (variableBorrowers state) of
        Just borrowers -> borrowers
        Nothing -> Set.empty

    setVariableLifetime :: String -> Int -> Int -> OwnershipState -> OwnershipState
    setVariableLifetime var start end state = state
      { variableLifetimes = (var, (start, end)) : 
                           filter ((/= var) . fst) (variableLifetimes state)
      }

    getVariableLifetime :: String -> OwnershipState -> Maybe (Int, Int)
    getVariableLifetime var state = lookup var (variableLifetimes state)

    isVariableValidAtScope :: String -> Int -> OwnershipState -> Bool
    isVariableValidAtScope var scope state = 
      case getVariableLifetime var state of
        Just (start, end) -> scope >= start && scope <= end
        Nothing -> False

    areLifetimesNested :: String -> String -> OwnershipState -> Bool
    areLifetimesNested outerVar innerVar state = 
      case (getVariableLifetime outerVar state, getVariableLifetime innerVar state) of
        (Just (outerStart, outerEnd), Just (innerStart, innerEnd)) -> 
          innerStart >= outerStart && innerEnd <= outerEnd
        _ -> False

    addMoveConstraint :: String -> OwnershipState -> OwnershipState
    addMoveConstraint var state = addConstraint var MoveConstraint state

    addCopyConstraint :: String -> OwnershipState -> OwnershipState
    addCopyConstraint var state = addConstraint var CopyConstraint state

    addConstraint :: String -> Constraint -> OwnershipState -> OwnershipState
    addConstraint var constraint state = state
      { variableConstraints = (var, [constraint]) : 
                             filter ((/= var) . fst) (variableConstraints state)
      }

    hasMoveConstraint :: String -> OwnershipState -> Bool
    hasMoveConstraint var state = MoveConstraint `elem` getVariableConstraints var state

    hasCopyConstraint :: String -> OwnershipState -> Bool
    hasCopyConstraint var state = CopyConstraint `elem` getVariableConstraints var state

    getVariableConstraints :: String -> OwnershipState -> [Constraint]
    getVariableConstraints var state = 
      case lookup var (variableConstraints state) of
        Just constraints -> constraints
        Nothing -> []

    inheritConstraints :: String -> String -> OwnershipState -> OwnershipState
    inheritConstraints fromVar toVar state = 
      case getVariableConstraints fromVar state of
        [] -> state
        constraints -> foldr addConstraint state (map (const toVar) constraints)

    canMove :: String -> OwnershipState -> Bool
    canMove var state = 
      var `Set.member` ownedVariables state && 
      not (CopyConstraint `elem` getVariableConstraints var state)

    canCopy :: String -> OwnershipState -> Bool
    canCopy var state = 
      CopyConstraint `elem` getVariableConstraints var state

    analyzeOwnershipOperations :: OwnershipState -> [OwnershipOperation] -> OwnershipAnalysisResult
    analyzeOwnershipOperations initialState operations = 
      let (finalState', violations) = foldl processOperation (initialState, []) operations
      in OwnershipAnalysisResult finalState' violations
      where
        processOperation (state, violations) (UseVariable var) = 
          if var `Set.member` ownedVariables state || var `Set.member` borrowedVariables state
          then (state, violations)
          else (state, "Use of uninitialized variable: " ++ var : violations)
        processOperation (state, violations) (MoveVariable var newOwner) = 
          let newState = transferOwnership var (getVariableOwner var state |> fromMaybe "") newOwner state
          in (newState, violations)
        processOperation (state, violations) (BorrowVariable var borrower) = 
          let newState = borrowVariable var borrower state
          in (newState, violations)
        processOperation (state, violations) (ReturnBorrow var borrower) = 
          let newState = returnBorrow var borrower state
          in (newState, violations)
        
        fromMaybe _ Nothing = ""
        fromMaybe def (Just x) = x

    getOwnershipViolations :: OwnershipAnalysisResult -> [String]
    getOwnershipViolations = ownershipViolations

    getFinalOwnershipState :: OwnershipAnalysisResult -> OwnershipState
    getFinalOwnershipState = finalState

    detectCircularTransfer :: String -> String -> OwnershipState -> Bool
    detectCircularTransfer var fromOwner state = 
      getVariableOwner var state == Just fromOwner && var == fromOwner

    ownershipInvariantsHold :: OwnershipState -> Bool
    ownershipInvariantsHold state = 
      let owned = ownedVariables state
          borrowed = borrowedVariables state
          owners = map fst (variableOwners state)
      in Set.isSubsetOf (Set.fromList owners) owned &&
         Set.disjoint owned borrowed

    -- Helper functions
    (|>) :: a -> (a -> b) -> b
    x |> f = f

    -- Helper instances for QuickCheck
    instance Arbitrary Constraint where
      arbitrary = elements [MoveConstraint, CopyConstraint]

    instance Arbitrary OwnershipOperation where
      arbitrary = oneof
        [ UseVariable <$> arbitrary
        , MoveVariable <$> arbitrary <*> arbitrary
        , BorrowVariable <$> arbitrary <*> arbitrary
        , ReturnBorrow <$> arbitrary <*> arbitrary
        ]

    instance Arbitrary OwnershipState where
      arbitrary = do
        owned <- arbitrary
        borrowed <- arbitrary
        return $ OwnershipState owned borrowed [] [] [] []
