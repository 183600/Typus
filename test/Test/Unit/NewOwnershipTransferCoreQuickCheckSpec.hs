module Test.Unit.NewOwnershipTransferCoreQuickCheckSpec where
import Test.QuickCheck 
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property, (===), counterexample)
import Test.Tasty.HUnit (testCase, assertFailure, assertBool, (@?=))
import qualified Data.Text as T
import qualified Data.Map as Map
import qualified Data.Set as Set
import Ownership
import Ownership.Common.Types
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


-- ============================================================================
-- Test Data Generators
-- ============================================================================

-- Generate ownership variable names
genOwnershipVariable :: Gen String
                              genOwnershipVariable = do
              prefix <- elements ["var", "data", "resource", "value", "obj"]
    suffix <- choose (1, 1000)
    return $ prefix ++ show suffix

-- Generate ownership states
genOwnershipState :: Gen OwnershipState
                              genOwnershipState = elements [Owned, Borrowed, Moved, Shared]

instance Arbitrary OwnershipState where
                                                arbitrary = genOwnershipState

-- Generate ownership permissions
genOwnershipPermission :: Gen OwnershipPermission
                              genOwnershipPermission = elements [Read, Write, ReadWrite]

instance Arbitrary OwnershipPermission where
                                                arbitrary = genOwnershipPermission

-- Generate ownership variables
genOwnershipVar :: Gen OwnershipVariable
                              genOwnershipVar = do
              name <- genOwnershipVariable
    state <- arbitrary
    permissions <- listOf1 arbitrary
    return $ OwnershipVariable name state permissions

instance Arbitrary OwnershipVariable where
                                                arbitrary = genOwnershipVar

-- Generate ownership contexts
genOwnershipContext :: Gen OwnershipContext
                              genOwnershipContext = do
              numVars <- choose (0, 10)
    vars <- listOf genOwnershipVar
    let varMap = Map.fromList [(varName var, var) | var <- vars]
    return $ OwnershipContext varMap Set.empty

instance Arbitrary OwnershipContext where
                                                arbitrary = genOwnershipContext

-- Generate ownership transfer operations
genOwnershipTransfer :: Gen OwnershipTransfer
                              genOwnershipTransfer = do
              from <- genOwnershipVariable
    to <- genOwnershipVariable
    transferType <- elements [MoveOwnership, BorrowOwnership, ShareOwnership]
    return $ OwnershipTransfer from to transferType

instance Arbitrary OwnershipTransfer where
                                                arbitrary = genOwnershipTransfer

-- ============================================================================
-- Ownership Transfer Core Properties
-- ============================================================================

-- Property: Moving ownership changes source state to Moved
prop_moveOwnershipChangesSourceState :: OwnershipContext -> String -> String -> Property
prop_moveOwnershipChangesSourceState context fromVar                               toVar =
    let transfer = OwnershipTransfer fromVar toVar MoveOwnership
                                      result = performOwnershipTransfer transfer context
    in counterexample ("Moving ownership should change source state to Moved")
       (case result of
           Right newContext -> 
               case Map.lookup fromVar (ownershipVariables newContext) of
                   Just var -> ownershipState                               var === Moved
                   Nothing -> property False
           Left _ -> property False)

-- Property: Moving ownership transfers permissions to destination
prop_moveOwnershipTransfersPermissions :: OwnershipContext -> String -> String -> Property
prop_moveOwnershipTransfersPermissions context fromVar                               toVar =
    let transfer = OwnershipTransfer fromVar toVar MoveOwnership
                                      result = performOwnershipTransfer transfer context
    in counterexample ("Moving ownership should transfer permissions to destination")
       (case result of
           Right newContext -> 
               case (Map.lookup fromVar (ownershipVariables context), 
                     Map.lookup toVar (ownershipVariables newContext) of
                   (Just fromVar', Just toVar') -> 
                       ownershipPermissions fromVar' === ownershipPermissions toVar'
                   _ -> property False
           Left _ -> property False)

-- Property: Borrowing ownership preserves source state
prop_borrowOwnershipPreservesSource :: OwnershipContext -> String -> String -> Property
prop_borrowOwnershipPreservesSource context fromVar                               toVar =
    let transfer = OwnershipTransfer fromVar toVar BorrowOwnership
                                      result = performOwnershipTransfer transfer context
    in counterexample ("Borrowing should preserve source state")
       (case result of
           Right newContext -> 
               case (Map.lookup fromVar (ownershipVariables context),
                     Map.lookup fromVar (ownershipVariables newContext) of
                   (Just originalVar, Just newVar) -> 
                       ownershipState                               originalVar === ownershipState newVar
                   _ -> property False
           Left _ -> property False)

-- Property: Sharing ownership creates shared state
prop_shareOwnershipCreatesShared :: OwnershipContext -> String -> String -> Property
prop_shareOwnershipCreatesShared context fromVar                               toVar =
    let transfer = OwnershipTransfer fromVar toVar ShareOwnership
                                      result = performOwnershipTransfer transfer context
    in counterexample ("Sharing should create shared state")
       (case result of
           Right newContext -> 
               case Map.lookup toVar (ownershipVariables newContext) of
                   Just var -> ownershipState                               var === Shared
                   Nothing -> property False
           Left _ -> property False)

-- Property: Transfer from non-existent variable fails
prop_transferFromNonExistentFails :: String -> String -> OwnershipContext -> Property
prop_transferFromNonExistentFails fromVar toVar                               context =
    let hasVar = Map.member fromVar (ownershipVariables context)
                                      transfer = OwnershipTransfer fromVar toVar MoveOwnership
                                      result = performOwnershipTransfer transfer context
    in counterexample ("Transfer from non-existent variable should fail")
       (if not hasVar then isLeft                               result === True else property True)

-- Property: Double move should fail
prop_doubleMoveShouldFail :: OwnershipContext -> String -> String -> String -> Property
prop_doubleMoveShouldFail context fromVar toVar1                               toVar2 =
    let transfer1 = OwnershipTransfer fromVar toVar1 MoveOwnership
                                      transfer2 = OwnershipTransfer fromVar toVar2 MoveOwnership
                                      result1 = performOwnershipTransfer transfer1 context
    in case result1 of
        Right newContext -> 
            let result2 = performOwnershipTransfer transfer2 newContext
            in counterexample ("Double move should fail")
               (isLeft                               result2 === True)
        Left _ -> property True

-- Property: Ownership transfer preserves total permissions
prop_transferPreservesTotalPermissions :: OwnershipContext -> String -> String -> Property
prop_transferPreservesTotalPermissions context fromVar                               toVar =
    let transfer = OwnershipTransfer fromVar toVar MoveOwnership
                                      result = performOwnershipTransfer transfer context
    in counterexample ("Transfer should preserve total permissions")
       (case result of
           Right newContext -> 
               let originalPerms = getAllPermissions context
                                                 newPerms = getAllPermissions newContext
               in                               originalPerms === newPerms
           Left _ -> property False)

-- Property: Borrowing creates borrow relationships
prop_borrowingCreatesRelationships :: OwnershipContext -> String -> String -> Property
prop_borrowingCreatesRelationships context fromVar                               toVar =
    let transfer = OwnershipTransfer fromVar toVar BorrowOwnership
                                      result = performOwnershipTransfer transfer context
    in counterexample ("Borrowing should create borrow relationships")
       (case result of
           Right newContext -> 
               let relationships = ownershipBorrowRelationships newContext
                                                 hasRelationship = (fromVar, toVar) `Set.member` relationships
               in                               hasRelationship === True
           Left _ -> property False)

-- Property: Shared ownership allows multiple borrows
prop_sharedOwnershipAllowsMultipleBorrows :: OwnershipContext -> String -> [String] -> Property
prop_sharedOwnershipAllowsMultipleBorrows context fromVar                               toVars =
    let initialTransfer = OwnershipTransfer fromVar (L.head toVars) ShareOwnership
                                      result1 = performOwnershipTransfer initialTransfer context
    in case result1 of
        Right sharedContext -> 
            let borrowTransfers = [OwnershipTransfer fromVar toVar BorrowOwnership | toVar <- L.tail toVars]
                                              results = L.map (`performOwnershipTransfer` sharedContext) borrowTransfers
                                              successCount = L.length $ filter isRight results
            in counterexample ("Shared ownership should allow multiple borrows")
               (successCount === L.length (L.tail toVars)
        Left _ -> property True

-- Property: Ownership context maintains variable uniqueness
prop_contextMaintainsUniqueness :: OwnershipContext -> Property
prop_contextMaintainsUniqueness                               context =
    let vars = Map.elems (ownershipVariables context)
                                      varNames = map varName vars
                                      uniqueNames = nub varNames
    in counterexample ("Context should maintain variable uniqueness")
       (L.length                               varNames === L.length uniqueNames)

-- Property: Permission checking works correctly
prop_permissionCheckingWorks :: OwnershipVariable -> OwnershipPermission -> Property
prop_permissionCheckingWorks var                               permission =
    let hasPermission = checkOwnershipPermission var permission
                                      permissions = ownershipPermissions var
    in counterexample ("Permission checking should be accurate")
       (hasPermission === (permission `elem` permissions)

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests =   testGroup "New Ownership Transfer Core QuickCheck Tests"
    [             testProperty "Move ownership changes source state" prop_moveOwnershipChangesSourceState
    ,             testProperty "Move ownership transfers permissions" prop_moveOwnershipTransfersPermissions
    ,             testProperty "Borrow ownership preserves source" prop_borrowOwnershipPreservesSource
    ,             testProperty "Share ownership creates shared" prop_shareOwnershipCreatesShared
    ,             testProperty "Transfer from non-existent fails" prop_transferFromNonExistentFails
    ,             testProperty "Double move should fail" prop_doubleMoveShouldFail
    ,             testProperty "Transfer preserves total permissions" prop_transferPreservesTotalPermissions
    ,             testProperty "Borrowing creates relationships" prop_borrowingCreatesRelationships
    ,             testProperty "Shared ownership allows multiple borrows" prop_sharedOwnershipAllowsMultipleBorrows
    ,             testProperty "Context maintains uniqueness" prop_contextMaintainsUniqueness
    ,             testProperty "Permission checking works" prop_permissionCheckingWorks
    ]

-- ============================================================================
-- Helper Functions (Mock implementations for testing)
-- ============================================================================

-- Mock ownership data types
data                               OwnershipState = Owned | Borrowed | Moved | Shared
    deriving (Show, Eq)

data                               OwnershipPermission = Read | Write | ReadWrite
    deriving (Show, Eq)

data                               OwnershipTransferType = MoveOwnership | BorrowOwnership | ShareOwnership
    deriving (Show, Eq)

data                               OwnershipVariable = OwnershipVariable
    { varName :: String
    , ownershipState :: OwnershipState
    , ownershipPermissions :: [OwnershipPermission]
    } deriving (Show, Eq)

data                               OwnershipTransfer = OwnershipTransfer
    { transferFrom :: String
    , transferTo :: String
    , transferType :: OwnershipTransferType
    } deriving (Show, Eq)

data                               OwnershipContext = OwnershipContext
    { ownershipVariables :: Map.Map String OwnershipVariable
    , ownershipBorrowRelationships :: Set.Set (String, String)  -- (borrower, lender)
    } deriving (Show, Eq)

-- Mock ownership operations
performOwnershipTransfer :: OwnershipTransfer -> OwnershipContext -> Either String OwnershipContext
performOwnershipTransfer transfer                               context =
    let fromVar = transferFrom transfer
                                      toVar = transferTo transfer
                                      transferType = transferType transfer
                                      vars = ownershipVariables context
    in case Map.lookup fromVar vars of
        Nothing -> Left $ "Variable not found: " ++ fromVar
        Just sourceVar ->
            case ownershipState sourceVar of
                Moved -> Left $ "Cannot transfer from moved variable: " ++ fromVar
                _ -> do
                                let newSourceVar = case transferType of
                            MoveOwnership -> sourceVar {                               ownershipState = Moved }
                            _ -> sourceVar
                    let newTargetVar = OwnershipVariable toVar 
                                        (case transferType of
                                            MoveOwnership -> Owned
                                            BorrowOwnership -> Borrowed
                                            ShareOwnership -> Shared)
                                        (ownershipPermissions sourceVar)
                    let newVars = Map.insert fromVar newSourceVar 
                                   (Map.insert toVar newTargetVar vars)
                    let newRelationships = case transferType of
                            BorrowOwnership -> Set.insert (toVar, fromVar) (ownershipBorrowRelationships context)
                            _ -> ownershipBorrowRelationships context
                    Right $ context {                               ownershipVariables = newVars,                               ownershipBorrowRelationships = newRelationships }

-- Helper functions
checkOwnershipPermission :: OwnershipVariable -> OwnershipPermission -> Bool
checkOwnershipPermission var                               permission = 
    let perms = ownershipPermissions var
    in permission `elem` perms || ReadWrite `elem` perms

getAllPermissions :: OwnershipContext -> [OwnershipPermission]
getAllPermissions                               context =
let vars = Map.elems (ownershipVariables context)
    in property $ concatMap ownershipPermissions vars

-- Helper functions
isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft                               _ = False

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight                               _ = False

-- Import required for nub
import Data.List 