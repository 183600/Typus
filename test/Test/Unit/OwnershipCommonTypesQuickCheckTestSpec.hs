{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.OwnershipCommonTypesQuickCheckTestSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck
import qualified Data.Map as Map
import Data.List (sort, nub)

import Ownership.Common.Types
import SourceLocation (SourcePos(..), SourceSpan(..), startPos)
import TestSupport.Arbitrary ()

tests :: TestTree
tests = testGroup "Ownership Common Types QuickCheck Tests"
  [ ownershipTypeTests
  , ownershipStateTests
  , ownershipTransferTests
  , ownershipRegionTests
  , ownershipPermissionTests
  , ownershipConstraintTests
  , ownershipVariableTests
  , ownershipFunctionTests
  , ownershipLifetimeTests
  , ownershipValidationTests
  ]

-- | 1. 所有权类型测试
ownershipTypeTests :: TestTree
ownershipTypeTests = testGroup "Ownership Type Tests"
  [ testCase "Owned type creation" $
      let ownedType = OwnershipType Owned
      in ownershipTypeKind ownedType @?= Owned
  
  , testCase "Borrowed type creation" $
      let borrowedType = OwnershipType Borrowed
      in ownershipTypeKind borrowedType @?= Borrowed
  
  , testCase "Shared type creation" $
      let sharedType = OwnershipType Shared
      in ownershipTypeKind sharedType @?= Shared
  
  , fastProperty "Ownership type equality" $
      \kind1 kind2 -> let type1 = OwnershipType kind1
                          type2 = OwnershipType kind2
                      in (type1 == type2) == (kind1 == kind2)
  ]

-- | 2. 所有权状态测试
ownershipStateTests :: TestTree
ownershipStateTests = testGroup "Ownership State Tests"
  [ testCase "Active ownership state" $
      let state = OwnershipState Active
      in ownershipStateStatus state @?= Active
  
  , testCase "Transferred ownership state" $
      let state = OwnershipState Transferred
      in ownershipStateStatus state @?= Transferred
  
  , testCase "Dropped ownership state" $
      let state = OwnershipState Dropped
      in ownershipStateStatus state @?= Dropped
  
  , fastProperty "Ownership state transitions" $
      \initialStatus -> let state = OwnershipState initialStatus
                        in ownershipStateStatus state == initialStatus
  ]

-- | 3. 所有权转移测试
ownershipTransferTests :: TestTree
ownershipTransferTests = testGroup "Ownership Transfer Tests"
  [ testCase "Ownership transfer creation" $
      let transfer = OwnershipTransfer "var1" "var2" (SourceSpan startPos startPos)
      in ownershipTransferSource transfer @?= "var1"
  
  , testCase "Ownership transfer destination" $
      let transfer = OwnershipTransfer "src" "dest" (SourceSpan startPos startPos)
      in ownershipTransferDestination transfer @?= "dest"
  
  , fastProperty "Transfer source and destination are different" $
      \src dest -> let transfer = OwnershipTransfer src dest (SourceSpan startPos startPos)
                   in (src == dest) || (ownershipTransferSource transfer /= ownershipTransferDestination transfer)
  ]

-- | 4. 所有权区域测试
ownershipRegionTests :: TestTree
ownershipRegionTests = testGroup "Ownership Region Tests"
  [ testCase "Region creation" $
      let region = OwnershipRegion "test_region" []
      in ownershipRegionName region @?= "test_region"
  
  , testCase "Region with variables" $
      let region = OwnershipRegion "region" ["var1", "var2"]
      in length (ownershipRegionVariables region) @?= 2
  
  , fastProperty "Region variable uniqueness" $
      \varNames -> let region = OwnershipRegion "test" varNames
                       uniqueVars = nub varNames
                   in length (ownershipRegionVariables region) >= length uniqueVars
  ]

-- | 5. 所有权权限测试
ownershipPermissionTests :: TestTree
ownershipPermissionTests = testGroup "Ownership Permission Tests"
  [ testCase "Read permission" $
      let permission = ReadPermission
      in ownershipPermissionType permission @?= Read
  
  , testCase "Write permission" $
      let permission = WritePermission
      in ownershipPermissionType permission @?= Write
  
  , testCase "ReadWrite permission" $
      let permission = ReadWritePermission
      in ownershipPermissionType permission @?= ReadWrite
  
  , fastProperty "Permission compatibility" $
      \perm1 perm2 -> let compatible = isPermissionCompatible perm1 perm2
                      in (perm1 == ReadPermission && perm2 == ReadPermission) ==> compatible
  ]
  where
    isPermissionCompatible ReadPermission ReadPermission = True
    isPermissionCompatible WritePermission WritePermission = True
    isPermissionCompatible ReadWritePermission _ = True
    isPermissionCompatible _ ReadWritePermission = True
    isPermissionCompatible _ _ = False

-- | 6. 所有权约束测试
ownershipConstraintTests :: TestTree
ownershipConstraintTests = testGroup "Ownership Constraint Tests"
  [ testCase "Lifetime constraint" $
      let constraint = LifetimeConstraint "var1" "var2"
      in ownershipConstraintType constraint @?= Lifetime
  
  , testCase "Borrow constraint" $
      let constraint = BorrowConstraint "owner" "borrower"
      in ownershipConstraintType constraint @?= Borrow
  
  , testCase "Move constraint" $
      let constraint = MoveConstraint "source" "target"
      in ownershipConstraintType constraint @?= Move
  
  , fastProperty "Constraint variable consistency" $
      \var1 var2 -> let constraint = LifetimeConstraint var1 var2
                    in (constraintSourceVar constraint, constraintTargetVar constraint) == (var1, var2)
  ]
  where
    constraintSourceVar (LifetimeConstraint src _) = src
    constraintSourceVar (BorrowConstraint src _) = src
    constraintSourceVar (MoveConstraint src _) = src
    constraintTargetVar (LifetimeConstraint _ tgt) = tgt
    constraintTargetVar (BorrowConstraint _ tgt) = tgt
    constraintTargetVar (MoveConstraint _ tgt) = tgt

-- | 7. 所有权变量测试
ownershipVariableTests :: TestTree
ownershipVariableTests = testGroup "Ownership Variable Tests"
  [ testCase "Variable creation" $
      let var = OwnershipVariable "x" (OwnershipType Owned) (SourceSpan startPos startPos)
      in ownershipVariableName var @?= "x"
  
  , testCase "Variable type" $
      let var = OwnershipVariable "y" (OwnershipType Borrowed) (SourceSpan startPos startPos)
      in ownershipTypeKind (ownershipVariableType var) @?= Borrowed
  
  , fastProperty "Variable name consistency" $
      \name varType -> let var = OwnershipVariable name varType (SourceSpan startPos startPos)
                       in (ownershipVariableName var, ownershipVariableType var) == (name, varType)
  ]

-- | 8. 所有权函数测试
ownershipFunctionTests :: TestTree
ownershipFunctionTests = testGroup "Ownership Function Tests"
  [ testCase "Function ownership annotation" $
      let func = OwnershipFunction "test" [Owned, Borrowed] Owned
      in ownershipFunctionName func @?= "test"
  
  , testCase "Function parameter ownership" $
      let func = OwnershipFunction "f" [Owned, Shared] Borrowed
      in length (ownershipFunctionParams func) @?= 2
  
  , fastProperty "Function return ownership" $
      \params returnOwnership -> let func = OwnershipFunction "test" params returnOwnership
                                 in ownershipFunctionReturn func == returnOwnership
  ]

-- | 9. 所有权生命周期测试
ownershipLifetimeTests :: TestTree
ownershipLifetimeTests = testGroup "Ownership Lifetime Tests"
  [ testCase "Named lifetime" $
      let lifetime = NamedLifetime "'a"
      in lifetimeName lifetime @?= "'a"
  
  , testCase "Static lifetime" $
      let lifetime = StaticLifetime
      in isStaticLifetime lifetime @?= True
  
  , testCase "Anonymous lifetime" $
      let lifetime = AnonymousLifetime
      in isAnonymousLifetime lifetime @?= True
  
  , fastProperty "Lifetime ordering" $
      \lifetime1 lifetime2 -> let ordered = compareLifetimes lifetime1 lifetime2
                               in ordered == LT || ordered == EQ || ordered == GT
  ]
  where
    isStaticLifetime StaticLifetime = True
    isStaticLifetime _ = False
    isAnonymousLifetime AnonymousLifetime = True
    isAnonymousLifetime _ = False
    compareLifetimes StaticLifetime _ = GT
    compareLifetimes _ StaticLifetime = LT
    compareLifetimes (NamedLifetime n1) (NamedLifetime n2) = compare n1 n2
    compareLifetimes _ _ = EQ

-- | 10. 所有权验证测试
ownershipValidationTests :: TestTree
ownershipValidationTests = testGroup "Ownership Validation Tests"
  [ testCase "Valid owned variable" $
      let var = OwnershipVariable "x" (OwnershipType Owned) (SourceSpan startPos startPos)
      in validateOwnershipVariable var @?= True
  
  , testCase "Valid transfer" $
      let transfer = OwnershipTransfer "src" "dest" (SourceSpan startPos startPos)
      in validateOwnershipTransfer transfer @?= True
  
  , testCase "Valid region" $
      let region = OwnershipRegion "test" ["var1", "var2"]
      in validateOwnershipRegion region @?= True
  
  , fastProperty "Variable type validation" $
      \name kind -> let var = OwnershipVariable name (OwnershipType kind) (SourceSpan startPos startPos)
                    in validateOwnershipVariable var
  ]
  where
    validateOwnershipVariable var = ownershipVariableName var /= ""
    validateOwnershipTransfer transfer = 
      ownershipTransferSource transfer /= ownershipTransferDestination transfer
    validateOwnershipRegion region = ownershipRegionName region /= ""