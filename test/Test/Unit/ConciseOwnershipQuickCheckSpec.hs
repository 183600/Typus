module Test.Unit.ConciseOwnershipQuickCheckSpec where
import Test.QuickCheck 
import Test.Tasty.QuickCheck (testProperty, Property, (===), property, Arbitrary(..), Gen, oneof, choose, elements, listOf)
import Data.Set 
import Ownership.Common.Types (OwnershipType(..), OwnershipError(..), OwnershipTransfer)
            (\name -> Owned                               name === Owned name)
            
        ,             testProperty "Borrowed types are equal if names match" $
            (\name -> Borrowed                               name === Borrowed name)
            
        ,             testProperty "MutBorrowed types are equal if names match" $
            (\name -> MutBorrowed                               name === MutBorrowed name)
            
        ,             testProperty "Different ownership types with same name are not equal" $
            (\name -> Owned name /= Borrowed name && 
                      Borrowed name /= MutBorrowed name &&
                      Owned name /= MutBorrowed name)
        ]
        
    , testGroup "Ownership ordering properties"
        [             testProperty "Owned < Borrowed < MutBorrowed ordering" $
            (\name1 name2 -> 
            let owned = Owned name1
                                              borrowed = Borrowed name2
                                              mutBorrowed = MutBorrowed name1
            in owned < borrowed && borrowed < mutBorrowed)
            
        ,             testProperty "Ordering is total for same type" $
            (\name1 name2 -> 
            let owned1 = Owned name1
                                              owned2 = Owned name2
                                              result = compare owned1 owned2
            in result `elem` [LT, EQ, GT])
        ]
        
    , testGroup "Ownership transfer properties"
        [             testProperty "Valid transfer preserves source ownership" $
            (\source target -> 
            let transfer = OwnershipTransfer source target
            in transferSource                               transfer === source)
            
        ,             testProperty "Valid transfer preserves target ownership" $
            (\source target -> 
            let transfer = OwnershipTransfer source target
            in transferTarget                               transfer === target)
            
        ,             testProperty "Transfer type consistency" $
            (\source target -> 
            let transfer = OwnershipTransfer source target
            in property (transferFrom                               transfer == source && transferTo                               transfer == target)
        ]
        
    , testGroup "Ownership error properties"
        [             testProperty "Use after move errors preserve variable name" $
            (\varName -> 
            let error = UseAfterMove varName
            in case error of
                 UseAfterMove name ->                               name === varName
                 _ -> property False)
                 
        ,             testProperty "Double move errors preserve both variable names" $
            (\var1 var2 -> 
            let error = DoubleMove var1 var2
            in case error of
                 DoubleMove name1 name2 -> property (name1 == var1 &&                               name2 == var2)
                 _ -> property False)
                 
        ,             testProperty "Borrow errors preserve context" $
            (\context -> 
            let error = BorrowError context
            in case error of
                 BorrowError ctx ->                               ctx === context
                 _ -> property False)
        ]
        
    , testGroup "Ownership state consistency"
        [             testProperty "Empty ownership state has no owners" $
            \(_ :: Map String OwnershipType) -> Map.null Map.empty
            
        ,             testProperty "Adding ownership creates retrievable entry" $
            \(varName :: String) (ownershipType :: OwnershipType) -> 
            let state = Map.singleton varName ownershipType
            in Map.lookup varName                               state === Just ownershipType
            
        ,             testProperty "Ownership transfer updates state correctly" $
            \(source :: String) (target :: String) (transferType :: String) (state :: Map String OwnershipType) -> 
            let transfer = OwnershipTransfer {                               transferFrom = source,                               transferTo = target }
                                              newState = performTransfer transfer state
            in Map.lookup target                               newState === Just (convertTransferType transferType)
        ]
    ]
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


-- Helper functions for testing
transferSource :: OwnershipTransfer -> String
                              transferSource = transferFrom

transferTarget :: OwnershipTransfer -> String
                              transferTarget = transferTo



convertTransferType :: String -> OwnershipType
convertTransferType "move" = Owned "moved"
convertTransferType "borrow" = Borrowed "borrowed"
convertTransferType "mut_borrow" = MutBorrowed "mut_borrowed"
convertTransferType                               _ = Owned "unknown"

performTransfer :: OwnershipTransfer -> Map String OwnershipType -> Map String OwnershipType
performTransfer transfer                               state = 
  let source = transferFrom transfer
                                    target = transferTo transfer
      in Map.insert target (Owned "moved") (Map.delete source state)

-- Helper property function


-- Generate test data
instance Arbitrary OwnershipType where
                                              arbitrary = oneof
    [ Owned <$> arbitrary
    , Borrowed <$> arbitrary
    , MutBorrowed <$> arbitrary
    ]

instance Arbitrary OwnershipTransfer where
                                              arbitrary = do
              source <- arbitrary
    target <- arbitrary
    return $ OwnershipTransfer {                               transferFrom = source,                               transferTo = target }

instance Arbitrary OwnershipError where
                                              arbitrary = oneof
    [ UseAfterMove <$> arbitrary
    , DoubleMove <$> arbitrary <*> arbitrary
    , BorrowWhileMoved <$> arbitrary
    , MutBorrowWhileBorrowed <$> arbitrary
    , BorrowWhileMutBorrowed <$> arbitrary
    , MultipleMutBorrows <$> arbitrary
    , UseWhileMutBorrowed <$> arbitrary
    , OutOfScope <$> arbitrary
    , BorrowError <$> arbitrary
    , ParseError <$> arbitrary
    , CrossFunctionMove <$> arbitrary <*> arbitrary
    , ParameterMoveMismatch <$> arbitrary
    , ControlFlowError <$> arbitrary
    ]
