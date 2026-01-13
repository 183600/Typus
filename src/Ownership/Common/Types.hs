module Ownership.Common.Types (
    OwnershipType(..),
    OwnershipError(..),
    OwnershipAnalyzer(..),
    OwnershipTransfer(..),
    newOwnershipAnalyzer
) where

-- | Ownership categories tracked by the analyzer
-- The constructors intentionally cover the superset of variants used across
-- different ownership analyzers (basic, advanced, control-flow enhanced, etc.).
data OwnershipType
    = Owned String          -- ^ Value is uniquely owned by the binding
    | Borrowed String       -- ^ Immutable borrow referencing the owner name
    | MutBorrowed String    -- ^ Mutable borrow referencing the owner name
    deriving (Eq)

instance Show OwnershipType where
    show (Owned name) = "Owned " ++ name
    show (Borrowed name) = "Borrowed " ++ name
    show (MutBorrowed name) = "MutBorrowed " ++ name

instance Ord OwnershipType where
    compare (Owned a) (Owned b) = compare a b
    compare (Owned _) (Borrowed _) = LT
    compare (Owned _) (MutBorrowed _) = LT
    compare (Borrowed a) (Borrowed b) = compare a b
    compare (Borrowed _) (MutBorrowed _) = LT
    compare (Borrowed _) (Owned _) = GT
    compare (MutBorrowed a) (MutBorrowed b) = compare a b
    compare (MutBorrowed _) (Owned _) = GT
    compare (MutBorrowed _) (Borrowed _) = GT

-- | Exhaustive ownership error taxonomy shared by all analyzers.
-- Individual analyzers may only emit a subset of these constructors but they
-- reuse the same definition to simplify error aggregation and reporting.
data OwnershipError
    = UseAfterMove String
    | DoubleMove String String
    | BorrowWhileMoved String
    | MutBorrowWhileBorrowed String
    | BorrowWhileMutBorrowed String
    | MultipleMutBorrows String
    | UseWhileMutBorrowed String
    | OutOfScope String
    | BorrowError String
    | ParseError String
    | CrossFunctionMove String String
    | ParameterMoveMismatch String
    | ControlFlowError String
    | PathSensitiveError String
    | LoopOwnershipError String
    | OwnershipError String
    deriving (Eq)

instance Show OwnershipError where
    show (UseAfterMove var) = "UseAfterMove " ++ var
    show (DoubleMove var1 var2) = "DoubleMove " ++ var1 ++ " " ++ var2
    show (BorrowWhileMoved var) = "BorrowWhileMoved " ++ var
    show (MutBorrowWhileBorrowed var) = "MutBorrowWhileBorrowed " ++ var
    show (BorrowWhileMutBorrowed var) = "BorrowWhileMutBorrowed " ++ var
    show (MultipleMutBorrows var) = "MultipleMutBorrows " ++ var
    show (UseWhileMutBorrowed var) = "UseWhileMutBorrowed " ++ var
    show (OutOfScope var) = "OutOfScope " ++ var
    show (BorrowError msg) = "BorrowError " ++ msg
    show (ParseError msg) = "ParseError " ++ msg
    show (CrossFunctionMove var1 var2) = "CrossFunctionMove " ++ var1 ++ " " ++ var2
    show (ParameterMoveMismatch var) = "ParameterMoveMismatch " ++ var
    show (ControlFlowError msg) = "ControlFlowError " ++ msg
    show (PathSensitiveError msg) = "PathSensitiveError " ++ msg
    show (LoopOwnershipError msg) = "LoopOwnershipError " ++ msg
    show (OwnershipError msg) = "OwnershipError " ++ msg

instance Ord OwnershipError where
    compare err1 err2 = compare (show err1) (show err2)

-- | Lightweight handle that keeps the public API stable while allowing the
-- implementation to evolve behind the scenes.
newtype OwnershipAnalyzer = OwnershipAnalyzer () deriving (Show, Eq)

-- | Ownership transfer operation between variables
data OwnershipTransfer = OwnershipTransfer
  { transferFrom :: String
  , transferTo :: String
  } deriving (Show, Eq)

-- | Smart constructor used by higher level code to obtain an analyzer handle.
newOwnershipAnalyzer :: OwnershipAnalyzer
newOwnershipAnalyzer = OwnershipAnalyzer ()
