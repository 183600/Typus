module Ownership.Common.Types (
    OwnershipType(..),
    OwnershipError(..),
    OwnershipAnalyzer(..),
    newOwnershipAnalyzer
) where

-- | Ownership categories tracked by the analyzer
-- The constructors intentionally cover the superset of variants used across
-- different ownership analyzers (basic, advanced, control-flow enhanced, etc.).
data OwnershipType
    = Owned String          -- ^ Value is uniquely owned by the binding
    | Borrowed String       -- ^ Immutable borrow referencing the owner name
    | MutBorrowed String    -- ^ Mutable borrow referencing the owner name
    deriving (Show, Eq)

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
    deriving (Show, Eq)

-- | Lightweight handle that keeps the public API stable while allowing the
-- implementation to evolve behind the scenes.
newtype OwnershipAnalyzer = OwnershipAnalyzer () deriving (Show, Eq)

-- | Smart constructor used by higher level code to obtain an analyzer handle.
newOwnershipAnalyzer :: OwnershipAnalyzer
newOwnershipAnalyzer = OwnershipAnalyzer ()
