module Ownership.Reporter
  ( formatOwnershipErrors
  ) where

import Data.List (intercalate)

import Ownership.Common.Types (OwnershipError(..))

formatOwnershipErrors :: [OwnershipError] -> String
formatOwnershipErrors = intercalate "; " . map formatError
  where
    formatError (UseAfterMove var) = "Use after move: " ++ var
    formatError (DoubleMove src dest) = "Double move: " ++ src ++ " to " ++ dest
    formatError (BorrowWhileMoved var) = "Borrow while moved: " ++ var
    formatError (MutBorrowWhileBorrowed var) = "Mutable borrow while borrowed: " ++ var
    formatError (BorrowWhileMutBorrowed var) = "Borrow while mut borrowed: " ++ var
    formatError (MultipleMutBorrows var) = "Multiple mutable borrows: " ++ var
    formatError (UseWhileMutBorrowed var) = "Use while mut borrowed: " ++ var
    formatError (OutOfScope var) = "Out of scope: " ++ var
    formatError (BorrowError var) = "Borrow error: " ++ var
    formatError (ParseError msg) = "Parse error: " ++ msg
    formatError (CrossFunctionMove src dest) = "Cross-function move: " ++ src ++ " to " ++ dest
    formatError (ParameterMoveMismatch param) = "Parameter move mismatch: " ++ param
    formatError (ControlFlowError msg) = "Control flow error: " ++ msg
    formatError (PathSensitiveError msg) = "Path sensitive error: " ++ msg
    formatError (LoopOwnershipError msg) = "Loop ownership error: " ++ msg
