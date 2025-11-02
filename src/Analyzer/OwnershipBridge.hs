module Analyzer.OwnershipBridge (
    runOwnershipAnalysis
) where

import Analyzer.State
import Analyzer.SymbolTable (isReservedName)
import Analyzer.Types
import qualified Ownership as Own

import Control.Monad.State
import qualified Data.Map.Strict as Map

runOwnershipAnalysis :: String -> IntegratedAnalyzer [Own.OwnershipError]
runOwnershipAnalysis code = do
    let ownershipErrs = Own.analyzeOwnership code
    mapM_ (addOwnershipError Error) ownershipErrs
    updateSymbolTableWithOwnership ownershipErrs
    symbols <- gets symbolTable
    pure $ filterSignificantOwnershipErrors ownershipErrs symbols

updateSymbolTableWithOwnership :: [Own.OwnershipError] -> IntegratedAnalyzer ()
updateSymbolTableWithOwnership ownershipErrs =
    modify $ \s -> s { symbolTable = updateOwnershipSymbols (symbolTable s) ownershipErrs }
  where
    updateOwnershipSymbols :: Map.Map String SymbolInfo -> [Own.OwnershipError] -> Map.Map String SymbolInfo
    updateOwnershipSymbols symbols errors = foldl updateSymbolForOwnership symbols errors

    updateSymbolForOwnership :: Map.Map String SymbolInfo -> Own.OwnershipError -> Map.Map String SymbolInfo
    updateSymbolForOwnership symbols (Own.UseAfterMove varName) =
        Map.adjust (\sym -> sym { isMoved = True }) varName symbols
    updateSymbolForOwnership symbols (Own.DoubleMove varName _) =
        Map.adjust (\sym -> sym { isMoved = True }) varName symbols
    updateSymbolForOwnership symbols (Own.BorrowWhileMoved varName) =
        Map.adjust (\sym -> sym { isMoved = True, isBorrowed = True }) varName symbols
    updateSymbolForOwnership symbols _ = symbols

filterSignificantOwnershipErrors :: [Own.OwnershipError] -> Map.Map String SymbolInfo -> [Own.OwnershipError]
filterSignificantOwnershipErrors errors symbols = filter isSignificant ownershipErrorsWithScope
  where
    isSignificant (Own.UseAfterMove varName) =
        length varName > 1 && not (isReservedName varName) && Map.member varName symbols
    isSignificant _ = True

    ownershipErrorsWithScope = errors
