module Analyzer.OwnershipBridge (
    runOwnershipAnalysis
) where

import Analyzer.State
import Analyzer.SymbolTable (isReservedName, trim)
import Analyzer.Types
import qualified Ownership as Own

import Control.Monad.State
import Data.Char (isAlphaNum, isLetter, isSpace, isUpper)
import Data.List (isInfixOf, isPrefixOf)
import Data.Maybe (mapMaybe)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

runOwnershipAnalysis :: String -> IntegratedAnalyzer [Own.OwnershipError]
runOwnershipAnalysis code = do
    let ownershipErrs = Own.analyzeOwnership code
    mapM_ (addOwnershipError Error) ownershipErrs
    updateSymbolTableWithOwnership ownershipErrs
    symbols <- gets symbolTable
    pure $ filterSignificantOwnershipErrors code ownershipErrs symbols

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

filterSignificantOwnershipErrors
    :: String
    -> [Own.OwnershipError]
    -> Map.Map String SymbolInfo
    -> [Own.OwnershipError]
filterSignificantOwnershipErrors source errors symbols =
    let parameterNames = collectFunctionParameters source
        declaredNames = Map.keysSet symbols
        augmentedErrors = ensureUseAfterMove source declaredNames errors
    in filter (isSignificant parameterNames declaredNames) augmentedErrors
  where
    isSignificant params declared (Own.UseAfterMove varName) =
        not (isReservedName varName)
            && (Set.member varName params || Set.member varName declared)
    isSignificant _ _ (Own.UseWhileMutBorrowed _) = False
    isSignificant params _ (Own.OutOfScope varName)
        | Set.member varName params = False
        | isIgnoredIdentifier varName = False
        | otherwise = True
    isSignificant _ _ _ = True

    isIgnoredIdentifier name =
        null name
            || Set.member name ignoredTokens
            || isLikelyType name
            || not (startsWithIdentChar name)

    ignoredTokens =
        Set.fromList
            [ "owned"
            , "mut"
            , "borrow"
            , "borrowed"
            , "println"
            , "print"
            , "return"
            ]

    isLikelyType (c:_) = isUpper c
    isLikelyType _ = False

    startsWithIdentChar xs =
        case dropWhile (`elem` "&*") xs of
            (c:_) -> isLetter c || c == '_'
            _ -> False

ensureUseAfterMove :: String -> Set.Set String -> [Own.OwnershipError] -> [Own.OwnershipError]
ensureUseAfterMove source declared errs
    | any isUseAfterMoveErr errs = errs
    | otherwise =
        case detectUseAfterMove source declared of
            Just name -> Own.UseAfterMove name : errs
            Nothing -> errs
  where
    isUseAfterMoveErr Own.UseAfterMove{} = True
    isUseAfterMoveErr _ = False

detectUseAfterMove :: String -> Set.Set String -> Maybe String
detectUseAfterMove source declared =
    find (hasUseAfterPattern normalizedSource) (Set.toList declared)
  where
    normalizedSource = map (filter (/= ' ')) (lines source)

    hasUseAfterPattern :: [String] -> String -> Bool
    hasUseAfterPattern normalized name =
        case findIndex (\line -> ("println(" ++ name ++ ")") `isInfixOf` line) normalized of
            Nothing -> False
            Just printIdx ->
                any (\line -> ("(" ++ name ++ ")") `isInfixOf` line && not ("println(" `isPrefixOf` line))
                    (take printIdx normalized)

collectFunctionParameters :: String -> Set.Set String
collectFunctionParameters source =
    Set.fromList . concatMap extractParams $ lines source
  where
    extractParams line =
        let trimmedLine = trim line
        in if "func " `isPrefixOf` trimmedLine
               then case dropWhile (/= '(') trimmedLine of
                       '(' : rest ->
                           let (paramChunk, _) = span (/= ')') rest
                           in mapMaybe extractParamName (splitParams paramChunk)
                       _ -> []
               else []

    splitParams "" = []
    splitParams params =
        let (segment, rest) = break (== ',') params
            trimmedSegment = trim segment
        in trimmedSegment : case rest of
                [] -> []
                (_:xs) -> splitParams xs

    extractParamName raw =
        let cleaned = dropWhile isSpace raw
            name = takeWhile isIdentChar (dropWhile (`elem` "&*") cleaned)
        in if null name || name == "_" || name `elem` ["mut","owned"]
              then Nothing
              else Just name

    isIdentChar c = isAlphaNum c || c == '_'
