module Analyzer.CrossAnalysis (
    runCrossAnalysis
) where

import Analyzer.State
import Analyzer.Types
import qualified Dependencies as Dep
import qualified Ownership as Own

import Control.Monad.State
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Char (isDigit, isLower)
import Data.List (isPrefixOf)
import Data.Maybe (mapMaybe, fromMaybe)
import Compiler.GoAst (parseGoModule)
import Ownership.Common.Lexer (Token(..), TokenKind(..))
import Ownership.Lexer (OwnershipToken, Sym(..), lexAll)

runCrossAnalysis :: String -> IntegratedAnalyzer [CombinedError]
runCrossAnalysis code = do
    symbols <- gets symbolTable
    crossErrors <- checkCrossAnalyzerIssues code symbols
    mapM_ addCombinedError crossErrors
    pure crossErrors

checkCrossAnalyzerIssues :: String -> Map.Map String SymbolInfo -> IntegratedAnalyzer [CombinedError]
checkCrossAnalyzerIssues code symbols = do
    conflicts <- checkOwnershipTypeConflicts symbols
    inconsistencies <- checkTypeOwnershipInconsistencies code symbols
    let unusedWarnings = checkUnusedVariables code symbols
    pure $ conflicts ++ inconsistencies ++ unusedWarnings

checkOwnershipTypeConflicts :: Map.Map String SymbolInfo -> IntegratedAnalyzer [CombinedError]
checkOwnershipTypeConflicts symbols = do
    let msgs = Map.foldlWithKey findConflicts [] symbols
    pure $ map (\s -> CrossAnalyzerError s Error []) msgs
  where
    findConflicts :: [String] -> String -> SymbolInfo -> [String]
    findConflicts acc _name symbol =
        case (symbolType symbol, ownershipState symbol) of
            (Just (Dep.TVCon _), Just (Own.Owned _)) ->
                if isMoved symbol
                    then ("Variable '" ++ symbolName symbol ++ "' has dependent type but was moved") : acc
                    else acc
            _ -> acc

checkTypeOwnershipInconsistencies :: String -> Map.Map String SymbolInfo -> IntegratedAnalyzer [CombinedError]
checkTypeOwnershipInconsistencies code symbols = do
    let linesOfCode = lines code
        inconsistencies = concatMap (checkLineInconsistencies symbols) (zip [1 ..] linesOfCode)
    pure $ map (\s -> CrossAnalyzerError s Error []) inconsistencies

checkLineInconsistencies :: Map.Map String SymbolInfo -> (Int, String) -> [String]
checkLineInconsistencies symbols (lineNum, line) =
    let usedVars = extractVariablesFromLine line
        relevantSymbols = mapMaybe (`Map.lookup` symbols) usedVars
    in concatMap (checkSymbolInconsistency lineNum) relevantSymbols

checkSymbolInconsistency :: Int -> SymbolInfo -> [String]
checkSymbolInconsistency lineNum symbol
    | isMoved symbol && isBorrowed symbol =
        ["Symbol '" ++ symbolName symbol ++ "' at line " ++ show lineNum ++ " is both moved and borrowed"]
    | otherwise = []

data UsageSummary = UsageSummary
    { usageByScope :: Map.Map (String, Int) Int
    , usageTotals :: Map.Map String Int
    }

checkUnusedVariables :: String -> Map.Map String SymbolInfo -> [CombinedError]
checkUnusedVariables code symbols
    | null trackedSymbols = []
    | otherwise =
        case parseGoModule (lines code) of
            Left _ -> []
            Right _ ->
                let tokens = lexAll code
                    usageSummary = computeUsageSummary trackedNameSet tokens
                in mapMaybe (unusedWarning usageSummary) trackedSymbols
  where
    trackedSymbols = filter isTrackedSymbol (Map.elems symbols)

    trackedNameSet = Set.fromList (map symbolName trackedSymbols)

    isTrackedSymbol symbol =
        case ownershipState symbol of
            Just (Own.Owned _) -> isWarnable (symbolName symbol)
            _ -> False

    unusedWarning summary symbol
        | isMoved symbol || isBorrowed symbol = Nothing
        | usageCount summary symbol > 1 = Nothing
        | otherwise =
            Just $ CrossAnalyzerError ("Variable '" ++ symbolName symbol ++ "' declared but never used") Warning []

    usageCount UsageSummary{..} symbol =
        let name = symbolName symbol
            scopeKey = (name, symbolScope symbol)
        in fromMaybe (Map.findWithDefault 0 name usageTotals) (Map.lookup scopeKey usageByScope)

    isWarnable name =
        case name of
            (c:_) -> name /= "_" && not (isKeyword name) && isLower c
            [] -> False

computeUsageSummary :: Set.Set String -> [OwnershipToken] -> UsageSummary
computeUsageSummary trackedNames tokens =
    go tokens 0 False Map.empty Map.empty
  where
    go [] _ _ scoped totals = UsageSummary scoped totals
    go (Token kind _ : rest) depth lastDot scoped totals =
        case kind of
            TSym SLBrace ->
                go rest (depth + 1) False scoped totals
            TSym SRBrace ->
                go rest (max 0 (depth - 1)) False scoped totals
            TSym SDot ->
                go rest depth True scoped totals
            TId ident ->
                let shouldCount =
                        Set.member ident trackedNames
                            && not lastDot
                            && not (nextIsColon rest)
                    scoped' =
                        if shouldCount
                            then Map.insertWith (+) (ident, depth) 1 scoped
                            else scoped
                    totals' =
                        if shouldCount
                            then Map.insertWith (+) ident 1 totals
                            else totals
                in go rest depth False scoped' totals'
            _ ->
                go rest depth False scoped totals

    nextIsColon (Token (TSym SColon) _ : _) = True
    nextIsColon _ = False

extractVariablesFromLine :: String -> [String]
extractVariablesFromLine line =
    let wordsList = words line
        isVariable word =
            not (null word)
                && not (isKeyword word)
                && not (isOperator word)
                && not ("\"" `isPrefixOf` word)
                && not ("'" `isPrefixOf` word)
                && case word of
                    [] -> False
                    (c : _) -> not (isDigit c)
    in filter isVariable wordsList

isKeyword :: String -> Bool
isKeyword word =
    word
        `elem` [ "func"
               , "var"
               , "let"
               , "if"
               , "else"
               , "for"
               , "return"
               , "import"
               , "package"
               , "type"
               , "struct"
               , "interface"
               , "const"
               ]

isOperator :: String -> Bool
isOperator word =
    word
        `elem` [ "+"
               , "-"
               , "*"
               , "/"
               , "="
               , ":="
               , "=="
               , "!="
               , "<"
               , ">"
               , "<="
               , ">="
               , "&&"
               , "||"
               , "!"
               , "&"
               , "|"
               , "^"
               ]


