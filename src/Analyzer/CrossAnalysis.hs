module Analyzer.CrossAnalysis (
    runCrossAnalysis
) where

import Analyzer.State
import Analyzer.Types
import qualified Dependencies as Dep
import qualified Ownership as Own

import Control.Monad.State
import qualified Data.Map.Strict as Map
import Data.Char (isAlphaNum, isDigit, isLower)
import Data.List (isPrefixOf)
import Data.Maybe (mapMaybe)

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

checkUnusedVariables :: String -> Map.Map String SymbolInfo -> [CombinedError]
checkUnusedVariables code symbols =
    let usageCounts = Map.fromListWith (+) [ (tok, 1 :: Int) | tok <- tokenizeIdentifiers code ]
        declared = Map.toList $ Map.filter isOwnedSymbol symbols
    in [ CrossAnalyzerError ("Variable '" ++ name ++ "' declared but never used") Warning []
       | (name, info) <- declared
       , shouldWarn name info usageCounts
       ]
  where
    isOwnedSymbol SymbolInfo{ownershipState = Just (Own.Owned _)} = True
    isOwnedSymbol _ = False

    shouldWarn name _ counts =
        case Map.lookup name counts of
            Just occ -> occ <= 1 && isWarnable name
            Nothing  -> isWarnable name

    isWarnable name =
        case name of
            (c:_) -> name /= "_" && not (isKeyword name) && isLower c
            [] -> False

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

tokenizeIdentifiers :: String -> [String]
tokenizeIdentifiers [] = []
tokenizeIdentifiers (c : cs)
    | isIdentChar c =
        let (ident, rest) = span isIdentChar (c : cs)
        in ident : tokenizeIdentifiers rest
    | otherwise = tokenizeIdentifiers cs
  where
    isIdentChar ch = isAlphaNum ch || ch == '_'
