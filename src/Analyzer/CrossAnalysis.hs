module Analyzer.CrossAnalysis (
    runCrossAnalysis
) where

import Analyzer.State
import Analyzer.Types
import qualified Dependencies as Dep
import qualified Ownership as Own

import Control.Monad.State
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Char (isDigit, isLower)
import Data.List (isPrefixOf)
import Data.Maybe (mapMaybe, fromMaybe)
import Compiler.GoAst (parseGoModule)
import Ownership.Common.Lexer (Token(..), TokenKind(..))
import Ownership.Lexer (OwnershipToken, Sym(..), lexAll)
import Ownership.Parser (Program(..), Stmt(..), Expr(..), parseProgram)

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

    usageCount UsageSummary{usageTotals = usageTotalsMap, usageByScope = usageByScopeMap} symbol =
        let name = symbolName symbol
            scopeKey = (name, symbolScope symbol)
        in fromMaybe (Map.findWithDefault 0 name usageTotalsMap) (Map.lookup scopeKey usageByScopeMap)

    isWarnable name =
        case name of
            (c:_) -> name /= "_" && not (isKeyword name) && isLower c
            [] -> False

computeUsageSummary :: Set.Set String -> [OwnershipToken] -> UsageSummary
computeUsageSummary trackedNames tokens =
    let program = parseProgram tokens
        occurrences = collectOccurrences trackedNames program
    in List.foldl' updateSummary (UsageSummary Map.empty Map.empty) occurrences
  where
    updateSummary (UsageSummary scoped totals) (name, depth) =
        let scoped' = Map.insertWith (+) (name, depth) 1 scoped
            totals' = Map.insertWith (+) name 1 totals
        in UsageSummary scoped' totals'

collectOccurrences :: Set.Set String -> Program -> [(String, Int)]
collectOccurrences tracked (Program stmts) = goStmts 0 stmts
  where
    goStmts depth = concatMap (goStmt depth)

    goStmt depth stmt =
        case stmt of
            SVarDecl name mInit _ ->
                record name depth ++ maybe [] (goExpr depth) mInit
            SLetDecl name mInit _ ->
                record name depth ++ maybe [] (goExpr depth) mInit
            SAssignStmt name _ expr _ ->
                record name depth ++ goExpr depth expr
            SExpr expr _ ->
                goExpr depth expr
            SBlock body _ ->
                goStmts (depth + 1) body
            SFunc body _ ->
                goStmts (depth + 1) body
            SFor body _ ->
                goStmts (depth + 1) body
            SDirectiveBlock _ body _ ->
                goStmts (depth + 1) body
            SDirectiveLine _ _ ->
                []

    goExpr depth expr =
        case expr of
            EIdent name _ ->
                record name depth
            ECall name args _ ->
                record name depth ++ concatMap (goExpr depth) args
            EMethodCall _ receiver args _ ->
                goExpr depth receiver ++ concatMap (goExpr depth) args
            EUnary _ inner _ ->
                goExpr depth inner
            EUnknown toks _ ->
                collectFromTokens depth toks
            _ ->
                []

    collectFromTokens depth toks =
        [ (name, depth)
        | name <- collectPlainIdentifiers toks ++ collectSelectorBases toks
        , Set.member name tracked
        ]

    record name depth
        | Set.member name tracked = [(name, depth)]
        | otherwise = []

collectPlainIdentifiers :: [OwnershipToken] -> [String]
collectPlainIdentifiers tokens = go Nothing tokens
  where
    go _ [] = []
    go prev (current:rest) =
        let next = case rest of
                (x:_) -> Just x
                [] -> Nothing
            remainder = go (Just current) rest
        in case current of
            Token (TId ident) _
                | not (isSymbolToken SDot prev)
                , not (isSymbolToken SDot next)
                , not (isSymbolToken SColon next)
                , not (isSymbolToken SLParen next) -> ident : remainder
                | otherwise -> remainder
            _ ->
                remainder

collectSelectorBases :: [OwnershipToken] -> [String]
collectSelectorBases = go []
  where
    go acc (Token (TId ident) _ : Token (TSym SDot) _ : Token (TId _) _ : rest) =
        go (ident : acc) rest
    go acc (_:rest) = go acc rest
    go acc [] = reverse acc

isSymbolToken :: Sym -> Maybe OwnershipToken -> Bool
isSymbolToken sym = maybe False matches
  where
    matches (Token (TSym sym') _) = sym == sym'
    matches _ = False

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


