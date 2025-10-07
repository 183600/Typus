{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module AnalyzerIntegration (
    IntegratedAnalyzer,
    AnalyzerState(..),
    AnalysisResult(..),
    CombinedError(..),
    ErrorSeverity(..),
    newIntegratedAnalyzer,
    runIntegratedAnalysis,
    analyzeCodeWithBothAnalyzers,
    getCombinedErrors,
    getAnalysisSummary,
    -- Re-export for convenience
    Own.OwnershipError(UseAfterMove, DoubleMove, BorrowWhileMoved, MutBorrowWhileBorrowed, BorrowWhileMutBorrowed, MultipleMutBorrows, UseWhileMutBorrowed, OutOfScope, ParseError),
    Own.OwnershipType(..),
    Dep.TypeVar(..),
    Dep.Constraint(..)
) where

import qualified Data.Map.Strict as Map
import Control.Monad.State
import Control.Monad.Except
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)
import Data.Char (isSpace, isAlphaNum, isDigit)
import Data.Maybe (mapMaybe)

-- Import both analyzers with qualified imports to avoid conflicts
import qualified Ownership as Own
import qualified Dependencies as Dep
-- import qualified DependentTypesParser as DTP

-- Error severity levels for prioritization
data ErrorSeverity = Error | Warning | Info
    deriving (Show, Eq, Ord)

-- Combined error type that unifies both analyzer errors
data CombinedError
    = OwnershipErrorCombined ErrorSeverity Own.OwnershipError
    | DependentTypeErrorCombined ErrorSeverity Dep.DependentTypeError
    | IntegrationError String ErrorSeverity
    | CrossAnalyzerError String ErrorSeverity [CombinedError]
    deriving (Show, Eq)

-- Analysis result structure
data AnalysisResult = AnalysisResult
    { ownershipErrors :: [Own.OwnershipError]
    , dependentTypeErrors :: [Dep.DependentTypeError]
    , analysisWarnings :: [String]
    , analysisInfo :: [String]
    , typeEnvironment :: Map.Map String Dep.TypeVar
    } deriving (Show, Eq)

-- Symbol information for cross-analyzer communication
data SymbolInfo = SymbolInfo
    { symbolName :: String
    , symbolType :: Maybe Dep.TypeVar
    , ownershipState :: Maybe Own.OwnershipType
    , symbolScope :: Int
    , isMoved :: Bool
    , isBorrowed :: Bool
    , constraints :: [Dep.Constraint]
    } deriving (Show, Eq)

-- Analysis phases
data AnalysisPhase = InitialPhase | OwnershipPhase | DependentTypePhase | IntegrationPhase
    deriving (Show, Eq)

-- Analysis context for cross-analyzer communication
data AnalysisContext = AnalysisContext
    { enableOwnership :: Bool
    , enableDependentTypes :: Bool
    , currentFile :: String
    , analysisPhase :: AnalysisPhase
    } deriving (Show, Eq)

-- Integrated analyzer state
data AnalyzerState = AnalyzerState
    { ownershipAnalyzer :: Own.OwnershipAnalyzer
    , dependentTypeChecker :: Dep.DependentTypeChecker
    , currentScope :: Int
    , symbolTable :: Map.Map String SymbolInfo
    , analysisContext :: AnalysisContext
    , combinedErrors :: [CombinedError]
    , ownershipErrorsAcc :: [Own.OwnershipError]
    , dependentTypeErrorsAcc :: [Dep.DependentTypeError]
    } deriving (Show, Eq)

-- Integrated analyzer type
type IntegratedAnalyzer = StateT AnalyzerState (ExceptT String IO)

-- Create new integrated analyzer
newIntegratedAnalyzer :: Bool -> Bool -> AnalyzerState
newIntegratedAnalyzer enableOwnershipFlag enableDependentTypesFlag = AnalyzerState
    { ownershipAnalyzer = Own.newOwnershipAnalyzer
    , dependentTypeChecker = Dep.newDependentTypeChecker
    , currentScope = 0
    , symbolTable = Map.empty
    , analysisContext = AnalysisContext
        { enableOwnership = enableOwnershipFlag
        , enableDependentTypes = enableDependentTypesFlag
        , currentFile = ""
        , analysisPhase = InitialPhase
        }
    , combinedErrors = []
    , ownershipErrorsAcc = []
    , dependentTypeErrorsAcc = []
    }

-- Run integrated analysis
runIntegratedAnalysis :: String -> AnalyzerState -> IO (Either String AnalysisResult)
runIntegratedAnalysis code initialState = do
    result <- runExceptT $ runStateT (analyzeCodeWithBothAnalyzers code) initialState
    return $ case result of
        Left err -> Left err
        Right (analysisResult, _) -> Right analysisResult

-- Main analysis function that coordinates both analyzers
analyzeCodeWithBothAnalyzers :: String -> IntegratedAnalyzer AnalysisResult
analyzeCodeWithBothAnalyzers code = do
    -- Set analysis context
    modify $ \s -> s { analysisContext = (analysisContext s) { currentFile = "<input>" } }

    -- Phase 1: Initial parsing and symbol collection
    setPhase InitialPhase
    symbols <- collectSymbolsAndTypes code
    modify $ \s -> s { symbolTable = symbols }

    -- Phase 2: Ownership analysis (if enabled)
    ownershipResults <- ifEnableOwnership [] $ do
        setPhase OwnershipPhase
        runOwnershipAnalysis code

    -- Phase 3: Dependent type analysis (if enabled)
    dependentTypeResults <- ifEnableDependentTypes [] $ do
        setPhase DependentTypePhase
        runDependentTypeAnalysis code

    -- Phase 4: Integration and cross-analysis
    setPhase IntegrationPhase
    integrationResults <- runCrossAnalysis code

    -- Combine all results
    combineAllResults ownershipResults dependentTypeResults integrationResults

-- Collect symbols and types from code
collectSymbolsAndTypes :: String -> IntegratedAnalyzer (Map.Map String SymbolInfo)
collectSymbolsAndTypes code = do
    let linesOfCode = lines code
    symbols <- mapM processLineForSymbols (zip [1..] linesOfCode)
    let combinedSymbols = foldr Map.union Map.empty symbols
    -- Validate and clean up the symbol table
    validatedSymbols <- validateSymbolTable combinedSymbols
    return validatedSymbols
  where
    processLineForSymbols :: (Int, String) -> IntegratedAnalyzer (Map.Map String SymbolInfo)
    processLineForSymbols (lineNum, line) = do
        let trimmed = trim line

        -- Skip comments and empty lines
        if "//" `isPrefixOf` trimmed || null trimmed || "/*" `isPrefixOf` trimmed || "*/" `isSuffixOf` trimmed
            then return Map.empty
            else do
                -- Process variable declarations
                if "var " `isPrefixOf` trimmed || ":=" `isInfixOf` trimmed
                    then processVariableDeclaration lineNum trimmed
                    else if "type " `isPrefixOf` trimmed
                         then processTypeDeclaration lineNum trimmed
                         else if "func " `isPrefixOf` trimmed
                              then processFunctionDeclaration lineNum trimmed
                              else if "const " `isPrefixOf` trimmed
                                   then processConstantDeclaration lineNum trimmed
                                   else return Map.empty

    processVariableDeclaration :: Int -> String -> IntegratedAnalyzer (Map.Map String SymbolInfo)
    processVariableDeclaration lineNum line = do
        let varName = extractVariableNameFromLine line
        let varType = extractTypeFromLine line
        if not (null varName) && isValidIdentifier varName
            then do
                symbolInfo <- createSymbolInfo varName varType lineNum
                return $ Map.singleton varName symbolInfo
            else return Map.empty

    processTypeDeclaration :: Int -> String -> IntegratedAnalyzer (Map.Map String SymbolInfo)
    processTypeDeclaration lineNum line = do
        let (typeName, typeParams, cs) = parseTypeDeclaration line
        if not (null typeName) && isValidIdentifier typeName
            then do
                -- Create a symbol for the type itself
                typeSymbol <- createTypeSymbolInfo typeName typeParams cs lineNum
                return $ Map.singleton typeName typeSymbol
            else return Map.empty

    processFunctionDeclaration :: Int -> String -> IntegratedAnalyzer (Map.Map String SymbolInfo)
    processFunctionDeclaration lineNum line = do
        let funcName = extractFunctionNameFromLine line
        if not (null funcName) && isValidIdentifier funcName
            then do
                symbolInfo <- createFunctionSymbolInfo funcName lineNum
                return $ Map.singleton funcName symbolInfo
            else return Map.empty

    processConstantDeclaration :: Int -> String -> IntegratedAnalyzer (Map.Map String SymbolInfo)
    processConstantDeclaration lineNum line = do
        let constName = extractConstantNameFromLine line
        if not (null constName) && isValidIdentifier constName
            then do
                symbolInfo <- createConstantSymbolInfo constName lineNum
                return $ Map.singleton constName symbolInfo
            else return Map.empty

    createSymbolInfo :: String -> Maybe Dep.TypeVar -> Int -> IntegratedAnalyzer SymbolInfo
    createSymbolInfo name mType _lineNum = do
        currentScope' <- gets currentScope
        return SymbolInfo
            { symbolName = name
            , symbolType = mType
            , ownershipState = Just $ Own.Owned name
            , symbolScope = currentScope'
            , isMoved = False
            , isBorrowed = False
            , constraints = []
            }

    createTypeSymbolInfo :: String -> [String] -> [Dep.Constraint] -> Int -> IntegratedAnalyzer SymbolInfo
    createTypeSymbolInfo name _params cs _lineNum = do
        currentScope' <- gets currentScope
        return SymbolInfo
            { symbolName = name
            , symbolType = Just $ Dep.TVCon name
            , ownershipState = Nothing
            , symbolScope = currentScope'
            , isMoved = False
            , isBorrowed = False
            , constraints = cs
            }

    createFunctionSymbolInfo :: String -> Int -> IntegratedAnalyzer SymbolInfo
    createFunctionSymbolInfo name _lineNum = do
        currentScope' <- gets currentScope
        return SymbolInfo
            { symbolName = name
            , symbolType = Just (Dep.TVFun [] (Dep.TVCon "void"))
            , ownershipState = Nothing
            , symbolScope = currentScope'
            , isMoved = False
            , isBorrowed = False
            , constraints = []
            }

    createConstantSymbolInfo :: String -> Int -> IntegratedAnalyzer SymbolInfo
    createConstantSymbolInfo name _lineNum = do
        currentScope' <- gets currentScope
        return SymbolInfo
            { symbolName = name
            , symbolType = Just $ Dep.TVCon "const"
            , ownershipState = Just $ Own.Owned name
            , symbolScope = currentScope'
            , isMoved = False
            , isBorrowed = False
            , constraints = []
            }

    -- Validate the symbol table for consistency
    validateSymbolTable :: Map.Map String SymbolInfo -> IntegratedAnalyzer (Map.Map String SymbolInfo)
    validateSymbolTable symbols = do
        let validatedSymbols = Map.filterWithKey validateSymbolEntry symbols
        return validatedSymbols
      where
        validateSymbolEntry :: String -> SymbolInfo -> Bool
        validateSymbolEntry name symbol =
            isValidIdentifier name &&
            not (isReservedName name) &&
            isValidSymbolInfo symbol

    isValidSymbolInfo :: SymbolInfo -> Bool
    isValidSymbolInfo symbol =
        not (null $ symbolName symbol) &&
        isValidIdentifier (symbolName symbol) &&
        symbolScope symbol >= 0

    isValidIdentifier :: String -> Bool
    isValidIdentifier name =
        not (null name) && not (isReservedName name) &&
        case name of
            [] -> False
            (c:_) -> not (isDigit c) &&
                     all (\char -> isAlphaNum char || char == '_') name


    extractFunctionNameFromLine :: String -> String
    extractFunctionNameFromLine line =
        let parts = words line
        in if length parts >= 2 && parts !! 0 == "func"
           then takeWhile (/= '(') (parts !! 1)
           else ""

    extractConstantNameFromLine :: String -> String
    extractConstantNameFromLine line =
        let parts = words line
        in if length parts >= 3 && parts !! 0 == "const"
           then parts !! 1
           else ""

-- Run ownership analysis with symbol table integration
runOwnershipAnalysis :: String -> IntegratedAnalyzer [Own.OwnershipError]
runOwnershipAnalysis code = do
    -- Run ownership analysis
    let ownershipErrs = Own.analyzeOwnership code

    -- Accumulate errors with severity and into typed lists
    mapM_ (addOwnershipError Error) ownershipErrs

    -- Update symbol table with ownership information
    updateSymbolTableWithOwnership ownershipErrs

    -- Get symbols for filtering
    symbols <- gets symbolTable

    -- Filter and prioritize errors
    let filteredErrors = filterSignificantOwnershipErrors ownershipErrs symbols

    return filteredErrors

-- Run dependent type analysis with symbol table integration
runDependentTypeAnalysis :: String -> IntegratedAnalyzer [Dep.DependentTypeError]
runDependentTypeAnalysis code = do
    -- Enhanced type analysis: first collect type definitions from code
    let typeDefinitions = extractTypeDefinitions code
    -- Create and store a type checker with pre-registered types
    let tc = Dep.newDependentTypeCheckerWithTypes typeDefinitions
    modify $ \s -> s { dependentTypeChecker = tc }

    -- Use the checker from state (ensures correct environment)
    _tc' <- gets dependentTypeChecker

    -- Run dependent type analysis with the configured checker
    let typeErrors = Dep.analyzeDependentTypes code

    -- Accumulate errors with severity and into typed lists
    mapM_ (addDependentTypeError Error) typeErrors

    -- Filter out TypeNotFound errors for known built-in types
    let filteredErrors = filterKnownTypeErrors typeErrors

    -- Update symbol table with type information (if any)
    updateSymbolTableWithTypes filteredErrors

    -- Get symbols for filtering
    symbols <- gets symbolTable

    -- Filter and prioritize errors
    let significantErrors = filterSignificantTypeErrors filteredErrors symbols

    return significantErrors

-- Run cross-analysis that checks for interactions between ownership and types
runCrossAnalysis :: String -> IntegratedAnalyzer [CombinedError]
runCrossAnalysis code = do
    symbols <- gets symbolTable
    crossErrors <- checkCrossAnalyzerIssues code symbols
    mapM_ addCombinedError crossErrors
    return crossErrors
  where
    checkCrossAnalyzerIssues :: String -> Map.Map String SymbolInfo -> IntegratedAnalyzer [CombinedError]
    checkCrossAnalyzerIssues code' symbols = do
        -- Conflicts between ownership and dependent types
        conflicts <- checkOwnershipTypeConflicts symbols
        -- Inconsistencies found by scanning code lines
        inconsistencies <- checkTypeOwnershipInconsistencies code' symbols
        return $ conflicts ++ inconsistencies

    checkOwnershipTypeConflicts :: Map.Map String SymbolInfo -> IntegratedAnalyzer [CombinedError]
    checkOwnershipTypeConflicts symbols = do
        let msgs = Map.foldlWithKey findConflicts [] symbols
        return $ map (\s -> CrossAnalyzerError s Error []) msgs
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
    checkTypeOwnershipInconsistencies code' symbols = do
        let linesOfCode = lines code'
            inconsistencies = concatMap (checkLineInconsistencies symbols) (zip [1..] linesOfCode)
        return $ map (\s -> CrossAnalyzerError s Error []) inconsistencies
      where
        checkLineInconsistencies :: Map.Map String SymbolInfo -> (Int, String) -> [String]
        checkLineInconsistencies _symbols' (lineNum, line) =
            let usedVars = extractVariablesFromLine line
                relevantSymbols = mapMaybe (`Map.lookup` symbols) usedVars
            in concatMap (checkSymbolInconsistency lineNum) relevantSymbols

        checkSymbolInconsistency :: Int -> SymbolInfo -> [String]
        checkSymbolInconsistency lineNum symbol =
            if isMoved symbol && isBorrowed symbol
            then ["Symbol '" ++ symbolName symbol ++ "' at line " ++ show lineNum ++ " is both moved and borrowed"]
            else []

-- Update symbol table with ownership information
updateSymbolTableWithOwnership :: [Own.OwnershipError] -> IntegratedAnalyzer ()
updateSymbolTableWithOwnership ownershipErrs = do
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

-- Update symbol table with type information (placeholder integration point)
updateSymbolTableWithTypes :: [Dep.DependentTypeError] -> IntegratedAnalyzer ()
updateSymbolTableWithTypes _typeErrors = do
    -- Future: refine symbol types from constraints or inferred info
    return ()

-- Combine all analysis results
combineAllResults :: [Own.OwnershipError] -> [Dep.DependentTypeError] -> [CombinedError] -> IntegratedAnalyzer AnalysisResult
combineAllResults ownershipErrs typeErrs integrationErrs = do
    symbols <- gets symbolTable
    return AnalysisResult
        { ownershipErrors = ownershipErrs
        , dependentTypeErrors = typeErrs
        , analysisWarnings = filterWarnings integrationErrs
        , analysisInfo = filterInfo integrationErrs
        , typeEnvironment = extractTypeEnvironment symbols
        }

-- Helper functions
setPhase :: AnalysisPhase -> IntegratedAnalyzer ()
setPhase phase = modify $ \s -> s { analysisContext = (analysisContext s) { analysisPhase = phase } }

ifEnableOwnership :: a -> IntegratedAnalyzer a -> IntegratedAnalyzer a
ifEnableOwnership def action = do
    enabled <- gets (enableOwnership . analysisContext)
    if enabled then action else return def

ifEnableDependentTypes :: a -> IntegratedAnalyzer a -> IntegratedAnalyzer a
ifEnableDependentTypes def action = do
    enabled <- gets (enableDependentTypes . analysisContext)
    if enabled then action else return def

addOwnershipError :: ErrorSeverity -> Own.OwnershipError -> IntegratedAnalyzer ()
addOwnershipError severity err = do
    modify $ \s -> s
        { ownershipErrorsAcc = ownershipErrorsAcc s ++ [err]
        , combinedErrors = combinedErrors s ++ [OwnershipErrorCombined severity err]
        }

addDependentTypeError :: ErrorSeverity -> Dep.DependentTypeError -> IntegratedAnalyzer ()
addDependentTypeError severity err = do
    modify $ \s -> s
        { dependentTypeErrorsAcc = dependentTypeErrorsAcc s ++ [err]
        , combinedErrors = combinedErrors s ++ [DependentTypeErrorCombined severity err]
        }

addCombinedError :: CombinedError -> IntegratedAnalyzer ()
addCombinedError err = modify $ \s -> s { combinedErrors = combinedErrors s ++ [err] }

-- Collect messages by severity (flatten nested combined errors)
filterWarnings :: [CombinedError] -> [String]
filterWarnings = collectMessages Warning

filterInfo :: [CombinedError] -> [String]
filterInfo = collectMessages Info

collectMessages :: ErrorSeverity -> [CombinedError] -> [String]
collectMessages sev = concatMap (go sev)
  where
    go :: ErrorSeverity -> CombinedError -> [String]
    go sev' (OwnershipErrorCombined s e) | s == sev' = [show e]
                                         | otherwise = []
    go sev' (DependentTypeErrorCombined s e) | s == sev' = [show e]
                                             | otherwise = []
    go sev' (IntegrationError msg s) | s == sev' = [msg]
                                     | otherwise = []
    go sev' (CrossAnalyzerError msg s subs) =
        (if s == sev' then [msg] else []) ++ concatMap (go sev') subs

extractTypeEnvironment :: Map.Map String SymbolInfo -> Map.Map String Dep.TypeVar
extractTypeEnvironment symbols = Map.mapMaybe symbolType symbols

-- Utility functions for parsing and filtering
trim :: String -> String
trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse

-- Split string by character delimiter
splitByChar :: Char -> String -> [String]
splitByChar delimiter s = case break (== delimiter) s of
    (a, []) -> [a]
    (a, _:b) -> a : splitByChar delimiter b

extractVariableNameFromLine :: String -> String
extractVariableNameFromLine line =
    let wordsList = words line
    in if "var" `isPrefixOf` line
       then if length wordsList >= 2 then wordsList !! 1 else ""
       else case break (== ':') line of
              (name, ':':'=':_) -> trim name
              _ -> ""

extractTypeFromLine :: String -> Maybe Dep.TypeVar
extractTypeFromLine line =
    let wordsList = words line
    in if "var" `isPrefixOf` line && length wordsList >= 3
       then Just $ Dep.TVCon (wordsList !! 2)
       else Nothing

parseTypeDeclaration :: String -> (String, [String], [Dep.Constraint])
parseTypeDeclaration line =
    let wordsList = words line
    in if length wordsList >= 2 && "type" `isPrefixOf` line
       then let typeName = wordsList !! 1
                typeParams = extractTypeParams (drop 2 wordsList)
                cs = []  -- Simplified for now; refined in extractTypeDefinitions
            in (typeName, typeParams, cs)
       else ("", [], [])

extractTypeParams :: [String] -> [String]
extractTypeParams =
    filter (not . null) . map (takeWhile (/= '>')) . filter (isPrefixOf "<")

extractVariablesFromLine :: String -> [String]
extractVariablesFromLine line =
    let wordsList = words line
        isVariable word = not (null word) &&
                          not (isKeyword word) &&
                          not (isOperator word) &&
                          not ("\"" `isPrefixOf` word) &&
                          not ("'" `isPrefixOf` word) &&
                          case word of
                              [] -> False
                              (c:_) -> not (isDigit c)
    in filter isVariable wordsList

isKeyword :: String -> Bool
isKeyword word = word `elem`
    ["func","var","let","if","else","for","return","import","package","type","struct","interface","const"]

isOperator :: String -> Bool
isOperator word = word `elem`
    ["+","-","*","/","=",
     ":=","==","!=","<",">","<=",">=",
     "&&","||","!","&","|","^"]

-- Error filtering functions
filterSignificantOwnershipErrors :: [Own.OwnershipError] -> Map.Map String SymbolInfo -> [Own.OwnershipError]
filterSignificantOwnershipErrors errors symbols =
    filter isSignificantOwnershipError errors
  where
    isSignificantOwnershipError (Own.UseAfterMove varName) =
        length varName > 1 && not (isReservedName varName) && Map.member varName symbols
    isSignificantOwnershipError _ = True

filterSignificantTypeErrors :: [Dep.DependentTypeError] -> Map.Map String SymbolInfo -> [Dep.DependentTypeError]
filterSignificantTypeErrors errors _symbols =
    filter isSignificantTypeError errors
  where
    isSignificantTypeError (Dep.DependentTypeMismatch t1 t2) =
        show t1 /= "" && show t2 /= ""
    isSignificantTypeError _ = True

isReservedName :: String -> Bool
isReservedName name =
    name `elem` ["fmt", "main", "func", "var", "let", "if", "else", "for", "return",
                 "import", "package", "type", "struct", "interface", "const",
                 "true", "false", "nil", "int", "string", "bool", "float64"]

-- Extract type definitions from code for enhanced type analysis
extractTypeDefinitions :: String -> [(String, [String], [Dep.TypeConstraint])]
extractTypeDefinitions code =
    let linesOfCode = lines code
        typeLines = filter isTypeDefinitionLine linesOfCode
    in map parseTypeDefinitionLine typeLines
  where
    isTypeDefinitionLine :: String -> Bool
    isTypeDefinitionLine line =
        let trimmed = trim line
        in "type " `isPrefixOf` trimmed && not ("//" `isPrefixOf` trimmed)

    parseTypeDefinitionLine :: String -> (String, [String], [Dep.TypeConstraint])
    parseTypeDefinitionLine line =
        let withoutType = drop 5 (trim line)
            (typeName, rest) = break (`elem` [' ', '<']) withoutType
        in if null typeName
           then ("", [], [])
           else if "<" `isInfixOf` rest
                then parseGenericTypeDefinition typeName rest
                else parseSimpleTypeDefinition typeName rest

    parseGenericTypeDefinition :: String -> String -> (String, [String], [Dep.TypeConstraint])
    parseGenericTypeDefinition typeName rest =
        let paramsPart = takeWhile (/= '>') (drop 1 rest)
            params = map trim (splitByComma paramsPart)
            afterParams = dropWhile (/= '>') rest
            cs = parseWhereConstraints afterParams
        in (typeName, params, cs)

    parseSimpleTypeDefinition :: String -> String -> (String, [String], [Dep.TypeConstraint])
    parseSimpleTypeDefinition typeName rest =
        (typeName, [], parseWhereConstraints rest)

    parseWhereConstraints :: String -> [Dep.TypeConstraint]
    parseWhereConstraints rest =
        let trimmed' = trim rest
        in if "where " `isPrefixOf` trimmed'
           then let constraintStr = drop 6 trimmed'
                in parseSimpleConstraints constraintStr
           else []

    parseSimpleConstraints :: String -> [Dep.TypeConstraint]
    parseSimpleConstraints constraintStr =
        let constraints' = splitByChar '&' constraintStr
        in map parseSingleConstraint constraints'

    parseSingleConstraint :: String -> Dep.TypeConstraint
    parseSingleConstraint constraint =
        let trimmed' = trim constraint
            wordsInConstraint = words trimmed'
        in case wordsInConstraint of
             [var, ">", value] ->
                 case reads value of
                     [(num, "")] -> Dep.TypeSizeGT (Dep.TVVar var) (num + 1)
                     _ -> Dep.Predicate trimmed' [Dep.TVVar var]
             [var, ">=", value] ->
                 case reads value of
                     [(num, "")] -> Dep.TypeSizeGE (Dep.TVVar var) num
                     _ -> Dep.Predicate trimmed' [Dep.TVVar var]
             ["len", var, ">", value] ->
                 case reads value of
                     [(num, "")] -> Dep.TypeSizeGT (Dep.TVVar var) (num + 1)
                     _ -> Dep.Predicate trimmed' [Dep.TVVar var]
             ["nonempty", var] -> Dep.TypeSizeGT (Dep.TVVar var) 0
             _ -> Dep.Predicate trimmed' []

    splitByComma :: String -> [String]
    splitByComma s = case break (== ',') s of
        (a, []) -> [a]
        (a, _:b) -> a : splitByComma b

-- Filter out TypeNotFound errors for known built-in types
filterKnownTypeErrors :: [Dep.DependentTypeError] -> [Dep.DependentTypeError]
filterKnownTypeErrors errors =
    filter isSignificantTypeError errors
  where
    isSignificantTypeError :: Dep.DependentTypeError -> Bool
    isSignificantTypeError (Dep.TypeNotFound typeName) =
        not (typeName `elem` knownTypes)
    isSignificantTypeError _ = True

    knownTypes :: [String]
    knownTypes =
        [ "int", "string", "bool", "float64", "byte", "rune"
        , "error", "interface{}", "[]int", "[]T", "[]string"
        , "Vector", "NonEmptySlice", "map", "chan", "func"
        ]

-- Public API functions
getCombinedErrors :: AnalyzerState -> [CombinedError]
getCombinedErrors state' = combinedErrors state'

getAnalysisSummary :: AnalyzerState -> String
getAnalysisSummary state' =
    let (errorCount, warningCount, infoCount) = countBySeverity (combinedErrors state')
    in unlines
        [ "Analysis Summary:"
        , "================="
        , "Errors: " ++ show errorCount
        , "Warnings: " ++ show warningCount
        , "Info: " ++ show infoCount
        , "Total symbols: " ++ show (Map.size $ symbolTable state')
        ]
  where
    countBySeverity :: [CombinedError] -> (Int, Int, Int)
    countBySeverity = foldr add (0,0,0)
      where
        add ce (e,w,i) =
            case severityOf ce of
              Error   -> (e+1, w, i)
              Warning -> (e, w+1, i)
              Info    -> (e, w, i+1)

    severityOf :: CombinedError -> ErrorSeverity
    severityOf (OwnershipErrorCombined s _) = s
    severityOf (DependentTypeErrorCombined s _) = s
    severityOf (IntegrationError _ s) = s
    severityOf (CrossAnalyzerError _ s _) = s