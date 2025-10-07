{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module IntegratedCompiler (
    compileWithIntegratedAnalyzers,
    IntegratedCompileResult(..),
    CompilerConfig(..),
    defaultCompilerConfig,
    AnalysisResult(..),
    CombinedError(..),
    ErrorSeverity(..),
    formatCompilationResult,
    getDetailedAnalysisSummary,
    showCombinedError
) where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.List (isPrefixOf, isInfixOf, nub)
import Data.Char (isSpace)
import Data.Maybe (catMaybes)
import GHC.Generics (Generic)

-- ============================================================================
-- Core Types and Data Structures
-- ============================================================================

-- Error severity levels
data ErrorSeverity = Info | Warning | Error
    deriving (Show, Eq, Ord, Generic)

-- Ownership error types
data OwnershipError = 
    UseAfterMove String
    | DoubleMove String
    | BorrowAfterMove String
    | InvalidOwnershipTransfer String
    deriving (Show, Eq, Generic)

-- Dependent type error types
data DependentTypeError = 
    TypeMismatch String String
    | RefinementViolation String
    | DependentConstraintFailed String
    | InvalidTypeApplication String
    deriving (Show, Eq, Generic)

-- Combined error type for unified error reporting
data CombinedError = 
    OwnershipErrorCombined ErrorSeverity OwnershipError
    | DependentTypeErrorCombined ErrorSeverity DependentTypeError
    | IntegrationError String ErrorSeverity
    | CrossAnalyzerError String ErrorSeverity (Maybe String)
    deriving (Show, Eq, Generic)

-- Function to convert CombinedError to string for display
showCombinedError :: CombinedError -> String
showCombinedError (OwnershipErrorCombined _ err) = "Ownership error: " ++ show err
showCombinedError (DependentTypeErrorCombined _ err) = "Dependent type error: " ++ show err
showCombinedError (IntegrationError msg _) = "Integration error: " ++ msg
showCombinedError (CrossAnalyzerError msg _ detail) = 
    "Cross-analyzer error: " ++ msg ++ maybe "" (" - " ++) detail

-- Symbol information
data SymbolInfo = SymbolInfo
    { symbolName :: String
    , symbolType :: String
    , symbolLocation :: (Int, Int)  -- (line, column)
    , symbolScope :: String
    } deriving (Show, Eq, Generic)

-- Analysis result from integrated analyzer
data AnalysisResult = AnalysisResult
    { ownershipErrors :: [OwnershipError]
    , dependentTypeErrors :: [DependentTypeError]
    , combinedErrors :: [CombinedError]
    , symbolTable :: Map String SymbolInfo
    , analysisWarnings :: [String]
    , analysisInfo :: [String]
    , crossAnalysisResults :: Map String String
    } deriving (Show, Eq, Generic)

-- Integrated analyzer configuration
data IntegratedAnalyzer = IntegratedAnalyzer
    { analyzeOwnership :: Bool
    , analyzeDependentTypes :: Bool
    , shouldPerformCrossAnalysis :: Bool
    } deriving (Show, Eq, Generic)

-- Compiler configuration
data CompilerConfig = CompilerConfig
    { enableOwnership :: Bool
    , enableDependentTypes :: Bool
    , enableSyntaxValidation :: Bool
    , enableCrossAnalysis :: Bool
    , errorReportingLevel :: ErrorSeverity
    , optimizationLevel :: Int
    , targetPlatform :: String
    } deriving (Show, Eq)

-- Integrated compilation result
data IntegratedCompileResult = IntegratedCompileResult
    { success :: Bool
    , compiledCode :: String
    , analysisResult :: Maybe AnalysisResult
    , syntaxErrors :: [String]
    , filteredErrors :: [CombinedError]
    , compilationWarnings :: [String]
    , compilationInfo :: [String]
    } deriving (Show, Eq)

-- Syntax error type
data SyntaxError = SyntaxError
    { errorMessage :: String
    , errorLine :: Int
    , errorColumn :: Int
    } deriving (Show, Eq, Generic)

-- ============================================================================
-- Default Configurations
-- ============================================================================

-- Default compiler configuration
defaultCompilerConfig :: CompilerConfig
defaultCompilerConfig = CompilerConfig
    { enableOwnership = True
    , enableDependentTypes = True
    , enableSyntaxValidation = True
    , enableCrossAnalysis = True
    , errorReportingLevel = Warning
    , optimizationLevel = 1
    , targetPlatform = "go"
    }

-- Create a new integrated analyzer
newIntegratedAnalyzer :: Bool -> Bool -> IntegratedAnalyzer
newIntegratedAnalyzer ownership depTypes = IntegratedAnalyzer
    { analyzeOwnership = ownership
    , analyzeDependentTypes = depTypes
    , shouldPerformCrossAnalysis = ownership && depTypes
    }

-- ============================================================================
-- Main Compilation Pipeline
-- ============================================================================

-- Main compilation function with integrated analyzers
compileWithIntegratedAnalyzers :: String -> CompilerConfig -> IO IntegratedCompileResult
compileWithIntegratedAnalyzers code config = do
    -- Phase 1: Syntax validation (if enabled)
    syntaxValidationResult <- if enableSyntaxValidation config
        then validateSyntax code
        else return $ Right ()
    
    case syntaxValidationResult of
        Left syntaxErrors -> do
            -- Syntax validation failed
            return $ IntegratedCompileResult
                { success = False
                , compiledCode = ""
                , analysisResult = Nothing
                , syntaxErrors = syntaxErrors
                , filteredErrors = []
                , compilationWarnings = ["Syntax validation failed"]
                , compilationInfo = ["Total syntax errors: " ++ show (length syntaxErrors)]
                }
        
        Right () -> do
            -- Phase 2: Integrated analysis
            analysisResult <- runIntegratedAnalysis code (newIntegratedAnalyzer 
                (enableOwnership config) 
                (enableDependentTypes config))
            
            case analysisResult of
                Left analysisError -> do
                    -- Analysis failed critically
                    return $ IntegratedCompileResult
                        { success = False
                        , compiledCode = ""
                        , analysisResult = Nothing
                        , syntaxErrors = []
                        , filteredErrors = [IntegrationError analysisError Error]
                        , compilationWarnings = []
                        , compilationInfo = ["Analysis phase failed"]
                        }
                
                Right finalResult -> do
                    -- Phase 3: Error filtering and reporting
                    let filteredErrors = filterErrorsByConfig finalResult config
                    let hasSignificantErrors = hasErrorsAboveLevel filteredErrors Error
                    
                    if hasSignificantErrors
                        then do
                            -- Compilation failed due to analysis errors
                            return $ IntegratedCompileResult
                                { success = False
                                , compiledCode = code
                                , analysisResult = Just finalResult
                                , syntaxErrors = []
                                , filteredErrors = filteredErrors
                                , compilationWarnings = analysisWarnings finalResult
                                , compilationInfo = analysisInfo finalResult ++ 
                                    ["Compilation halted due to errors"]
                                }
                        else do
                            -- Phase 4: Code generation (successful compilation)
                            generatedCode <- generateCode code finalResult config
                            return $ IntegratedCompileResult
                                { success = True
                                , compiledCode = generatedCode
                                , analysisResult = Just finalResult
                                , syntaxErrors = []
                                , filteredErrors = filteredErrors
                                , compilationWarnings = analysisWarnings finalResult
                                , compilationInfo = analysisInfo finalResult ++ 
                                    ["Code generation completed successfully"]
                                }

-- ============================================================================
-- Analysis Functions
-- ============================================================================

-- Run integrated analysis on the code
runIntegratedAnalysis :: String -> IntegratedAnalyzer -> IO (Either String AnalysisResult)
runIntegratedAnalysis code analyzer = do
    -- Parse the code into an AST (simplified for now)
    let parseResult = parseCode code
    
    case parseResult of
        Left parseError -> return $ Left $ "Parse error: " ++ parseError
        Right ast -> do
            -- Run ownership analysis
            ownershipErrs <- if analyzeOwnership analyzer
                then performOwnershipAnalysis ast
                else return []
            
            -- Run dependent type analysis
            typeErrs <- if analyzeDependentTypes analyzer
                then performDependentTypeAnalysis ast
                else return []
            
            -- Run cross-analysis if both analyzers are enabled
            (crossErrs, crossResults) <- if shouldPerformCrossAnalysis analyzer
                then performCrossAnalysis ast ownershipErrs typeErrs
                else return ([], Map.empty)
            
            -- Build symbol table
            let symbols = buildSymbolTable ast
            
            -- Combine all errors
            let combinedErrs = 
                    map (OwnershipErrorCombined Error) ownershipErrs ++
                    map (DependentTypeErrorCombined Error) typeErrs ++
                    crossErrs
            
            -- Generate warnings and info
            let warnings = generateWarnings ast
            let info = generateInfo ast (length ownershipErrs) (length typeErrs)
            
            return $ Right $ AnalysisResult
                { ownershipErrors = ownershipErrs
                , dependentTypeErrors = typeErrs
                , combinedErrors = combinedErrs
                , symbolTable = symbols
                , analysisWarnings = warnings
                , analysisInfo = info
                , crossAnalysisResults = crossResults
                }

-- Enhanced AST type with more detail
type AST = [ASTNode]

-- Type alias for ownership state tracking
type OwnershipState = Map String (Bool, Bool)

-- Type alias for type environment
type TypeEnv = Map String String

data ASTNode =
    FunctionNode String [String] [ASTNode]  -- name, params, body
    | VariableNode String String            -- name, type
    | AssignmentNode String String          -- variable, value
    | CallNode String [String]              -- function, args
    | TypeNode String [String]              -- type name, params
    | StructNode String [(String, String)]  -- struct name, fields
    | ReturnNode String                     -- return value
    | IfNode String [ASTNode] [ASTNode]     -- condition, then-branch, else-branch
    | ForNode String [ASTNode]              -- condition, body
    deriving (Show, Eq)

-- Enhanced code parsing with better structure analysis
parseCode :: String -> Either String AST
parseCode code =
    let lines' = lines code
        -- Filter out comments and empty lines
        validLines = filter (not . isCommentOrEmpty) lines'
        nodes = catMaybes $ map parseLine validLines
    in if null nodes && not (null validLines)
       then Left "Unable to parse code: No valid AST nodes found"
       else Right nodes
  where
    isCommentOrEmpty :: String -> Bool
    isCommentOrEmpty line =
        let t = trim line
        in null t || "//" `isPrefixOf` t || "/*" `isPrefixOf` t
    
    parseLine :: String -> Maybe ASTNode
    parseLine line
        | "func " `isPrefixOf` trimmed = parseFunctionDecl trimmed
        | "type " `isPrefixOf` trimmed = parseTypeDecl trimmed
        | "var " `isPrefixOf` trimmed = parseVariableDecl trimmed
        | ":=" `isInfixOf` trimmed = parseAssignment trimmed
        | "return " `isPrefixOf` trimmed = parseReturn trimmed
        | "if " `isPrefixOf` trimmed = parseIf trimmed
        | "for " `isPrefixOf` trimmed = parseFor trimmed
        | "(" `isInfixOf` trimmed && not ("func " `isPrefixOf` trimmed) = parseCall trimmed
        | otherwise = Nothing
      where trimmed = trim line
    
    parseFunctionDecl :: String -> Maybe ASTNode
    parseFunctionDecl line =
        let afterFunc = drop 5 line  -- Remove "func "
            (nameAndParams, _) = break (== '{') afterFunc
            (funcName, paramsStr) = break (== '(') nameAndParams
            params = extractParams paramsStr
        in Just $ FunctionNode (trim funcName) params []
    
    parseTypeDecl :: String -> Maybe ASTNode
    parseTypeDecl line =
        let afterType = drop 5 line  -- Remove "type "
            (typeName, rest) = break (\c -> c == ' ' || c == '{') afterType
        in if "struct" `isInfixOf` rest
           then Just $ StructNode (trim typeName) []
           else Just $ TypeNode (trim typeName) []
    
    parseVariableDecl :: String -> Maybe ASTNode
    parseVariableDecl line =
        let afterVar = drop 4 line  -- Remove "var "
            parts = words afterVar
        in if length parts >= 2
           then case parts of
                  (first:second:_) -> Just $ VariableNode first second
                  (first:_) -> Just $ VariableNode first "unknown"
                  _ -> Nothing
           else Nothing
    
    parseAssignment :: String -> Maybe ASTNode
    parseAssignment line =
        let (varPart, valuePart) = break (== ':') line
            value = drop 2 valuePart  -- Remove ":="
        in Just $ AssignmentNode (trim varPart) (trim value)
    
    parseReturn :: String -> Maybe ASTNode
    parseReturn line =
        let afterReturn = drop 7 line  -- Remove "return "
        in Just $ ReturnNode (trim afterReturn)
    
    parseIf :: String -> Maybe ASTNode
    parseIf line =
        let afterIf = drop 3 line  -- Remove "if "
            (condition, _) = break (== '{') afterIf
        in Just $ IfNode (trim condition) [] []
    
    parseFor :: String -> Maybe ASTNode
    parseFor line =
        let afterFor = drop 4 line  -- Remove "for "
            (condition, _) = break (== '{') afterFor
        in Just $ ForNode (trim condition) []
    
    parseCall :: String -> Maybe ASTNode
    parseCall line =
        let (funcPart, argsPart) = break (== '(') line
            args = extractParams argsPart
        in Just $ CallNode (trim funcPart) args
    
    extractParams :: String -> [String]
    extractParams str =
        let cleaned = filter (`notElem` ("()" :: String)) str
        in if null cleaned then [] else map trim (splitByComma cleaned)
    
    splitByComma :: String -> [String]
    splitByComma s = case break (== ',') s of
        (a, []) -> [a]
        (a, _:b) -> a : splitByComma b

-- Enhanced ownership analysis with move and borrow tracking
performOwnershipAnalysis :: AST -> IO [OwnershipError]
performOwnershipAnalysis ast = do
    let errors = analyzeOwnership' ast Map.empty []
    return errors
  where
    -- Track ownership state: Map of variable name to (isMoved, isBorrowed)
    analyzeOwnership' :: AST -> OwnershipState -> [String] -> [OwnershipError]
    analyzeOwnership' [] _ _ = []
    
    analyzeOwnership' (VariableNode var _ : rest) state scope =
        -- New variable declaration - add to scope as owned
        let newState = Map.insert var (False, False) state
        in analyzeOwnership' rest newState (var : scope)
    
    analyzeOwnership' (AssignmentNode var val : rest) state scope =
        let errors = case Map.lookup var state of
              Just (True, _) ->
                  -- Variable was already moved
                  [UseAfterMove var]
              Just (False, True) ->
                  -- Variable is borrowed, check if assignment moves it
                  if isMove val
                  then [BorrowAfterMove var]
                  else []
              _ -> []
            -- Update state if this is a move
            newState = if isMove val
                       then Map.adjust (\(_, b) -> (True, b)) var state
                       else state
        in errors ++ analyzeOwnership' rest newState scope
    
    analyzeOwnership' (CallNode _ args : rest) state scope =
        -- Check if any arguments are moved or borrowed incorrectly
        let errors = concatMap (checkArgument state) args
        in errors ++ analyzeOwnership' rest state scope
    
    analyzeOwnership' (FunctionNode _ _ body : rest) state scope =
        -- Analyze function body with current scope
        let bodyErrors = analyzeOwnership' body state scope
        in bodyErrors ++ analyzeOwnership' rest state scope
    
    analyzeOwnership' (IfNode _ thenBranch elseBranch : rest) state scope =
        let thenErrors = analyzeOwnership' thenBranch state scope
            elseErrors = analyzeOwnership' elseBranch state scope
        in thenErrors ++ elseErrors ++ analyzeOwnership' rest state scope
    
    analyzeOwnership' (ForNode _ body : rest) state scope =
        let bodyErrors = analyzeOwnership' body state scope
        in bodyErrors ++ analyzeOwnership' rest state scope
    
    analyzeOwnership' (_ : rest) state scope =
        analyzeOwnership' rest state scope
    
    isMove :: String -> Bool
    isMove val = not ("&" `isPrefixOf` val) && not (null val)
    
    checkArgument :: OwnershipState -> String -> [OwnershipError]
    checkArgument state arg =
        case Map.lookup arg state of
          Just (True, _) -> [UseAfterMove arg]
          _ -> []

-- Enhanced dependent type analysis with constraint checking
performDependentTypeAnalysis :: AST -> IO [DependentTypeError]
performDependentTypeAnalysis ast = do
    let errors = analyzeTypes' ast Map.empty
    return errors
  where
    analyzeTypes' :: AST -> TypeEnv -> [DependentTypeError]
    analyzeTypes' [] _ = []
    
    analyzeTypes' (VariableNode name typ : rest) env =
        let errors = if typ == "unknown" || null typ
                     then [TypeMismatch name "explicit type required"]
                     else []
            newEnv = Map.insert name typ env
        in errors ++ analyzeTypes' rest newEnv
    
    analyzeTypes' (AssignmentNode var val : rest) env =
        case Map.lookup var env of
          Just expectedType ->
              let actualType = inferValueType val
                  errors = if not (typesCompatible expectedType actualType)
                          then [TypeMismatch var expectedType]
                          else []
              in errors ++ analyzeTypes' rest env
          Nothing ->
              -- Variable not declared
              [TypeMismatch var "undeclared variable"] ++ analyzeTypes' rest env
    
    analyzeTypes' (FunctionNode _name params body : rest) env =
        -- Add function parameters to environment
        let paramEnv = foldl (\e p -> Map.insert p "param" e) env params
            bodyErrors = analyzeTypes' body paramEnv
        in bodyErrors ++ analyzeTypes' rest env
    
    analyzeTypes' (CallNode _funcName args : rest) env =
        -- Check if arguments have valid types
        let argErrors = concatMap (checkArgType env) args
        in argErrors ++ analyzeTypes' rest env
    
    analyzeTypes' (TypeNode name _params : rest) env =
        -- Register type definition
        let newEnv = Map.insert name "type" env
        in analyzeTypes' rest newEnv
    
    analyzeTypes' (ReturnNode val : rest) env =
        let returnType = inferValueType val
            errors = if returnType == "unknown"
                    then [TypeMismatch "return" "cannot infer type"]
                    else []
        in errors ++ analyzeTypes' rest env
    
    analyzeTypes' (_ : rest) env =
        analyzeTypes' rest env
    
    inferValueType :: String -> String
    inferValueType val
        | null val = "unknown"
        | not (null val) && case val of (c:_) -> c == '"'; _ -> False = "string"
        | val `elem` ["true", "false"] = "bool"
        | all (`elem` ("0123456789" :: String)) val = "int"
        | any (== '.') val = "float64"
        | otherwise = "unknown"
    
    typesCompatible :: String -> String -> Bool
    typesCompatible _ "unknown" = True
    typesCompatible t1 t2 = t1 == t2 || t1 == "interface{}"
    
    checkArgType :: TypeEnv -> String -> [DependentTypeError]
    checkArgType env arg =
        if not (Map.member arg env) && not (isLiteral arg)
        then [TypeMismatch arg "argument type unknown"]
        else []
    
    isLiteral :: String -> Bool
    isLiteral s = not (null s) && ((not (null s) && case s of (c:_) -> c `elem` ("\"'0123456789" :: String); _ -> False) || s `elem` ["true", "false", "nil"])

-- Perform cross-analysis
performCrossAnalysis :: AST -> [OwnershipError] -> [DependentTypeError] -> 
                       IO ([CombinedError], Map String String)
performCrossAnalysis _ast ownershipErrs typeErrs = do
    let crossErrors = if not (null ownershipErrs) && not (null typeErrs)
                      then [CrossAnalyzerError "Ownership and type errors detected together" Error Nothing]
                      else []
    let results :: Map String String
        results = Map.fromList [("cross_analysis", "completed")]
    return (crossErrors, results)

-- Build symbol table from AST
buildSymbolTable :: AST -> Map String SymbolInfo
buildSymbolTable ast = 
    Map.fromList $ concatMap extractSymbols ast
  where
    extractSymbols :: ASTNode -> [(String, SymbolInfo)]
    extractSymbols (FunctionNode name _params _) =
        [(name, SymbolInfo name "function" (0, 0) "global")]
    extractSymbols (VariableNode name typ) = 
        [(name, SymbolInfo name typ (0, 0) "local")]
    extractSymbols _ = []

-- Generate warnings from AST
generateWarnings :: AST -> [String]
generateWarnings ast = 
    if length ast > 100
    then ["Large code size detected, consider splitting into modules"]
    else []

-- Generate info messages
generateInfo :: AST -> Int -> Int -> [String]
generateInfo ast ownershipCount typeCount =
    [ "Analyzed " ++ show (length ast) ++ " AST nodes"
    , "Found " ++ show ownershipCount ++ " ownership issues"
    , "Found " ++ show typeCount ++ " type issues"
    ]

-- ============================================================================
-- Syntax Validation
-- ============================================================================

-- Syntax validation function
validateSyntax :: String -> IO (Either [String] ())
validateSyntax code = do
    let syntaxErrors = validateAllSyntax code
    if null syntaxErrors
        then return $ Right ()
        else return $ Left syntaxErrors
  where
    validateAllSyntax :: String -> [String]
    validateAllSyntax _code' =
        let basicErrors = validateBasicSyntax code
            goErrors = validateGoSyntax code
            typusErrors = validateTypusSyntax code
            allErrors = basicErrors ++ goErrors ++ typusErrors
        in nub allErrors  -- Using standard library nub

-- Basic syntax validation
validateBasicSyntax :: String -> [String]
validateBasicSyntax code =
    let lines' = lines code
        errors = concatMap (uncurry validateLine) (zip [1..] lines')
    in errors
  where
    validateLine :: Int -> String -> [String]
    validateLine lineNum line =
        checkBalancedParens line lineNum ++
        checkBalancedBraces line lineNum
    
    checkBalancedParens s n = 
        if countChar '(' s /= countChar ')' s
        then ["Line " ++ show n ++ ": Unbalanced parentheses"]
        else []
    
    checkBalancedBraces s n = 
        if countChar '{' s /= countChar '}' s
        then ["Line " ++ show n ++ ": Unbalanced braces"]
        else []
    
    countChar c = length . filter (== c)

-- Go-specific syntax validation
validateGoSyntax :: String -> [String]
validateGoSyntax code =
    let lines' = lines code
        hasPackage = any ("package " `isPrefixOf`) lines'
    in if not hasPackage && not (null lines')
       then ["Missing package declaration"]
       else []

-- Typus-specific syntax validation
validateTypusSyntax :: String -> [String]
validateTypusSyntax _code =
    -- Placeholder for Typus-specific validation
    []

-- ============================================================================
-- Error Filtering and Reporting
-- ============================================================================

-- Get severity of an error (unified function)
getErrorSeverity :: CombinedError -> ErrorSeverity
getErrorSeverity (OwnershipErrorCombined severity _) = severity
getErrorSeverity (DependentTypeErrorCombined severity _) = severity
getErrorSeverity (IntegrationError _ severity) = severity
getErrorSeverity (CrossAnalyzerError _ severity _) = severity

-- Filter errors based on compiler configuration (FIXED)
filterErrorsByConfig :: AnalysisResult -> CompilerConfig -> [CombinedError]
filterErrorsByConfig result config = 
    let allErrors = combinedErrors result  -- FIXED: Now using actual errors
        minSeverity = errorReportingLevel config
    in filter (isErrorAboveSeverity minSeverity) allErrors
  where
    isErrorAboveSeverity :: ErrorSeverity -> CombinedError -> Bool
    isErrorAboveSeverity minSeverity err = 
        getErrorSeverity err >= minSeverity

-- Check if there are errors above the specified level
hasErrorsAboveLevel :: [CombinedError] -> ErrorSeverity -> Bool
hasErrorsAboveLevel errors level = 
    any (\err -> getErrorSeverity err >= level) errors

-- ============================================================================
-- Code Generation
-- ============================================================================

-- Generate final code with analysis results
generateCode :: String -> AnalysisResult -> CompilerConfig -> IO String
generateCode originalCode result config = do
    -- Generate enhanced Go code with proper structure
    let analysisComments = generateAnalysisComments result config
    let processedCode = processCodeForGeneration originalCode result config
    let finalCode = processedCode ++ "\n\n" ++ analysisComments
    return finalCode

-- Process code for generation
processCodeForGeneration :: String -> AnalysisResult -> CompilerConfig -> String
processCodeForGeneration code _result _config =
    let lines' = lines code
        -- Ensure basic Go structure
        withPackage = ensurePackageDeclaration lines'
        withImports = ensureImports withPackage
        withMain = ensureMainFunction withImports
    in unlines withMain

-- Ensure package declaration exists
ensurePackageDeclaration :: [String] -> [String]
ensurePackageDeclaration lines' =
    if any ("package " `isPrefixOf`) lines'
    then lines'
    else "package main" : lines'

-- Ensure necessary imports
ensureImports :: [String] -> [String]
ensureImports lines' =
    let (pkgLines, rest) = span ("package " `isPrefixOf`) lines'
        hasImport = any (\line -> "import " `isPrefixOf` line || "import(" `isPrefixOf` line) rest
    in if hasImport
       then lines'
       else pkgLines ++ ["", "import \"fmt\""] ++ rest

-- Ensure main function exists
ensureMainFunction :: [String] -> [String]
ensureMainFunction lines' =
    if any ("func main()" `isInfixOf`) lines'
    then lines'
    else lines' ++ ["", "func main() {", "    fmt.Println(\"Generated by Typus Compiler\")", "}"]

-- Generate analysis comments
generateAnalysisComments :: AnalysisResult -> CompilerConfig -> String
generateAnalysisComments result config = 
    let commentLines = 
            [ "// ===== Integrated Analysis Results ====="
            , "// Ownership Analysis: " ++ if enableOwnership config then "ENABLED" else "DISABLED"
            , "// Dependent Types: " ++ if enableDependentTypes config then "ENABLED" else "DISABLED"
            , "// Cross Analysis: " ++ if enableCrossAnalysis config then "ENABLED" else "DISABLED"
            , "// Optimization Level: " ++ show (optimizationLevel config)
            , "// Target Platform: " ++ targetPlatform config
            , ""
            ] ++ generateErrorComments result config ++
            [ "// ===== End Analysis Results ====="
            ]
    in unlines commentLines

-- Generate error comments
generateErrorComments :: AnalysisResult -> CompilerConfig -> [String]
generateErrorComments result config = 
    let filteredErrors = filterErrorsByConfig result config
        errorCount = length $ filter (\e -> getErrorSeverity e == Error) filteredErrors
        warningCount = length $ filter (\e -> getErrorSeverity e == Warning) filteredErrors
        infoCount = length $ filter (\e -> getErrorSeverity e == Info) filteredErrors
    in [ "// Errors: " ++ show errorCount
       , "// Warnings: " ++ show warningCount
       , "// Info: " ++ show infoCount
       ]

-- ============================================================================
-- Formatting and Display Functions
-- ============================================================================

-- Format compilation result for display
formatCompilationResult :: IntegratedCompileResult -> String
formatCompilationResult result = 
    let statusLine :: String
        statusLine = if success result 
                    then "✅ Compilation Successful" 
                    else "❌ Compilation Failed"
        
        syntaxErrorLines = if not (null $ syntaxErrors result)
                          then "\n📝 Syntax Errors:\n" ++ unlines (map ("  • " ++) (syntaxErrors result))
                          else ""
        
        errorLines = if not (null $ filteredErrors result)
                    then "\n⚠️ Analysis Errors:\n" ++ unlines (map formatError (filteredErrors result))
                    else ""
        
        warningLines = if not (null $ compilationWarnings result)
                      then "\n⚡ Warnings:\n" ++ unlines (map ("  • " ++) (compilationWarnings result))
                      else ""
        
        infoLines = if not (null $ compilationInfo result)
                   then "\nℹ️ Info:\n" ++ unlines (map ("  • " ++) (compilationInfo result))
                   else ""
    
    in unlines [statusLine, syntaxErrorLines, errorLines, warningLines, infoLines]
  where
    formatError :: CombinedError -> String
    formatError err = "  • [" ++ showSeverity (getErrorSeverity err) ++ "] " ++ formatErrorDetails err
    
    formatErrorDetails :: CombinedError -> String
    formatErrorDetails (OwnershipErrorCombined _ err) = "Ownership: " ++ show err
    formatErrorDetails (DependentTypeErrorCombined _ err) = "Type: " ++ show err
    formatErrorDetails (IntegrationError msg _) = "Integration: " ++ msg
    formatErrorDetails (CrossAnalyzerError msg _ detail) = 
        "Cross-Analysis: " ++ msg ++ maybe "" (" - " ++) detail
    
    showSeverity :: ErrorSeverity -> String
    showSeverity Error = "ERROR"
    showSeverity Warning = "WARN"
    showSeverity Info = "INFO"

-- Get detailed analysis summary (FIXED)
getDetailedAnalysisSummary :: AnalysisResult -> String
getDetailedAnalysisSummary result = 
    let ownershipErrorCount = length $ ownershipErrors result
        typeErrorCount = length $ dependentTypeErrors result
        combinedErrorCount = length $ combinedErrors result  -- FIXED: Now using actual count
        symbolCount = Map.size $ symbolTable result  -- FIXED: Now using actual count
        crossAnalysisCount = Map.size $ crossAnalysisResults result
    in unlines $
        [ "╔══════════════════════════════════════╗"
        , "║     Detailed Analysis Summary        ║"
        , "╚══════════════════════════════════════╝"
        , ""
        , "📊 Error Statistics:"
        , "  • Ownership Errors: " ++ show ownershipErrorCount
        , "  • Type Errors: " ++ show typeErrorCount
        , "  • Combined Issues: " ++ show combinedErrorCount
        , ""
        , "🔍 Analysis Coverage:"
        , "  • Symbols Analyzed: " ++ show symbolCount
        , "  • Cross-Analysis Results: " ++ show crossAnalysisCount
        , ""
        , "📈 Analysis Status:"
        ] ++
        (if ownershipErrorCount > 0 
         then ["  ❌ Ownership Analysis: Issues detected"] 
         else ["  ✅ Ownership Analysis: Clean"]) ++
        (if typeErrorCount > 0 
         then ["  ❌ Type Analysis: Issues detected"] 
         else ["  ✅ Type Analysis: Clean"]) ++
        (if combinedErrorCount > 0 
         then ["  ❌ Cross-Analysis: Integration issues"] 
         else ["  ✅ Cross-Analysis: Clean"]) ++
        ["", "════════════════════════════════════════"]

-- ============================================================================
-- Utility Functions
-- ============================================================================

-- Trim whitespace from string
trim :: String -> String
trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse

-- ============================================================================
-- Example Usage and Testing
-- ============================================================================

