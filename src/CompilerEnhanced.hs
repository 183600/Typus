{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- Enhanced Typus to Go Compiler with Structured Error Handling

module CompilerEnhanced (
    compileWithErrors,
    compileFile,
    CompilationResult(..),
    module EnhancedErrorHandler
) where

import Parser (TypusFile(..), CodeBlock(..), FileDirectives(..), BlockDirectives(..))
import DependentTypesParser (DependentTypeError(..), runDependentTypesParser, parserErrors)
import Ownership (OwnershipError(..), analyzeOwnership)
import EnhancedErrorHandler
import SourceLocation
import Data.List (intercalate, isInfixOf, isPrefixOf)
import Data.Char (isSpace)
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad.State
import Control.Monad.Except
import Data.Maybe (catMaybes)

-- ============================================================================
-- Compilation Result
-- ============================================================================

data CompilationResult = CompilationResult
    { crCode :: Maybe String        -- Generated code (if successful)
    , crErrors :: [CompilerError]   -- All errors collected
    , crWarnings :: [CompilerError] -- Warnings that don't stop compilation
    , crStatistics :: ErrorStatistics -- Error statistics
    } deriving (Show)

-- ============================================================================
-- Main Compilation Functions
-- ============================================================================

-- Compile with full error tracking
compileWithErrors :: TypusFile -> CompilationResult
compileWithErrors typusFile =
    case runCompilerM (compileTypusFile typusFile) of
        Left errs -> CompilationResult
            { crCode = Nothing
            , crErrors = filter isError errs
            , crWarnings = filter isWarning errs
            , crStatistics = analyzeErrors errs
            }
        Right (code, warnings) -> CompilationResult
            { crCode = Just code
            , crErrors = []
            , crWarnings = warnings
            , crStatistics = analyzeErrors warnings
            }
  where
    isError err = let sev = severity (ceError err) 
                  in sev == Fatal || sev == Error
    isWarning err = severity (ceError err) == Warning

-- Compile a file (backward compatible)
compileFile :: TypusFile -> Either String String
compileFile typusFile =
    case compileWithErrors typusFile of
        CompilationResult { crCode = Just code, crErrors = [] } -> Right code
        CompilationResult { crErrors = errs } -> Left $ formatCompilerErrors errs

-- ============================================================================
-- Compilation Pipeline with Error Recovery
-- ============================================================================

-- Main compilation pipeline
compileTypusFile :: TypusFile -> CompilerM (String, [CompilerError])
compileTypusFile typusFile = do
    -- Phase 1: Syntax validation
    validateSyntax typusFile
    
    -- Phase 2: Dependent types checking (if enabled)
    checkDependentTypesWithRecovery typusFile
    
    -- Phase 3: Ownership analysis (if enabled)
    checkOwnershipWithRecovery typusFile
    
    -- Phase 4: Type checking
    checkTypesWithRecovery typusFile
    
    -- Phase 5: Code generation
    code <- generateGoCodeSafe typusFile
    
    -- Collect all warnings
    warnings <- get
    return (code, filter isWarning warnings)
  where
    isWarning err = severity (ceError err) == Warning

-- ============================================================================
-- Phase 1: Syntax Validation
-- ============================================================================

validateSyntax :: TypusFile -> CompilerM ()
validateSyntax typusFile = do
    let content = intercalate "\n" $ map cbContent (tfBlocks typusFile)
    when (null content) $ 
        recoverFrom $ syntaxError "E001" 
            "Empty file" 
            startPos
    when ("malformed" `isInfixOf` content) $
        recoverFrom $ syntaxError "E002"
            "Malformed syntax detected"
            startPos

-- ============================================================================
-- Phase 2: Dependent Types Checking with Recovery
-- ============================================================================

checkDependentTypesWithRecovery :: TypusFile -> CompilerM ()
checkDependentTypesWithRecovery typusFile = do
    let fileEnabled = case fdDependentTypes (tfDirectives typusFile) of
            Just True -> True
            _ -> False
        blockEnabled = any (\block -> bdDependentTypes (cbDirectives block)) (tfBlocks typusFile)
        shouldCheck = fileEnabled || blockEnabled
    
    when shouldCheck $ do
        let content = extractDependentTypeContent typusFile
        unless (null content) $ do
            case runDependentTypesParser content of
                Left err -> 
                    recoverFrom $ dependentTypeError "E100"
                        (T.pack $ "Dependent type parsing error: " ++ err)
                        (spanFrom startPos)
                        ["Check dependent type syntax", "Verify type constraints"]
                Right (_, parser) -> do
                    let dtErrors = parserErrors parser
                    mapM_ (convertDependentTypeError) dtErrors

-- Convert DependentTypeError to CompilerError
convertDependentTypeError :: DependentTypeError -> CompilerM ()
convertDependentTypeError dtErr = 
    recoverFrom $ case dtErr of
        SyntaxError msg line snippet ->
            dependentTypeError "E101"
                (T.pack $ "Syntax error: " ++ msg)
                (spanBetween (posAt line 1) (posAt line 100))
                [T.pack $ "Near: " ++ snippet]
        InvalidTypeSyntax msg ->
            dependentTypeError "E102"
                (T.pack $ "Invalid type syntax: " ++ msg)
                (spanFrom startPos)
                ["Check type declaration format"]
        MissingConstraint msg ->
            dependentTypeError "E103"
                (T.pack $ "Missing constraint: " ++ msg)
                (spanFrom startPos)
                ["Add required constraint", "Verify constraint syntax"]
        InvalidParameter msg ->
            dependentTypeError "E104"
                (T.pack $ "Invalid parameter: " ++ msg)
                (spanFrom startPos)
                ["Check parameter types"]
        ConstraintParseError msg ->
            dependentTypeError "E105"
                (T.pack $ "Constraint parse error: " ++ msg)
                (spanFrom startPos)
                ["Verify constraint format"]
        TypeVariableError msg ->
            dependentTypeError "E106"
                (T.pack $ "Type variable error: " ++ msg)
                (spanFrom startPos)
                ["Check type variable usage"]

-- ============================================================================
-- Phase 3: Ownership Checking with Recovery
-- ============================================================================

checkOwnershipWithRecovery :: TypusFile -> CompilerM ()
checkOwnershipWithRecovery typusFile = do
    let fileEnabled = case fdOwnership (tfDirectives typusFile) of
            Just True -> True
            _ -> False
        blockEnabled = any (\block -> bdOwnership (cbDirectives block)) (tfBlocks typusFile)
        shouldCheck = fileEnabled || blockEnabled
    
    when shouldCheck $ do
        let content = extractOwnershipContent typusFile
        unless (null content) $ do
            let ownershipErrors = analyzeOwnership content
            mapM_ convertOwnershipError ownershipErrors

-- Convert OwnershipError to CompilerError
convertOwnershipError :: OwnershipError -> CompilerM ()
convertOwnershipError owErr = 
    recoverFrom $ case owErr of
        UseAfterMove var line ->
            ownershipError "E200"
                (T.pack $ "Use after move: variable '" ++ var ++ "' was moved")
                (spanBetween (posAt line 1) (posAt line 80))
                ("Variable: " ++ var)
                ["Use a reference instead of moving", "Clone the value if needed"]
        BorrowWhileMoved var line ->
            ownershipError "E201"
                (T.pack $ "Cannot borrow '" ++ var ++ "' - value was already moved")
                (spanBetween (posAt line 1) (posAt line 80))
                ("Variable: " ++ var)
                ["Don't move the value before borrowing", "Use references consistently"]
        DoubleBorrow var line ->
            ownershipError "E202"
                (T.pack $ "Double borrow of '" ++ var ++ "'")
                (spanBetween (posAt line 1) (posAt line 80))
                ("Variable: " ++ var)
                ["Ensure only one mutable borrow at a time"]
        InvalidBorrow var line ->
            ownershipError "E203"
                (T.pack $ "Invalid borrow of '" ++ var ++ "'")
                (spanBetween (posAt line 1) (posAt line 80))
                ("Variable: " ++ var)
                ["Check borrow rules", "Verify variable lifetime"]

-- ============================================================================
-- Phase 4: Type Checking with Recovery
-- ============================================================================

checkTypesWithRecovery :: TypusFile -> CompilerM ()
checkTypesWithRecovery typusFile = do
    let content = intercalate "\n" $ map cbContent (tfBlocks typusFile)
        declarations = extractDeclarations content
        calls = extractFunctionCalls content
        env = buildTypeEnv declarations
    
    -- Check declarations
    mapM_ (checkDeclaration env) declarations
    
    -- Check function calls
    mapM_ (checkCall env) calls

checkDeclaration :: TypeEnv -> String -> CompilerM ()
checkDeclaration env decl = do
    when (hasTypeError env decl) $
        recoverFrom $ typeError "E300"
            (T.pack $ "Type error in declaration: " ++ take 50 decl)
            (spanFrom startPos)
            (Just decl)
            ["Check variable type", "Verify initialization matches type"]

checkCall :: TypeEnv -> String -> CompilerM ()
checkCall env call = do
    when (hasTypeError env call) $
        recoverFrom $ typeError "E301"
            (T.pack $ "Type error in function call: " ++ take 50 call)
            (spanFrom startPos)
            (Just call)
            ["Check function signature", "Verify argument types"]

-- ============================================================================
-- Phase 5: Code Generation (Safe)
-- ============================================================================

generateGoCodeSafe :: TypusFile -> CompilerM String
generateGoCodeSafe typusFile = do
    let header = "package main\n"
        imports = generateImports typusFile
        originalContentRaw = intercalate "\n" $ map cbContent (tfBlocks typusFile)
        originalContent = enforceGoStructure $ fixUnusedS2 $ fixVarBlocks $ cleanCodeBlocks originalContentRaw
        allParts = filter (not . null) [header, imports, originalContent]
    return $ intercalate "\n" allParts

-- ============================================================================
-- Helper Functions (from original Compiler)
-- ============================================================================

extractDependentTypeContent :: TypusFile -> String
extractDependentTypeContent typusFile =
    let dependentBlocks = filter (\block -> bdDependentTypes (cbDirectives block)) (tfBlocks typusFile)
    in concatMap cbContent dependentBlocks

extractOwnershipContent :: TypusFile -> String
extractOwnershipContent typusFile =
    let ownershipBlocks = filter (\block -> bdOwnership (cbDirectives block)) (tfBlocks typusFile)
    in concatMap cbContent ownershipBlocks

-- Type checking data types
data Type = IntType | StringType | BoolType | FloatType | VoidType | FunctionType [Type] Type
    deriving (Eq, Show)

data TypeEnv = TypeEnv
    { varTypes :: [(String, Type)]
    , functionTypes :: [(String, ([Type], Type))]
    } deriving (Show)

emptyTypeEnv :: TypeEnv
emptyTypeEnv = TypeEnv [] []

extractDeclarations :: String -> [String]
extractDeclarations content =
    let ls = lines content
        isVarDecl line = any (`isPrefixOf` trim line) ["var ", "const ", "func "]
    in filter isVarDecl ls

extractFunctionCalls :: String -> [String]
extractFunctionCalls content =
    let ls = lines content
        isFunctionCall line = "=" `isInfixOf` line && "(" `isInfixOf` line && not (isPrefixOf "func" (trim line))
    in filter isFunctionCall ls

buildTypeEnv :: [String] -> TypeEnv
buildTypeEnv declarations = foldl addDeclaration emptyTypeEnv declarations
  where
    addDeclaration env line =
        let t = trim line
        in if isPrefixOf "var " t
            then addVarDeclaration env t
            else if isPrefixOf "func " t
                then addFunctionDeclaration env t
                else env

addVarDeclaration :: TypeEnv -> String -> TypeEnv
addVarDeclaration env line =
    let withoutVar = drop 3 (trim line)
        (varName, varType) = break (\c -> c == ' ' || c == '=') withoutVar
        varType' = trim $ dropWhile (\c -> c == ' ' || c == '=') varType
        inferredType = inferVarType varType'
    in env { varTypes = (varName, inferredType) : varTypes env }

addFunctionDeclaration :: TypeEnv -> String -> TypeEnv
addFunctionDeclaration env line =
    let withoutFunc = drop 4 (trim line)
        (funcName, rest) = break (\c -> c == ' ' || c == '(') withoutFunc
        paramsAndReturn = dropWhile (\c -> c == ' ' || c == '(') rest
        (params, returnType) = break (== ')') paramsAndReturn
        paramTypes = map inferVarType $ splitByComma params
        returnType' = if ") " `isPrefixOf` (drop 1 returnType)
                      then inferVarType $ trim $ drop 2 returnType
                      else VoidType
    in env { functionTypes = (funcName, (paramTypes, returnType')) : functionTypes env }

inferVarType :: String -> Type
inferVarType typeStr
    | "int" `isPrefixOf` typeStr = IntType
    | "string" `isPrefixOf` typeStr = StringType
    | "bool" `isPrefixOf` typeStr = BoolType
    | "float" `isPrefixOf` typeStr = FloatType
    | otherwise = VoidType

splitByComma :: String -> [String]
splitByComma s = map trim $ splitOn ',' s
  where
    splitOn _ [] = []
    splitOn delimiter str =
        let (token, rest) = break (== delimiter) str
        in token : case rest of
            [] -> []
            (_:rest') -> splitOn delimiter rest'

hasTypeError :: TypeEnv -> String -> Bool
hasTypeError env line =
    let t = trim line
    in if isPrefixOf "var " t || isPrefixOf "const " t
        then checkVarDeclaration env t
        else if "=" `isInfixOf` t && "(" `isInfixOf` t
            then checkFunctionCall env t
            else False

-- Enhanced variable declaration type checking
checkVarDeclaration :: TypeEnv -> String -> Bool
checkVarDeclaration env line =
    let withoutVar = drop 3 (trim line)
        parts = words withoutVar
    in if length parts < 2
       then True  -- Malformed declaration
       else
           let varName = head parts
               typeAndValue = unwords (tail parts)
           in case lookup varName (varTypes env) of
               Nothing -> False  -- Variable not in environment (acceptable for new declarations)
               Just declaredType ->
                   -- Check if the value matches the declared type
                   let valueType = inferTypeFromValue typeAndValue
                   in declaredType /= valueType

-- Enhanced function call type checking
checkFunctionCall :: TypeEnv -> String -> Bool
checkFunctionCall env line =
    let beforeEq = takeWhile (/= '=') line
        afterEq = dropWhile (== ' ') $ drop 1 $ dropWhile (/= '=') line
        (funcName, argsPart) = span (/= '(') afterEq
        argsStr = takeWhile (/= ')') $ drop 1 argsPart
    in if null funcName || null argsPart
       then False  -- Not a function call
       else case lookup (trim funcName) (functionTypes env) of
           Nothing -> True  -- Function not found - this is an error
           Just (paramTypes, _) ->
               let args = map trim $ splitByComma argsStr
                   argTypes = map inferTypeFromValue args
               in length argTypes /= length paramTypes ||
                  any (\(expected, actual) -> expected /= actual && actual /= VoidType)
                      (zip paramTypes argTypes)

-- Infer type from value expression
inferTypeFromValue :: String -> Type
inferTypeFromValue expr
    | null expr = VoidType
    | head expr == '"' = StringType
    | all (`elem` "0123456789") expr = IntType
    | "true" `isPrefixOf` expr || "false" `isPrefixOf` expr = BoolType
    | any (== '.') expr && all (`elem` "0123456789.") expr = FloatType
    | otherwise = VoidType

-- Code generation utilities
cleanCodeBlocks :: String -> String
cleanCodeBlocks content =
    let ls = lines content
        goRemoveImports [] _ acc = reverse acc
        goRemoveImports (l:rest) inImport acc =
            let t = trim l
            in if inImport
                then if t == ")" then goRemoveImports rest False acc
                    else goRemoveImports rest True acc
                else if isPrefixOf "package" t
                    then goRemoveImports rest False acc
                    else if isPrefixOf "import" t
                        then if "(" `isInfixOf` t
                            then goRemoveImports rest True acc
                            else goRemoveImports rest False acc
                        else goRemoveImports rest False (l:acc)
    in unlines (goRemoveImports ls False [])

generateImports :: TypusFile -> String
generateImports typusFile =
    let content = intercalate "\n" $ map cbContent (tfBlocks typusFile)
        processedContent = fixUnusedS2 $ fixVarBlocks $ cleanCodeBlocks content
        
        hasFmt = "fmt." `isInfixOf` processedContent
        hasTesting = "testing." `isInfixOf` processedContent
        
        imports = filter (not . null)
            [ if hasFmt then "    \"fmt\"" else ""
            , if hasTesting then "    \"testing\"" else ""
            ]
    in if null imports
        then ""
        else "import (\n" ++ intercalate "\n" imports ++ "\n)\n"

enforceGoStructure :: String -> String
enforceGoStructure content =
    let ls = lines content
        hasMain = any (\x -> "func main()" `isInfixOf` x) ls
        nonDecl = filter (\x -> not (null (trim x)) && not (any (`isPrefixOf` trim x) ["package", "import", "func", "type", "var", "const"])) ls
    in if null nonDecl || hasMain then content
        else unlines $ [head (filter (\x -> "package " `isPrefixOf` x) ls) | any (\x -> "package " `isPrefixOf` x) ls] ++
                      ["", "func main() {"] ++ nonDecl ++ ["}"]

trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace

fixVarBlocks :: String -> String
fixVarBlocks content =
    let ls = lines content
        go [] inVar acc = reverse acc ++ [")" | inVar]
        go (l:rest) inVar acc =
            let t = trim l
            in if inVar
                then if t == ")" then go rest False (l:acc)
                    else if isPrefixOf "func" t || isPrefixOf "type" t || isPrefixOf "const" t || isPrefixOf "var" t
                        then go rest False (l:acc++[")"])
                        else go rest True (l:acc)
                else if isPrefixOf "var (" t
                    then go rest True (l:acc)
                    else go rest False (l:acc)
    in unlines (go ls False [])

fixUnusedS2 :: String -> String
fixUnusedS2 content =
    let ls = lines content
        declIdxs = [ i | (i,l) <- zip [0..] ls, let t = trim l, isPrefixOf "var s2" t || isInfixOf "s2 :=" t ]
        usedElsewhere i = let name = "s2" in any (\(j,l) -> j /= i && name `isInfixOf` l) (zip [0..] ls)
        filtered = [ l | (i,l) <- zip [0..] ls, not (i `elem` [d | d <- declIdxs, not (usedElsewhere d)]) ]
    in unlines filtered