module Compiler
  ( compile
  , CompilationError(..)
  , ErrorKind(..)
  , SourceLocation(..)
  , renderCompilationError
  , hasTypeErrors
  , extractDeclarations
  , extractFunctionCalls
  , buildTypeEnv
  , isMethodDeclaration
  , checkTypeError
  , hasMalformedSyntax
  , checkDependentTypes
  , checkOwnership
  , generateGoCode
  ) where

import Parser (TypusFile(..), CodeBlock(..), FileDirectives(..), BlockDirectives(..))
import DependentTypesParser (DependentTypeError(..), runDependentTypesParser, parserErrors)
import Ownership (analyzeOwnership, formatOwnershipErrors, OwnershipError(..))
import Compiler.GoAst
import qualified Compiler.IR as IR
import Compiler.Error

import Data.Char (isSpace)
import Data.List (intercalate, isInfixOf, isPrefixOf)
import Data.Maybe (catMaybes)

--------------------------------------------------------------------------------
-- Public API
--------------------------------------------------------------------------------

compile :: TypusFile -> Either CompilationError String
compile typusFile = do
  sourceIR <- ensureSourceIR typusFile
  let parsedFile = IR.sourceTypusFile sourceIR
  checkDependentTypes parsedFile
  ensureNoTypeErrors parsedFile
  checkOwnership parsedFile
  semanticIR <- IR.buildSemanticIR sourceIR
  let goArtifact = IR.emitGo semanticIR
  pure (IR.goSource goArtifact)
  where
    ensureNoTypeErrors file =
      if hasTypeErrors file
        then Left $ mkCompilationError TypeErrorKind "Type errors detected" []
        else Right ()

ensureSourceIR :: TypusFile -> Either CompilationError IR.SourceIR
ensureSourceIR typusFile =
  if hasMalformedSyntax typusFile
    then Left $ mkCompilationError SyntaxErrorKind "Malformed syntax detected" []
    else Right (IR.buildSourceIR typusFile)

--------------------------------------------------------------------------------
-- Dependent types
--------------------------------------------------------------------------------

checkDependentTypes :: TypusFile -> Either CompilationError ()
checkDependentTypes typusFile =
  let fileEnabled = case fdDependentTypes (tfDirectives typusFile) of
        Just True -> True
        _ -> False
      blockEnabled = any (bdDependentTypes . cbDirectives) (tfBlocks typusFile)
      shouldCheck = fileEnabled || blockEnabled
  in if shouldCheck
        then case extractDependentTypeContent typusFile of
          [] -> Right ()
          content ->
            case runDependentTypesParser content of
              Left err -> Left $ mkCompilationError DependentTypeErrorKind ("Dependent type parsing error: " ++ err) []
              Right (_, parser) ->
                let errors = parserErrors parser
                in if null errors
                      then Right ()
                      else let (msg, locs) = formatDependentTypeErrors errors
                           in Left $ mkCompilationError DependentTypeErrorKind ("Dependent type errors: " ++ msg) locs
        else Right ()

extractDependentTypeContent :: TypusFile -> String
extractDependentTypeContent typusFile =
  let dependentBlocks = filter (bdDependentTypes . cbDirectives) (tfBlocks typusFile)
  in concatMap cbContent dependentBlocks

formatDependentTypeErrors :: [DependentTypeError] -> (String, [SourceLocation])
formatDependentTypeErrors errs =
  let formatted = map format errs
      message = intercalate "; " (map fst formatted)
      locations = catMaybes (map snd formatted)
  in (message, locations)
  where
    format (SyntaxError msg line snippet) =
      let base = "Syntax error at line " ++ show line ++ ": " ++ msg ++ if null snippet then "" else " (" ++ snippet ++ ")"
          loc = if line > 0 then Just (sourceLocation Nothing (Just line) Nothing) else Nothing
      in (base, loc)
    format (InvalidTypeSyntax msg) = ("Invalid type syntax: " ++ msg, Nothing)
    format (MissingConstraint msg) = ("Missing constraint: " ++ msg, Nothing)
    format (InvalidParameter msg) = ("Invalid parameter: " ++ msg, Nothing)
    format (ConstraintParseError msg) = ("Constraint parse error: " ++ msg, Nothing)
    format (TypeVariableError msg) = ("Type variable error: " ++ msg, Nothing)

--------------------------------------------------------------------------------
-- Ownership
--------------------------------------------------------------------------------

checkOwnership :: TypusFile -> Either CompilationError ()
checkOwnership typusFile =
  let fileEnabled = case fdOwnership (tfDirectives typusFile) of
        Just True -> True
        _ -> False
      blockEnabled = any (bdOwnership . cbDirectives) (tfBlocks typusFile)
      shouldCheck = fileEnabled || blockEnabled
      fullContent = intercalate "\n" $ map cbContent (tfBlocks typusFile)
      contentToCheck = if fileEnabled then fullContent else extractOwnershipContent typusFile
  in if shouldCheck
        then case contentToCheck of
          "" -> Right ()
          content ->
            let errors0 = analyzeOwnership content
                valueCopyVars = extractValueCopyVars content
                errors = filter (not . isIgnorableOwnershipError valueCopyVars) errors0
            in if null errors
                  then Right ()
                  else Left $ mkCompilationError OwnershipErrorKind ("Ownership errors: " ++ formatOwnershipErrors errors) []
        else Right ()

extractOwnershipContent :: TypusFile -> String
extractOwnershipContent typusFile =
  let ownershipBlocks = filter (bdOwnership . cbDirectives) (tfBlocks typusFile)
  in concatMap cbContent ownershipBlocks

extractValueCopyVars :: String -> [String]
extractValueCopyVars src =
  let ls = lines src
      isValueInit t = any (`isInfixOf` t) ["\"", " true", " false", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9"]
      pickName t = trim $ takeWhile (/= ':') t
  in [ pickName (trim l)
     | l <- ls
     , let t = trim l
     , ":=" `isInfixOf` t
     , isValueInit t
     , not ("&" `isInfixOf` t)
     ]

isIgnorableOwnershipError :: [String] -> OwnershipError -> Bool
isIgnorableOwnershipError valueCopyVars err = case err of
  UseAfterMove v -> v `elem` valueCopyVars
  _              -> False

--------------------------------------------------------------------------------
-- Syntax & type analysis
--------------------------------------------------------------------------------

hasMalformedSyntax :: TypusFile -> Bool
hasMalformedSyntax typusFile =
  let source = IR.rawSourceFromTypus typusFile
  in null (trim source) || case parseGoModule (lines source) of
       Left _  -> True
       Right _ -> False

-- Basic type representations remain intentionally coarse grained.
data Type
  = IntType
  | StringType
  | BoolType
  | FloatType
  | VoidType
  | FunctionType [Type] Type
  deriving (Eq, Show)

data TypeEnv = TypeEnv
  { varTypes      :: [(String, Type)]
  , functionTypes :: [(String, ([Type], Type))]
  } deriving (Show)

emptyTypeEnv :: TypeEnv
emptyTypeEnv = TypeEnv [] []

hasTypeErrors :: TypusFile -> Bool
hasTypeErrors typusFile =
  case IR.moduleFromTypus typusFile of
    Left _ -> True
    Right goModule ->
      let decls = extractDeclarationsFromModule goModule
          calls = extractFunctionCallsFromModule goModule
          env = buildTypeEnv decls
          varDecls = filter (isPrefixOf "var " . trim) decls
      in any (not . checkTypeError env) (varDecls ++ calls)

extractDeclarations :: String -> [String]
extractDeclarations content =
  case parseGoModule (lines content) of
    Left _         -> []
    Right goModule -> extractDeclarationsFromModule goModule

extractFunctionCalls :: String -> [String]
extractFunctionCalls content =
  case parseGoModule (lines content) of
    Left _         -> []
    Right goModule -> extractFunctionCallsFromModule goModule

extractDeclarationsFromModule :: GoModule -> [String]
extractDeclarationsFromModule GoModule{..} =
  concatMap gather gmDecls
  where
    gather (GoFunc (FuncDecl ls)) = case ls of
      []      -> []
      (h:_)   -> let t = trim h in [t | isPrefixOf "func" t]
    gather (GoVar (VarDecl ls _)) = map trim ls
    gather (GoConst (ConstDecl ls _)) = map trim ls
    gather _ = []

extractFunctionCallsFromModule :: GoModule -> [String]
extractFunctionCallsFromModule GoModule{..} =
  concatMap gather gmDecls
  where
    gather (GoFunc (FuncDecl ls)) =
      case ls of
        []      -> []
        (_:body) -> [ trim line | line <- body, isFunctionCall (trim line) ]
    gather (GoStatement (StatementBlock ls)) = [ trim line | line <- ls, isFunctionCall (trim line) ]
    gather _ = []

buildTypeEnv :: [String] -> TypeEnv
buildTypeEnv = foldl addDeclaration emptyTypeEnv
  where
    addDeclaration env line
      | "var " `isPrefixOf` trim line || "const " `isPrefixOf` trim line = addVarDeclaration env line
      | "func " `isPrefixOf` trim line = addFunctionDeclaration env line
      | otherwise = env

addVarDeclaration :: TypeEnv -> String -> TypeEnv
addVarDeclaration env line =
  let t = trim line
      withoutKw = if "var " `isPrefixOf` t then drop 4 t else if "const " `isPrefixOf` t then drop 6 t else t
      (varName, rest) = break (\c -> c == ' ' || c == '=') withoutKw
      varType = trim $ dropWhile (\c -> c == ' ' || c == '=') rest
      inferredType = inferVarType varType
  in env { varTypes = (varName, inferredType) : varTypes env }

addFunctionDeclaration :: TypeEnv -> String -> TypeEnv
addFunctionDeclaration env line
  | isMethodDeclaration line = env
  | otherwise =
      let t = drop 4 (trim line)
          (funcName, rest) = break (\c -> c == ' ' || c == '(') t
          paramsAndReturn = dropWhile (\c -> c == ' ' || c == '(') rest
          (params, returnTypeRaw) = break (== ')') paramsAndReturn
          paramTypes = map (inferVarType . lastWord) $ splitByComma params
          returnType = if ") " `isPrefixOf` drop 1 returnTypeRaw
                         then inferVarType $ trim $ drop 2 returnTypeRaw
                         else VoidType
      in env { functionTypes = (funcName, (paramTypes, returnType)) : functionTypes env }
  where
    lastWord s = case words s of
      [] -> ""
      ws -> last ws

inferVarType :: String -> Type
inferVarType typeStr
  | "int" `isPrefixOf` typeStr    = IntType
  | "string" `isPrefixOf` typeStr = StringType
  | "bool" `isPrefixOf` typeStr   = BoolType
  | "float" `isPrefixOf` typeStr  = FloatType
  | otherwise                     = VoidType

checkTypeError :: TypeEnv -> String -> Bool
checkTypeError env line
  | "var " `isPrefixOf` trim line || "const " `isPrefixOf` trim line = checkVarDeclaration env line
  | isFunctionCall (trim line) = checkFunctionCall env line
  | otherwise = False

checkVarDeclaration :: TypeEnv -> String -> Bool
checkVarDeclaration env line =
  let t = trim line
      withoutKw = if "var " `isPrefixOf` t then drop 4 t else if "const " `isPrefixOf` t then drop 6 t else t
      (varName, rest) = break (\c -> c == ' ' || c == '=') withoutKw
      varType = trim $ dropWhile (\c -> c == ' ' || c == '=') rest
  in case lookup varName (varTypes env) of
      Nothing -> True
      Just declaredType ->
        let inferredType = inferVarType varType
        in inferredType == VoidType || declaredType == inferredType

checkFunctionCall :: TypeEnv -> String -> Bool
checkFunctionCall env line =
  let callPart = if "=" `isInfixOf` line then trim $ dropWhile (/= '=') line else line
      (funcName, rest) = span (/= '(') (trim callPart)
      argsStr = takeWhile (/= ')') $ drop 1 rest
      args = splitByComma argsStr
      argTypes = map inferVarType args
  in case lookup funcName (functionTypes env) of
      Nothing -> True
      Just (paramTypes, _) ->
        let unknown t = t == VoidType
        in if any unknown paramTypes || any unknown argTypes
              then True
              else length argTypes == length paramTypes && and (zipWith (==) argTypes paramTypes)

isMethodDeclaration :: String -> Bool
isMethodDeclaration line =
  let trimmed = trim line
  in "func (" `isPrefixOf` trimmed

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

isFunctionCall :: String -> Bool
isFunctionCall line =
  let t = trim line
  in "(" `isInfixOf` t
     && not ("func " `isPrefixOf` t)
     && not ("type " `isPrefixOf` t)
     && not ("if " `isPrefixOf` t)
     && not ("for " `isPrefixOf` t)
     && not ("switch " `isPrefixOf` t)

splitByComma :: String -> [String]
splitByComma s = case break (== ',') s of
    (a, []) -> [trim a]
    (a, _:b) -> trim a : splitByComma b

trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace

--------------------------------------------------------------------------------
-- Go code generation helper
--------------------------------------------------------------------------------

generateGoCode :: TypusFile -> String
generateGoCode typusFile =
  case IR.moduleFromTypus typusFile of
    Left _         -> IR.rawSourceFromTypus typusFile
    Right goModule -> renderGoModule goModule
