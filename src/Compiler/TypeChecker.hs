{-# LANGUAGE RecordWildCards #-}
module Compiler.TypeChecker (
    Type(..),
    TypeEnv(..),
    hasTypeErrors,
    extractDeclarations,
    extractFunctionCalls,
    buildTypeEnv,
    isMethodDeclaration,
    checkTypeError,
    hasMalformedSyntax,
    trim
) where

import Parser (TypusFile(..))
import Compiler.GoAst
import qualified Compiler.IR as IR

import Data.List (isInfixOf, isPrefixOf)
import Utils (splitByComma, trim)

data Type
    = IntType
    | StringType
    | BoolType
    | FloatType
    | VoidType
    | FunctionType [Type] Type
    deriving (Eq, Show)

data TypeEnv = TypeEnv
    { varTypes :: [(String, Type)]
    , functionTypes :: [(String, ([Type], Type))]
    } deriving (Show)

emptyTypeEnv :: TypeEnv
emptyTypeEnv = TypeEnv [] []

hasMalformedSyntax :: TypusFile -> Bool
hasMalformedSyntax typusFile =
    let source = IR.rawSourceFromTypus typusFile
    in null (trim source) || case parseGoModule (lines source) of
        Left _ -> True
        Right _ -> False

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
        Left _ -> []
        Right goModule -> extractDeclarationsFromModule goModule

extractFunctionCalls :: String -> [String]
extractFunctionCalls content =
    case parseGoModule (lines content) of
        Left _ -> []
        Right goModule -> extractFunctionCallsFromModule goModule

extractDeclarationsFromModule :: GoModule -> [String]
extractDeclarationsFromModule GoModule{..} = concatMap gather gmDecls
  where
    gather (GoFunc (FuncDecl ls)) = case ls of
        [] -> []
        (h : _) -> let t = trim h in [t | isPrefixOf "func" t]
    gather (GoVar (VarDecl ls _)) = map trim ls
    gather (GoConst (ConstDecl ls _)) = map trim ls
    gather _ = []

extractFunctionCallsFromModule :: GoModule -> [String]
extractFunctionCallsFromModule GoModule{..} = concatMap gather gmDecls
  where
    gather (GoFunc (FuncDecl ls)) =
        case ls of
            [] -> []
            (_ : body) -> [trim line | line <- body, isFunctionCall (trim line)]
    gather (GoStatement (StatementBlock ls)) = [trim line | line <- ls, isFunctionCall (trim line)]
    gather _ -> []

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
            returnType =
                if ") " `isPrefixOf` drop 1 returnTypeRaw
                    then inferVarType $ trim $ drop 2 returnTypeRaw
                    else VoidType
        in env { functionTypes = (funcName, (paramTypes, returnType)) : functionTypes env }
  where
    lastWord s = case words s of
        [] -> ""
        ws -> last ws

inferVarType :: String -> Type
inferVarType typeStr
    | "int" `isPrefixOf` typeStr = IntType
    | "string" `isPrefixOf` typeStr = StringType
    | "bool" `isPrefixOf` typeStr = BoolType
    | "float" `isPrefixOf` typeStr = FloatType
    | otherwise = VoidType

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

isFunctionCall :: String -> Bool
isFunctionCall line =
    let t = trim line
    in "(" `isInfixOf` t
        && not ("func " `isPrefixOf` t)
        && not ("type " `isPrefixOf` t)
        && not ("if " `isPrefixOf` t)
        && not ("for " `isPrefixOf` t)
        && not ("switch " `isPrefixOf` t)


