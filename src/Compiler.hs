{-# LANGUAGE RecordWildCards #-}

module Compiler
  ( compile
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

import Data.Char (isSpace)
import Data.Function (on)
import Data.List (dropWhileEnd, intercalate, isInfixOf, isPrefixOf, nubBy)

--------------------------------------------------------------------------------
-- Public API
--------------------------------------------------------------------------------

compile :: TypusFile -> Either String String
compile typusFile =
  if hasMalformedSyntax typusFile
    then Left "Malformed syntax detected"
    else case checkDependentTypes typusFile of
      Left err -> Left err
      Right _ ->
        if hasTypeErrors typusFile
          then Left "Type errors detected"
          else case checkOwnership typusFile of
            Left err -> Left err
            Right _ -> Right (generateGoCode typusFile)

--------------------------------------------------------------------------------
-- Dependent types
--------------------------------------------------------------------------------

checkDependentTypes :: TypusFile -> Either String ()
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
              Left err -> Left $ "Dependent type parsing error: " ++ err
              Right (_, parser) ->
                let errors = parserErrors parser
                in if null errors
                      then Right ()
                      else Left $ "Dependent type errors: " ++ formatDependentTypeErrors errors
        else Right ()

extractDependentTypeContent :: TypusFile -> String
extractDependentTypeContent typusFile =
  let dependentBlocks = filter (bdDependentTypes . cbDirectives) (tfBlocks typusFile)
  in concatMap cbContent dependentBlocks

formatDependentTypeErrors :: [DependentTypeError] -> String
formatDependentTypeErrors = intercalate "; " . map formatError
  where
    formatError (SyntaxError msg line snippet) =
      "Syntax error at line " ++ show line ++ ": " ++ msg ++ " (" ++ snippet ++ ")"
    formatError (InvalidTypeSyntax msg) = "Invalid type syntax: " ++ msg
    formatError (MissingConstraint msg) = "Missing constraint: " ++ msg
    formatError (InvalidParameter msg) = "Invalid parameter: " ++ msg
    formatError (ConstraintParseError msg) = "Constraint parse error: " ++ msg
    formatError (TypeVariableError msg) = "Type variable error: " ++ msg

--------------------------------------------------------------------------------
-- Ownership
--------------------------------------------------------------------------------

checkOwnership :: TypusFile -> Either String ()
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
                  else Left $ "Ownership errors: " ++ formatOwnershipErrors errors
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
  let source = rawSourceFromTypus typusFile
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
  case moduleFromTypus typusFile of
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
    Left _        -> []
    Right goModule -> extractDeclarationsFromModule goModule

extractFunctionCalls :: String -> [String]
extractFunctionCalls content =
  case parseGoModule (lines content) of
    Left _        -> []
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

--------------------------------------------------------------------------------
-- AST-driven code generation pipeline
--------------------------------------------------------------------------------

generateGoCode :: TypusFile -> String
generateGoCode typusFile =
  case moduleFromTypus typusFile of
    Left _        -> rawSourceFromTypus typusFile
    Right goModule -> renderGoModule goModule

moduleFromTypus :: TypusFile -> Either String GoModule
moduleFromTypus typusFile = do
  let rawSource = rawSourceFromTypus typusFile
  parsed <- parseGoModule (lines rawSource)
  let module0 = parsed { gmBuildTags = if null (tfBuildTags typusFile)
                                       then gmBuildTags parsed
                                       else tfBuildTags typusFile
                       }
      module1 = applyGenerics module0
      module2 = ensurePackageDecl module1
      module3 = ensureMainFunction module2
      module4 = attachInferredImports module3
  pure module4

rawSourceFromTypus :: TypusFile -> String
rawSourceFromTypus TypusFile{..} = intercalate "\n" $ map cbContent tfBlocks

applyGenerics :: GoModule -> GoModule
applyGenerics goModule =
  goModule { gmDecls = map convertDecl (gmDecls goModule) }
  where
    convertDecl decl = mapDeclLines convertLine decl

    convertLine line
      | "type " `isPrefixOf` trim line = convertTypeDeclLine line
      | otherwise = replaceGenericAngles line

    convertTypeDeclLine line =
      let t = trim line
      in case break (== ' ') (drop 5 t) of
          (nameAndParams, rest) ->
            case break (== '<') nameAndParams of
              (name, '<':paramRest) ->
                let (params, after) = break (== '>') paramRest
                    paramList = filter (not . null) $ map trim (splitByComma params)
                    annotated = intercalate ", " [ p ++ " any" | p <- paramList ]
                    headReplacement = "type " ++ name ++ "[" ++ annotated ++ "]"
                in joinPreserveIndent line (headReplacement ++ drop 1 after ++ rest)
              _ -> line

replaceGenericAngles :: String -> String
replaceGenericAngles = go
  where
    go [] = []
    go (c:'<':xs)
      | isIdentChar c && not (startsWithDash xs) =
          case takeUntilMatching '>' xs 0 [] of
            (inside, rest, True) -> c : '[' : inside ++ ']' : go rest
            _                    -> c : '<' : go xs
      | otherwise = c : '<' : go xs
    go (c:xs) = c : go xs

    isIdentChar ch =
      (ch >= '0' && ch <= '9') || (ch >= 'A' && ch <= 'Z') || (ch >= 'a' && ch <= 'z') || ch == '_' || ch == ']'
    startsWithDash ('-':_) = True
    startsWithDash _       = False

    takeUntilMatching _ [] _ acc = (reverse acc, [], False)
    takeUntilMatching end (y:ys) depth acc
      | y == end && depth == 0 = (reverse acc, ys, True)
      | y == end = takeUntilMatching end ys (depth - 1) (y:acc)
      | y == '<' = takeUntilMatching end ys (depth + 1) (y:acc)
      | otherwise = takeUntilMatching end ys depth (y:acc)

ensurePackageDecl :: GoModule -> GoModule
ensurePackageDecl goModule@GoModule{..} =
  case gmPackage of
    Just _  -> goModule
    Nothing -> goModule { gmPackage = Just (PackageDecl "main") }

ensureMainFunction :: GoModule -> GoModule
ensureMainFunction goModule@GoModule{..} =
  let (statements, others) = partitionStatements gmDecls
      hasMain = any isMainFunc others
  in if null statements || hasMain
        then goModule
        else
          let body = concatMap statementLines statements
              newMain = GoFunc (FuncDecl ("func main() {" : map indent body ++ ["}"]))
          in goModule { gmDecls = others ++ [newMain] }
  where
    isMainFunc (GoFunc funcDecl) = isMainFunction funcDecl
    isMainFunc _                 = False

    indent line
      | null line = ""
      | otherwise = "  " ++ line

attachInferredImports :: GoModule -> GoModule
attachInferredImports goModule@GoModule{..} =
  let content = moduleContentText goModule
      inferred = detectImports content
      merged = mergeImports gmImports inferred
  in goModule { gmImports = merged }

moduleContentText :: GoModule -> String
moduleContentText GoModule{..} =
  unlines $ concatMap flattenDeclLines gmDecls

partitionStatements :: [GoDecl] -> ([StatementBlock], [GoDecl])
partitionStatements = foldr step ([], [])
  where
    step decl (stmts, acc) = case decl of
      GoStatement block -> (block:stmts, acc)
      _                 -> (stmts, decl:acc)

mergeImports :: [ImportDecl] -> [ImportDecl] -> [ImportDecl]
mergeImports existing extras =
  let keys = map importKey existing
      newOnes = filter (\imp -> importKey imp `notElem` keys) extras
  in nubBy ((==) `on` importKey) (existing ++ newOnes)

importKey :: ImportDecl -> (Maybe String, String)
importKey ImportDecl{..} = (importAlias, importPath)

mapDeclLines :: (String -> String) -> GoDecl -> GoDecl
mapDeclLines f decl = case decl of
  GoFunc (FuncDecl ls)            -> GoFunc (FuncDecl (map f ls))
  GoType (TypeDecl ls g)          -> GoType (TypeDecl (map f ls) g)
  GoVar (VarDecl ls g)            -> GoVar (VarDecl (map f ls) g)
  GoConst (ConstDecl ls g)        -> GoConst (ConstDecl (map f ls) g)
  GoStatement (StatementBlock ls) -> GoStatement (StatementBlock (map f ls))
  GoRaw block                     -> GoRaw block

joinPreserveIndent :: String -> String -> String
joinPreserveIndent original replacement =
  let indent = takeWhile isSpace original
  in indent ++ replacement

--------------------------------------------------------------------------------
-- Import inference
--------------------------------------------------------------------------------

data ImportDetector = ImportDetector
  { detectorAlias :: Maybe String
  , detectorPath  :: String
  , detectorMatch :: String -> Bool
  }

simpleDetector :: String -> [String] -> ImportDetector
simpleDetector path patterns = ImportDetector Nothing path (\txt -> any (`isInfixOf` txt) patterns)

importDetectors :: [ImportDetector]
importDetectors =
  [ simpleDetector "bufio" ["bufio."]
  , simpleDetector "container/list" ["container/list", "list."]
  , simpleDetector "context" ["context."] `withGuard` \txt -> not ("context :=" `isInfixOf` txt || "&context" `isInfixOf` txt)
  , simpleDetector "log" ["log."]
  , simpleDetector "reflect" ["reflect."]
  , simpleDetector "fmt" ["fmt.", "fmt.Println", "fmt.Printf"]
  , simpleDetector "math" ["math.", "math.Pi", "math.Sqrt"]
  , simpleDetector "math/cmplx" ["cmplx."]
  , simpleDetector "math/big" ["big."]
  , detectorWith "math/rand" ["math/rand", "rand.Seed", "rand.Intn"] (\txt -> not ("crypto/rand" `isInfixOf` txt))
  , simpleDetector "time" ["time.", "time.Now", "time.Sleep"]
  , simpleDetector "os" ["os.", "os.Create", "os.ReadFile", "os.Open"]
  , simpleDetector "path/filepath" ["filepath."]
  , detectorWith "io" [" io.", "io.Copy(", "io.Reader", "io.Writer", "io.ReadFull("] (\txt -> not ("bufio" `isInfixOf` txt))
  , simpleDetector "io/ioutil" ["ioutil."]
  , simpleDetector "strings" ["strings."]
  , simpleDetector "sync" ["sync."]
  , simpleDetector "sync/atomic" ["atomic."]
  , simpleDetector "runtime" ["runtime."]
  , simpleDetector "unicode/utf8" ["unicode/utf8", "utf8."]
  , simpleDetector "unsafe" ["unsafe."]
  , simpleDetector "strconv" ["strconv."]
  , simpleDetector "encoding/json" ["json."]
  , simpleDetector "encoding/xml" ["xml."]
  , simpleDetector "regexp" ["regexp."]
  , simpleDetector "errors" ["errors."]
  , simpleDetector "net/http" ["http."]
  , simpleDetector "net/url" ["url."]
  , simpleDetector "net" ["net."]
  , simpleDetector "sort" ["sort."]
  , simpleDetector "crypto/md5" ["md5."]
  , simpleDetector "crypto/sha1" ["sha1."]
  , simpleDetector "crypto/sha256" ["sha256."]
  , simpleDetector "crypto/sha512" ["sha512."]
  , simpleDetector "crypto/aes" ["aes."]
  , simpleDetector "crypto/cipher" ["cipher."]
  , simpleDetector "crypto/rand" ["crypto/rand", "rand.Reader"]
  , simpleDetector "encoding/base64" ["base64."]
  , simpleDetector "encoding/hex" ["hex."]
  , simpleDetector "encoding/csv" ["csv."]
  , simpleDetector "bytes" ["bytes."]
  , simpleDetector "encoding/binary" ["binary."]
  , simpleDetector "compress/gzip" ["gzip."]
  , simpleDetector "container/ring" ["ring.New"]
  , simpleDetector "math/bits" ["bits."]
  , simpleDetector "hash/fnv" ["fnv."]
  , simpleDetector "syscall" ["syscall."]
  , detectorWith "database/sql" ["database/sql", "sql.DB", "sql.Open", "sql.Query", "sql.Exec"] (const True)
  , ImportDetector (Just "_") "github.com/mattn/go-sqlite3" (\txt -> "github.com/mattn/go-sqlite3" `isInfixOf` txt || "_ \"github.com/mattn/go-sqlite3\"" `isInfixOf` txt)
  , simpleDetector "testing" ["*testing.T", "*testing.B", "testing.", "func Test", "func Benchmark"]
  ]
  where
    detectorWith path patterns guardFn = ImportDetector Nothing path (\txt -> any (`isInfixOf` txt) patterns && guardFn txt)

    withGuard detector guardFn = detector { detectorMatch = \txt -> detectorMatch detector txt && guardFn txt }

detectImports :: String -> [ImportDecl]
detectImports content =
  [ ImportDecl (detectorAlias det) (detectorPath det)
  | det <- importDetectors
  , detectorMatch det content
  ]

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

isMethodDeclaration :: String -> Bool
isMethodDeclaration line = "func (" `isPrefixOf` trim line

isFunctionCall :: String -> Bool
isFunctionCall line =
  let t = trim line
  in "(" `isInfixOf` t && ")" `isInfixOf` t && not ("func" `isPrefixOf` t) && not ("var " `isPrefixOf` t) && not ("const " `isPrefixOf` t)

splitByComma :: String -> [String]
splitByComma s = map trim $ splitOn ',' s
  where
    splitOn _ [] = []
    splitOn delimiter str =
      let (token, rest) = break (== delimiter) str
      in token : splitOn delimiter (drop 1 rest)

trim :: String -> String
trim = dropWhile isSpace . dropWhileEnd isSpace
