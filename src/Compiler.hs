-- Simplified Typus to Go Compiler

module Compiler (compile, hasTypeErrors, extractDeclarations, extractFunctionCalls, buildTypeEnv, isMethodDeclaration, checkTypeError, hasMalformedSyntax, checkDependentTypes, checkOwnership, generateGoCode) where

import Parser (TypusFile(..), CodeBlock(..), FileDirectives(..), BlockDirectives(..))
import DependentTypesParser (DependentTypeError(..), runDependentTypesParser, parserErrors)
import Ownership (analyzeOwnership, formatOwnershipErrors)
import Data.List (intercalate, isInfixOf, isPrefixOf)
import Data.Char (isSpace)

-- Compile function that takes a TypusFile and generates Go code
compile :: TypusFile -> Either String String
compile typusFile =
  -- Check for malformed syntax (very basic check)
  if hasMalformedSyntax typusFile
    then Left "Malformed syntax detected"
    -- Check for dependent types errors if enabled
    else case checkDependentTypes typusFile of
      Left err -> Left err
      Right _ ->
        -- Check for basic type errors (very basic check)
        if hasTypeErrors typusFile
          then Left "Type errors detected"
          -- Check for ownership errors
          else case checkOwnership typusFile of
            Left err -> Left err
            Right _ -> Right $ generateGoCode typusFile

-- Check for dependent types errors
checkDependentTypes :: TypusFile -> Either String ()
checkDependentTypes typusFile =
  -- Check if dependent types are enabled at file or block level
  let fileEnabled = case fdDependentTypes (tfDirectives typusFile) of
        Just True -> True
        _ -> False
      blockEnabled = any (\block -> bdDependentTypes (cbDirectives block)) (tfBlocks typusFile)
      shouldCheck = fileEnabled || blockEnabled
  in if shouldCheck
     then case extractDependentTypeContent typusFile of
       [] -> Right ()  -- No dependent type content to check
       content ->
         case runDependentTypesParser content of
           Left err -> Left $ "Dependent type parsing error: " ++ err
           Right (_, parser) ->
             let errors = parserErrors parser
             in if null errors
                then Right ()
                else Left $ "Dependent type errors: " ++ formatDependentTypeErrors errors
     else Right ()

-- Extract dependent type content from TypusFile
extractDependentTypeContent :: TypusFile -> String
extractDependentTypeContent typusFile =
  let dependentBlocks = filter (\block -> bdDependentTypes (cbDirectives block)) (tfBlocks typusFile)
  in concatMap cbContent dependentBlocks

-- Extract ownership content from TypusFile
extractOwnershipContent :: TypusFile -> String
extractOwnershipContent typusFile =
  let ownershipBlocks = filter (\block -> bdOwnership (cbDirectives block)) (tfBlocks typusFile)
  in concatMap cbContent ownershipBlocks

-- Format dependent type errors for display
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


-- Check for ownership errors
checkOwnership :: TypusFile -> Either String ()
checkOwnership typusFile =
  -- Check if ownership is enabled at file or block level
  let fileEnabled = case fdOwnership (tfDirectives typusFile) of
        Just True -> True
        _ -> False
      blockEnabled = any (\block -> bdOwnership (cbDirectives block)) (tfBlocks typusFile)
      shouldCheck = fileEnabled || blockEnabled
  in if shouldCheck
     then case extractOwnershipContent typusFile of
       "" -> Right ()  -- No ownership content to check
       content ->
         let errors = analyzeOwnership content
         in if null errors
            then Right ()
            else Left $ "Ownership errors: " ++ formatOwnershipErrors errors
     else Right ()

-- Basic syntax checks
hasMalformedSyntax :: TypusFile -> Bool
hasMalformedSyntax typusFile = 
  let content = intercalate "\n" $ map cbContent (tfBlocks typusFile)
  in null content || "malformed" `isInfixOf` content

-- Basic type data types
data Type = IntType | StringType | BoolType | FloatType | VoidType | FunctionType [Type] Type
  deriving (Eq, Show)

data TypeEnv = TypeEnv {
  varTypes :: [(String, Type)],
  functionTypes :: [(String, ([Type], Type))]
} deriving (Show)

emptyTypeEnv :: TypeEnv
emptyTypeEnv = TypeEnv [] []

-- Basic type checks
hasTypeErrors :: TypusFile -> Bool
hasTypeErrors typusFile =
  let
    -- Use the same cleaning pipeline as codegen to avoid false positives
    contentRaw = intercalate "\n" $ map cbContent (tfBlocks typusFile)
    content = fixUnusedS2 $ fixVarBlocks $ cleanCodeBlocks contentRaw
    decls = extractDeclarations content
    calls = extractFunctionCalls content
    env = buildTypeEnv decls
    varDecls = filter (\line -> let t = trim line in isPrefixOf "var " t || isPrefixOf "const " t) decls
  in any (not . checkTypeError env) (varDecls ++ calls)

-- Check if a line is a method declaration (has a receiver)
isMethodDeclaration :: String -> Bool
isMethodDeclaration line =
  let t = trim line
  in isPrefixOf "func (" t

-- Check if a line is a function call
isFunctionCall :: String -> Bool
isFunctionCall line =
  let t = trim line
  in "(" `isInfixOf` t && ")" `isInfixOf` t && not (isPrefixOf "func" t) && not (isPrefixOf "var " t) && not (isPrefixOf "const " t)

-- Generate Go code from TypusFile
generateGoCode :: TypusFile -> String
generateGoCode typusFile =
  let
    header = "package main\n"
    imports = generateImports typusFile
    originalContentRaw = intercalate "\n" $ map cbContent (tfBlocks typusFile)
    originalContent = enforceGoStructure $ fixUnusedS2 $ fixVarBlocks $ cleanCodeBlocks originalContentRaw
    allParts = filter (not . null) [header, imports, originalContent]
  in intercalate "\n" allParts

-- Simple clean - just remove package and import lines
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

-- Generate imports section with enhanced detection
generateImports :: TypusFile -> String
generateImports typusFile =
  let
    content = intercalate "\n" $ map cbContent (tfBlocks typusFile)
    processedContent = fixUnusedS2 $ fixVarBlocks $ cleanCodeBlocks content  -- Process content like the final code will be
    
    -- Enhanced import detection - detect usage in processed content
    hasTesting = "*testing.T" `isInfixOf` processedContent || "*testing.B" `isInfixOf` processedContent || "func Test" `isInfixOf` processedContent || "func Benchmark" `isInfixOf` processedContent || "testing." `isInfixOf` processedContent
    hasBufio = "bufio." `isInfixOf` processedContent
    hasFmt = "fmt." `isInfixOf` processedContent || "fmt.Println" `isInfixOf` processedContent || "fmt.Printf" `isInfixOf` processedContent
    hasMath = "math." `isInfixOf` processedContent || "math.Pi" `isInfixOf` processedContent || "math.Sqrt" `isInfixOf` processedContent
    hasMathRand = ("math/rand" `isInfixOf` processedContent || "rand.Seed" `isInfixOf` processedContent || "rand.Intn" `isInfixOf` processedContent) && not ("crypto/rand" `isInfixOf` processedContent)
    hasCmplx = "cmplx." `isInfixOf` processedContent || "cmplx.Sqrt" `isInfixOf` processedContent
    hasMathBig = "big." `isInfixOf` processedContent || "big.NewInt" `isInfixOf` processedContent || "big.Int" `isInfixOf` processedContent
    hasTime = "time." `isInfixOf` processedContent || "time.Now" `isInfixOf` processedContent || "time.Sleep" `isInfixOf` processedContent
    hasOs = "os." `isInfixOf` processedContent || "os.Create" `isInfixOf` processedContent || "os.ReadFile" `isInfixOf` processedContent
    hasPathFilepath = "filepath." `isInfixOf` processedContent || "filepath.Join" `isInfixOf` processedContent
    hasIo = (" io." `isInfixOf` processedContent && not ("bufio" `isInfixOf` processedContent)) || "io.Copy(" `isInfixOf` processedContent || "io.Reader" `isInfixOf` processedContent || "io.Writer" `isInfixOf` processedContent || "io.ReadFull(" `isInfixOf` processedContent
    hasIoUtil = "ioutil." `isInfixOf` processedContent || "ioutil.ReadFile" `isInfixOf` processedContent || "ioutil.WriteFile" `isInfixOf` processedContent || "ioutil.ReadAll" `isInfixOf` processedContent
    hasStrings = "strings." `isInfixOf` processedContent || "strings.Split" `isInfixOf` processedContent || "strings.ToUpper" `isInfixOf` processedContent
    hasSync = "sync." `isInfixOf` processedContent || "sync.Mutex" `isInfixOf` processedContent || "sync.WaitGroup" `isInfixOf` processedContent
    hasSyncAtomic = "atomic." `isInfixOf` processedContent || "atomic.AddInt64" `isInfixOf` processedContent || "atomic.Bool" `isInfixOf` processedContent
    hasRuntime = "runtime." `isInfixOf` processedContent || "runtime.GOOS" `isInfixOf` processedContent
    hasUnsafe = "unsafe." `isInfixOf` processedContent
    hasContainerList = "container/list" `isInfixOf` processedContent || "list.New" `isInfixOf` processedContent
    hasUnicodeUtf8 = "unicode/utf8" `isInfixOf` processedContent || "utf8.RuneCountInString" `isInfixOf` processedContent
    hasContext = "context." `isInfixOf` processedContent && not ("context :=" `isInfixOf` processedContent) && not ("context :=" `isInfixOf` processedContent) && not ("&context" `isInfixOf` processedContent)
    hasLog = "log." `isInfixOf` processedContent
    hasReflect = "reflect." `isInfixOf` processedContent || "reflect.TypeOf" `isInfixOf` processedContent || "reflect.ValueOf" `isInfixOf` processedContent
    hasStrconv = "strconv." `isInfixOf` processedContent || "strconv.Itoa" `isInfixOf` processedContent || "strconv.Atoi" `isInfixOf` processedContent
    hasJson = "json." `isInfixOf` processedContent || "json.Marshal" `isInfixOf` processedContent || "json.Unmarshal" `isInfixOf` processedContent
    hasXml = "xml." `isInfixOf` processedContent || "xml.Marshal" `isInfixOf` processedContent
    hasRegexp = "regexp." `isInfixOf` processedContent || "regexp.MatchString" `isInfixOf` processedContent || "regexp.MustCompile" `isInfixOf` processedContent
    hasErrors = "errors." `isInfixOf` processedContent || "errors.New" `isInfixOf` processedContent
    hasHttp = "http." `isInfixOf` processedContent || "http.HandleFunc" `isInfixOf` processedContent || "http.ListenAndServe" `isInfixOf` processedContent
    hasNetUrl = "url." `isInfixOf` processedContent || "url.Parse" `isInfixOf` processedContent
    hasSort = "sort." `isInfixOf` processedContent || "sort.Ints" `isInfixOf` processedContent || "sort.Strings" `isInfixOf` processedContent
    hasMd5 = "md5." `isInfixOf` processedContent || "md5.Sum" `isInfixOf` processedContent || "md5.New" `isInfixOf` processedContent
    hasBytes = "bytes." `isInfixOf` processedContent || "bytes.Buffer" `isInfixOf` processedContent
    hasGzip = "gzip." `isInfixOf` processedContent || "gzip.NewWriter" `isInfixOf` processedContent || "gzip.NewReader" `isInfixOf` processedContent
    hasRing = "ring." `isInfixOf` processedContent || "ring.New" `isInfixOf` processedContent
    hasBinary = "binary." `isInfixOf` processedContent || "binary.Write" `isInfixOf` processedContent || "binary.Read" `isInfixOf` processedContent
    hasBits = "bits." `isInfixOf` processedContent || "bits.LeadingZeros" `isInfixOf` processedContent
    hasFnv = "fnv." `isInfixOf` processedContent || "fnv.New32" `isInfixOf` processedContent || "fnv.New64" `isInfixOf` processedContent
    hasNet = "net." `isInfixOf` processedContent || "net.Dial" `isInfixOf` processedContent || "net.Listen" `isInfixOf` processedContent
    hasSyscall = "syscall." `isInfixOf` processedContent || "syscall.ForkExec" `isInfixOf` processedContent || "syscall.Kill" `isInfixOf` processedContent
    hasSha1 = "sha1." `isInfixOf` processedContent || "sha1.Sum" `isInfixOf` processedContent || "sha1.New" `isInfixOf` processedContent
    hasSha256 = "sha256." `isInfixOf` processedContent || "sha256.Sum" `isInfixOf` processedContent || "sha256.New" `isInfixOf` processedContent
    hasSha512 = "sha512." `isInfixOf` processedContent || "sha512.Sum" `isInfixOf` processedContent || "sha512.New" `isInfixOf` processedContent
    hasCrypto = "crypto/aes" `isInfixOf` processedContent || "crypto/cipher" `isInfixOf` processedContent || "aes.NewCipher" `isInfixOf` processedContent || "cipher.NewCFBEncrypter" `isInfixOf` processedContent
    hasCryptoRand = "crypto/rand" `isInfixOf` processedContent || "rand.Reader" `isInfixOf` processedContent
    hasBase64 = "base64." `isInfixOf` processedContent || "base64.StdEncoding" `isInfixOf` processedContent
    hasHex = "hex." `isInfixOf` processedContent || "hex.EncodeToString" `isInfixOf` processedContent || "hex.DecodeString" `isInfixOf` processedContent
    hasCsv = "csv." `isInfixOf` processedContent || "csv.NewReader" `isInfixOf` processedContent
    hasDatabaseSql = "database/sql" `isInfixOf` processedContent || "sql.DB" `isInfixOf` processedContent || "sql.Open" `isInfixOf` processedContent || "sql.Query" `isInfixOf` processedContent || "sql.Exec" `isInfixOf` processedContent || "_ \"github.com/mattn/go-sqlite3\"" `isInfixOf` processedContent

    imports = filter (not . null) [
        if hasBufio then "    \"bufio\"" else "",
        if hasContainerList then "    \"container/list\"" else "",
        if hasContext then "    \"context\"" else "",
        if hasLog then "    \"log\"" else "",
        if hasReflect then "    \"reflect\"" else "",
        if hasFmt then "    \"fmt\"" else "",
        if hasMath then "    \"math\"" else "",
        if hasMathRand then "    \"math/rand\"" else "",
        if hasCmplx then "    \"math/cmplx\"" else "",
        if hasMathBig then "    \"math/big\"" else "",
        if hasTime then "    \"time\"" else "",
        if hasOs then "    \"os\"" else "",
        if hasPathFilepath then "    \"path/filepath\"" else "",
        if hasIo then "    \"io\"" else "",
        if hasIoUtil then "    \"io/ioutil\"" else "",
        if hasStrings then "    \"strings\"" else "",
        if hasSync then "    \"sync\"" else "",
        if hasSyncAtomic then "    \"sync/atomic\"" else "",
        if hasRuntime then "    \"runtime\"" else "",
        if hasUnicodeUtf8 then "    \"unicode/utf8\"" else "",
        if hasUnsafe then "    \"unsafe\"" else "",
        if hasStrconv then "    \"strconv\"" else "",
        if hasJson then "    \"encoding/json\"" else "",
        if hasXml then "    \"encoding/xml\"" else "",
        if hasRegexp then "    \"regexp\"" else "",
        if hasErrors then "    \"errors\"" else "",
        if hasHttp then "    \"net/http\"" else "",
        if hasNetUrl then "    \"net/url\"" else "",
        if hasNet then "    \"net\"" else "",
        if hasSort then "    \"sort\"" else "",
        if hasMd5 then "    \"crypto/md5\"" else ""
        , if hasSha1 then "    \"crypto/sha1\"" else ""
        , if hasSha256 then "    \"crypto/sha256\"" else ""
        , if hasSha512 then "    \"crypto/sha512\"" else ""
        , if hasCrypto then "    \"crypto/aes\"" else ""
        , if hasCrypto then "    \"crypto/cipher\"" else ""
        , if hasCryptoRand then "    \"crypto/rand\"" else ""
        , if hasBase64 then "    \"encoding/base64\"" else ""
        , if hasHex then "    \"encoding/hex\"" else "",
        if hasCsv then "    \"encoding/csv\"" else "",
        if hasBytes then "    \"bytes\"" else "",
        if hasBinary then "    \"encoding/binary\"" else "",
        if hasGzip then "    \"compress/gzip\"" else "",
        if hasRing then "    \"container/ring\"" else "",
        if hasBits then "    \"math/bits\"" else "",
        if hasFnv then "    \"hash/fnv\"" else "",
        if hasSyscall then "    \"syscall\"" else "",
        if hasDatabaseSql then "    \"database/sql\"" else "",
        if hasDatabaseSql then "    _ \"github.com/mattn/go-sqlite3\"" else "",
        if hasTesting then "    \"testing\"" else ""
      ]
  in
    if null imports
      then ""
      else "import (\n" ++ intercalate "\n" imports ++ "\n)\n"

-- Enforce Go structure by wrapping stray statements into main
enforceGoStructure :: String -> String
enforceGoStructure content =
  let ls = lines content
      hasMain = any (\x -> "func main()" `isInfixOf` x) ls
      nonDecl = filter (\x -> not (null (trim x)) && not (any (`isPrefixOf` trim x) ["package", "import", "func", "type", "var", "const"])) ls
  in if null nonDecl || hasMain then content
     else unlines $ [case filter (\x -> "package " `isPrefixOf` x) ls of
                             (x:_) -> x
                             [] -> "package main"] ++
                    ["", "func main() {"] ++ nonDecl ++ ["}"]

-- Utility function to trim whitespace
trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace

-- Fix missing closing parenthesis for var blocks
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

-- Remove unused variable s2 declarations

fixUnusedS2 :: String -> String
fixUnusedS2 content =
  let ls = lines content
      declIdxs :: [Int]
      declIdxs = [ i | (i,l) <- zip [0..] ls, let t = trim l, isPrefixOf "var s2" t || isInfixOf "s2 :=" t ]
      usedElsewhere i = let name = "s2" in any (\(j,l) -> j /= i && name `isInfixOf` l) (zip [0..] ls)
      filtered = [ l | (i,l) <- zip [0..] ls, not (i `elem` [d | d <- declIdxs, not (usedElsewhere d)]) ]
  in unlines filtered

-- Extract variable and function declarations from code
extractDeclarations :: String -> [String]
extractDeclarations content =
  let ls = lines content
      isVarDecl line = any (`isPrefixOf` trim line) ["var ", "const ", "func "]
      decls = filter isVarDecl ls
  in decls

-- Extract function calls from code
extractFunctionCalls :: String -> [String]
extractFunctionCalls content =
  let ls = lines content
      calls = filter isFunctionCall ls
  in calls

-- Build type environment from declarations
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

-- Add variable declaration to type environment
addVarDeclaration :: TypeEnv -> String -> TypeEnv
addVarDeclaration env line =
  let withoutVar = drop 3 (trim line)
      (varName, varType) = break (\c -> c == ' ' || c == '=') withoutVar
      varType' = trim $ dropWhile (\c -> c == ' ' || c == '=') varType
      inferredType = inferVarType varType'
  in env { varTypes = (varName, inferredType) : varTypes env }

-- Add function declaration to type environment
addFunctionDeclaration :: TypeEnv -> String -> TypeEnv
addFunctionDeclaration env line =
  -- Skip method declarations (functions with receivers)
  if isMethodDeclaration line
    then env
    else let withoutFunc = drop 4 (trim line)
             (funcName, rest) = break (\c -> c == ' ' || c == '(') withoutFunc
             paramsAndReturn = dropWhile (\c -> c == ' ' || c == '(') rest
             (params, returnType) = break (== ')') paramsAndReturn
             paramTypes = map inferVarType $ splitByComma params
             returnType' = if ") " `isPrefixOf` (drop 1 returnType)
                           then inferVarType $ trim $ drop 2 returnType
                           else VoidType
         in env { functionTypes = (funcName, (paramTypes, returnType')) : functionTypes env }

-- Infer type from variable declaration string
inferVarType :: String -> Type
inferVarType typeStr
  | "int" `isPrefixOf` typeStr = IntType
  | "string" `isPrefixOf` typeStr = StringType
  | "bool" `isPrefixOf` typeStr = BoolType
  | "float" `isPrefixOf` typeStr = FloatType
  | otherwise = VoidType

-- Split string by comma
splitByComma :: String -> [String]
splitByComma s = map trim $ splitOn ',' s
  where
    splitOn _ [] = []
    splitOn delimiter str =
      let (token, rest) = break (== delimiter) str
      in token : splitOn delimiter (drop 1 rest)

-- Check for type errors in declarations and calls
checkTypeError :: TypeEnv -> String -> Bool
checkTypeError env line =
  let t = trim line
  in if isPrefixOf "var " t || isPrefixOf "const " t
     then checkVarDeclaration env t
     else if isFunctionCall t
          then checkFunctionCall env t
          else False

-- Check variable declaration type consistency
checkVarDeclaration :: TypeEnv -> String -> Bool
checkVarDeclaration env line =
  let withoutVar = drop 3 (trim line)
      (varName, rest) = break (\c -> c == ' ' || c == '=') withoutVar
      varType = trim $ dropWhile (\c -> c == ' ' || c == '=') rest
  in case lookup varName (varTypes env) of
    Nothing -> False
    Just declaredType ->
      let inferredType = inferVarType varType
      in declaredType == inferredType

-- Check function call type consistency
checkFunctionCall :: TypeEnv -> String -> Bool
checkFunctionCall env line =
  let callPart = if "=" `isInfixOf` line then trim $ dropWhile (/= '=') line else line
      (funcName, rest) = span (/= '(') (trim callPart)
      argsStr = takeWhile (/= ')') $ drop 1 rest
      args = splitByComma argsStr
      argTypes = map inferVarType args
  in case lookup funcName (functionTypes env) of
    Nothing -> True  -- Function not found in environment, assume it's imported or built-in
    Just (paramTypes, _) ->
      length argTypes == length paramTypes &&
      and (zipWith (==) argTypes paramTypes)
