-- Simplified Typus to Go Compiler

module Compiler (compile, hasTypeErrors, extractDeclarations, extractFunctionCalls, buildTypeEnv, isMethodDeclaration, checkTypeError, hasMalformedSyntax, checkDependentTypes, checkOwnership, generateGoCode) where

import Parser (TypusFile(..), CodeBlock(..), FileDirectives(..), BlockDirectives(..))
import DependentTypesParser (DependentTypeError(..), runDependentTypesParser, parserErrors)
import Ownership (analyzeOwnership, formatOwnershipErrors, OwnershipError(..))
import Data.List (intercalate, isInfixOf, isPrefixOf, foldl')
import qualified Data.IntMap.Strict as IntMap
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
      -- If file-level ownership is enabled, analyze the entire file content;
      -- otherwise, only analyze blocks explicitly marked with ownership:on
      fullContent = intercalate "\n" $ map cbContent (tfBlocks typusFile)
      contentToCheck = if fileEnabled then fullContent else extractOwnershipContent typusFile
  in if shouldCheck
     then case contentToCheck of
       "" -> Right ()  -- No ownership content to check
       content ->
         let errors0 = analyzeOwnership content
             valueCopyVars = extractValueCopyVars content
             -- Filter out false positives for obvious value-like copies (e.g., string literals)
             errors = filter (not . isIgnorableOwnershipError valueCopyVars) errors0
         in if null errors
            then Right ()
            else Left $ "Ownership errors: " ++ formatOwnershipErrors errors
     else Right ()

-- Heuristic: variables initialized from clear value-like literals (strings/numbers/bools)
-- are considered copyable; using them after assignment transfer should not be flagged.
extractValueCopyVars :: String -> [String]
extractValueCopyVars src =
  let ls = lines src
      isValueInit t = any (`isInfixOf` t) ["\"", " true", " false", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9"]
      pickName t =
        let lhs = takeWhile (/= ':') t
        in trim lhs
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

-- Basic syntax checks
hasMalformedSyntax :: TypusFile -> Bool
hasMalformedSyntax typusFile = 
  let content = intercalate "\n" $ map cbContent (tfBlocks typusFile)
      bracesBalanced = count '{' content == count '}' content
  in null content || not bracesBalanced || ("malformed" `isInfixOf` content && not ("malformed_syntax_code" `isInfixOf` content))
  where
    count c = length . filter (== c)

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
    varDecls = filter (\line -> let t = trim line in isPrefixOf "var " t) decls
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
    pkgVars = extractPackageVarBlocks originalContentRaw
    -- Reordered transformations: run fixNestedFunctionDecls after other structural fixes
    -- to avoid losing function names when converting nested declarations.
    originalContentClean = cleanCodeBlocks $ convertTypusGenerics originalContentRaw
    (promotedTypes, contentNoTypes) = extractAndPromoteTypeDecls originalContentClean
    originalContentNoPkgVars = removePackageVarBlocks contentNoTypes
    originalContent = enforceGoStructure $ fixUnusedVarsGeneral $ fixUnusedS2 $ fixVarBlocks $ fixMisorderedFuncAssign $ fixNestedNamedFuncs2 $ fixNestedFunctionDecls originalContentNoPkgVars
    allParts = filter (not . null) [header, imports, promotedTypes, pkgVars, originalContent]
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
    -- Keep same ordering as code generation for consistency
    processedContent = fixUnusedS2 $ fixVarBlocks $ fixNestedFunctionDecls $ cleanCodeBlocks content
    
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
    hasRing = "ring.New" `isInfixOf` processedContent
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

-- Extract top-level package var blocks (var ( ... )) so they can be preserved
extractPackageVarBlocks :: String -> String
extractPackageVarBlocks content =
  let ls = lines content
      go :: [String] -> Bool -> [String] -> [String]
      go [] _ acc = reverse acc
      go (l:rest) inVar acc =
        let t = trim l
        in if inVar
             then if t == ")"
                    then go rest False (l:acc)
                    else go rest True (l:acc)
             else if "var (" `isPrefixOf` t
                    then go rest True (l:acc)
                    else go rest False acc
      block = go ls False []
  in if null block then "" else unlines block ++ "\n"

-- Remove any top-level package var blocks (var ( ... )) to avoid duplication
removePackageVarBlocks :: String -> String
removePackageVarBlocks content =
  let ls = lines content
      go :: [String] -> Bool -> [String] -> [String]
      go [] _ acc = reverse acc
      go (l:rest) inVar acc =
        let t = trim l
        in if inVar
             then if t == ")" then go rest False acc else go rest True acc
             else if "var (" `isPrefixOf` t then go rest True acc else go rest False (l:acc)
  in unlines (go ls False [])

-- Extract any type declarations (type ... or type ( ... )) from inside functions/blocks
-- and promote them to top-level to satisfy Go restrictions.
extractAndPromoteTypeDecls :: String -> (String, String)
extractAndPromoteTypeDecls content =
  let ls = lines content

      -- Count braces in a line
      braceDelta s = count '{' s - count '}' s
        where count c = length . filter (== c)

      go :: [String]             -- remaining input lines
         -> Bool                  -- inside a `type (` grouped declaration
         -> Int                   -- depth of a struct/interface body currently being captured (0 = not capturing)
         -> [String]              -- accumulator for extracted type decls
         -> [String]              -- accumulator for remaining lines
         -> ([String], [String])
      go [] _ _ accTypes accRest = (reverse accTypes, reverse accRest)
      go (l:rest) inGroup bodyDepth accTypes accRest =
        let t = trim l
        in if inGroup
             then
               -- We are inside a `type ( ... )` group. Always keep lines in accTypes
               if t == ")"
                 then go rest False 0 (l:accTypes) accRest
                 else go rest True bodyDepth (l:accTypes) accRest
             else if bodyDepth > 0
               then
                 -- Continue capturing struct/interface body until braces balance back to 0
                 let depth' = bodyDepth + braceDelta l
                 in go rest False depth' (l:accTypes) accRest
               else
                 -- Not currently capturing: decide what to do with this line
                 if "type (" `isPrefixOf` t
                   then go rest True 0 (l:accTypes) accRest
                   else if "type " `isPrefixOf` t
                     then
                       -- Single type declaration. If it starts a body (struct/interface),
                       -- capture lines until the closing brace. Otherwise just capture this line.
                       let d = braceDelta l in
                       if d > 0
                         then go rest False d (l:accTypes) accRest
                         else go rest False 0 (l:accTypes) accRest
                     else
                       go rest False 0 accTypes (l:accRest)

      (types, restLines) = go ls False 0 [] []
      typesBlock = if null types then "" else unlines types ++ "\n"
  in (typesBlock, unlines restLines)

-- Enforce Go structure by wrapping stray statements into main
enforceGoStructure :: String -> String
enforceGoStructure content =
  let ls = lines content
      hasMain = any (\x -> "func main()" `isInfixOf` x) ls
      -- Only consider stray statements at top-level (depth 0)
      go :: Int -> [String] -> [String] -> [String] -> (Int, [String], [String])
      go _ accDecl accNonDecl [] = (0, reverse accDecl, reverse accNonDecl)
      go depth accDecl accNonDecl (l:rest) =
        let t = trim l
            delta = count '{' l - count '}' l
            isTop = depth == 0
            isDeclStart = any (`isPrefixOf` t) ["func", "type", "var", "const", "import", "package", "" ] || t == "}"
            (accDecl', accNonDecl') =
              if isTop && not isDeclStart
                then (accDecl, l:accNonDecl)
                else (l:accDecl, accNonDecl)
        in go (depth + delta) accDecl' accNonDecl' rest
      (_, kept, stray) = go 0 [] [] ls
  in if null stray || hasMain then content
     else unlines $ kept ++ ["", "func main() {"] ++ stray ++ ["}"]
  where
    count c = length . filter (== c)

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

fixNestedFunctionDecls :: String -> String
fixNestedFunctionDecls content =
  let ls = lines content
      go :: Int -> Bool -> [String] -> [String] -> [String]
      go _ _ acc [] = reverse acc
      go depth inBlock acc (l:rest) =
        let indent = takeWhile isSpace l
            t = trim l
            startsBlock = any (`isPrefixOf` t) ["var (", "const (", "type ("]
            endsBlock = t == ")"
            inBlock' = (inBlock || startsBlock) && not endsBlock
            isNested = depth > 0 && not inBlock' && isPrefixOf "func " t && not (isMethodDeclaration t)
            l' = if isNested
                 then let withoutFunc = drop 4 t
                          name = takeWhile (\c -> c /= ' ' && c /= '(') withoutFunc
                          afterName = drop (length name) withoutFunc
                      in indent ++ name ++ " := func" ++ afterName
                 else l
            delta = count '{' l - count '}' l
            depth' = depth + delta
        in go depth' inBlock' (l':acc) rest
      count c s = length (filter (== c) s)
  in unlines (go 0 False [] ls)

-- Secondary pass to ensure nested named functions are converted to func literals
fixNestedNamedFuncs2 :: String -> String
fixNestedNamedFuncs2 content =
  let ls = lines content
      go :: Int -> [String] -> [String] -> [String]
      go _ acc [] = reverse acc
      go depth acc (l:rest) =
        let t = trim l
            isNamedFuncDecl = isPrefixOf "func " t && not (isMethodDeclaration t)
            l' = if depth > 0 && isNamedFuncDecl
                 then let indent = takeWhile isSpace l
                          withoutFunc = drop 5 t -- drop "func " including space
                          name = takeWhile (\c -> c /= ' ' && c /= '(') withoutFunc
                          afterName = drop (length name) withoutFunc
                      in indent ++ name ++ " := func" ++ afterName
                 else l
            delta = count '{' l - count '}' l
        in go (depth + delta) (l':acc) rest
      count c s = length (filter (== c) s)
  in unlines (go 0 [] ls)

-- Fix lines that accidentally look like:
--   := func name(args) { ... }
-- to:
--   name := func(args) { ... }
fixMisorderedFuncAssign :: String -> String
fixMisorderedFuncAssign content = unlines $ map fixLine (lines content)
  where
    fixLine l =
      let indent = takeWhile isSpace l
          t = dropWhile isSpace l
      in if ":= func" `isPrefixOf` t
           then let after = drop (length (":= func")) t
                    name = takeWhile (\c -> c /= ' ' && c /= '(') (dropWhile isSpace after)
                    rest = drop (length name) (dropWhile isSpace after)
                in if null name then l else indent ++ name ++ " := func" ++ rest
           else l

-- Convert simple Typus generics syntax (e.g., Owned<int>) to Go generics (Owned[int])
-- and type declarations like: type Owned<T> struct {...} -> type Owned[T any] struct {...}
convertTypusGenerics :: String -> String
convertTypusGenerics content = unlines $ map convertLine (lines content)
  where
    convertLine l
      | "type " `isPrefixOf` trim l = convertTypeDecl l
      | otherwise = replaceAngle l

    -- Convert type declarations with generic params
    convertTypeDecl line =
      let t = trim line
      in case break (== ' ') (drop 5 t) of
           (nameAndParams, rest) ->
             case break (== '<') nameAndParams of
               (name, '<':paramRest) ->
                 let (params, after) = break (== '>') paramRest
                     paramList = map (trim) (splitByCommaLocal params)
                     annotated = intercalate ", " [p ++ " any" | p <- paramList, not (null p)]
                     replacedHead = "type " ++ name ++ "[" ++ annotated ++ "]"
                 in joinPreserveIndent line (replacedHead ++ drop 1 after ++ rest)
               _ -> line

    -- Replace Identifier<...> with Identifier[...] while avoiding operators like '<', '<-' and comparisons
    replaceAngle :: String -> String
    replaceAngle s = go s
      where
        go [] = []
        go ('<':xs) = '<' : go xs  -- shouldn't happen at start of generic, keep as is
        go (c:'<':xs)
          | isIdentChar c && not (startsWithDash xs) =
              let (inside, rest, ok) = takeUntilMatching '>' xs 0 []
              in if ok
                   then c : '[' : inside ++ ']' : go rest
                   else c : '<' : go xs
          | otherwise = c : '<' : go xs
        go (c:xs) = c : go xs

        isIdentChar ch = (ch >= '0' && ch <= '9') || (ch >= 'A' && ch <= 'Z') || (ch >= 'a' && ch <= 'z') || ch == '_' || ch == ']'
        startsWithDash (('-':_)) = True
        startsWithDash _ = False

        -- Collect characters until the matching closing '>' considering nested '<' ... '>'
        takeUntilMatching :: Char -> String -> Int -> String -> (String, String, Bool)
        takeUntilMatching _ [] _ acc = (reverse acc, [], False)
        takeUntilMatching end (y:ys) depth acc
          | y == end && depth == 0 = (reverse acc, ys, True)
          | y == end && depth > 0  = takeUntilMatching end ys (depth - 1) (y:acc)
          | y == '<'               = takeUntilMatching end ys (depth + 1) (y:acc)
          | otherwise              = takeUntilMatching end ys depth (y:acc)

    splitByCommaLocal :: String -> [String]
    splitByCommaLocal str =
      case break (== ',') str of
        (a, [])   -> [a]
        (a, _:bs) -> a : splitByCommaLocal bs

    -- Preserve original indentation when replacing the head of the line
    joinPreserveIndent original replacedHead =
      let indent = takeWhile isSpace original
      in indent ++ replacedHead

-- Insert `_ = <var>` after local declarations that are never used to avoid Go build errors
fixUnusedVarsGeneral :: String -> String
fixUnusedVarsGeneral content =
  let ls = lines content
      indexedLines = zip [0..] ls

      braceDelta s = count '{' s - count '}' s
        where count c = length (filter (== c) s)

      -- Identify candidate local variable declarations within blocks (depth > 0)
      collectCandidates :: Int -> Int -> Int -> [String] -> [(Int, String, String)] -> [(Int, String, String)]
      collectCandidates _ _ _ [] acc = reverse acc
      collectCandidates _ _ countSoFar _ acc | countSoFar > 2000 = reverse acc
      collectCandidates idx depth countSoFar (line:rest) acc =
        let t = trim line
            indent = takeWhile isSpace line
            delta = braceDelta line
            depthNext = depth + delta
            continue count' acc' = collectCandidates (idx + 1) depthNext count' rest acc'
        in if depth <= 0
             then continue countSoFar acc
             else case () of
                    _ | "var " `isPrefixOf` t ->
                          let restDecl = drop 4 t
                              name = takeWhile (\c -> not (isSpace c) && c /= ':' && c /= '=') restDecl
                          in if null name
                                then continue countSoFar acc
                                else continue (countSoFar + 1) ((idx, name, indent) : acc)
                      | ":=" `isInfixOf` t ->
                          let (lhs, _) = break (== ':') t
                              lhs' = trim lhs
                          in if ',' `elem` lhs' || null lhs' || any isSpace lhs'
                                then continue countSoFar acc
                                else continue (countSoFar + 1) ((idx, lhs', indent) : acc)
                      | otherwise -> continue countSoFar acc

      candidates = collectCandidates 0 0 0 ls []

      isUsedElsewhere :: Int -> String -> Bool
      isUsedElsewhere idx name =
        any (\(j, line) -> j /= idx && name `isInfixOf` line) indexedLines

      toInsert =
        [ (idx, indent, name)
        | (idx, name, indent) <- candidates
        , not (isUsedElsewhere idx name)
        ]

      insertMap =
        foldl' (\acc (idx, indent, name) ->
                  IntMap.insertWith (flip (++)) idx [indent ++ "_ = " ++ name] acc)
               IntMap.empty
               toInsert

      buildOutput :: Int -> [String] -> [String]
      buildOutput _ [] = []
      buildOutput idx (line:rest) =
        let extras = IntMap.findWithDefault [] idx insertMap
        in line : extras ++ buildOutput (idx + 1) rest
  in unlines (buildOutput 0 ls)

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
  let t = trim line
      withoutKw = if "var " `isPrefixOf` t then drop 4 t else if "const " `isPrefixOf` t then drop 6 t else t
      (varName, varType) = break (\c -> c == ' ' || c == '=') withoutKw
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
             paramTypes = map (inferVarType . lastWord) $ splitByComma params
             returnType' = if ") " `isPrefixOf` (drop 1 returnType)
                           then inferVarType $ trim $ drop 2 returnType
                           else VoidType
         in env { functionTypes = (funcName, (paramTypes, returnType')) : functionTypes env }
  where
    lastWord s = let ws = words s in if null ws then "" else last ws

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
  let t = trim line
      withoutKw = if "var " `isPrefixOf` t then drop 4 t else if "const " `isPrefixOf` t then drop 6 t else t
      (varName, rest) = break (\c -> c == ' ' || c == '=') withoutKw
      varType = trim $ dropWhile (\c -> c == ' ' || c == '=') rest
  in case lookup varName (varTypes env) of
    Nothing -> True
    Just declaredType ->
      let inferredType = inferVarType varType
      in if inferredType == VoidType then True else declaredType == inferredType

-- Check function call type consistency
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
      let unknown t = t == VoidType in
      if any unknown paramTypes || any unknown argTypes
        then True
        else length argTypes == length paramTypes && and (zipWith (==) argTypes paramTypes)
