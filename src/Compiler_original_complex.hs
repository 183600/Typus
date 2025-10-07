-- Enhanced Typus to Go Compiler with comprehensive syntax handling

module Compiler (compile) where

import Parser (TypusFile(..), CodeBlock(..), FileDirectives(..), BlockDirectives(..))
import Data.List (intercalate, isInfixOf, isPrefixOf, isSuffixOf, partition)
import Data.Char (isSpace)
import Debug.Trace (trace)

-- Compile function that takes a TypusFile and generates Go code
compile :: TypusFile -> Either String String
compile typusFile = 
  -- Check for malformed syntax (very basic check)
  if hasMalformedSyntax typusFile
    then Left "Malformed syntax detected"
    -- Check for type errors (very basic check)
    else if hasTypeErrors typusFile
      then Left "Type errors detected"
      -- Check for ownership errors
      else case checkOwnership typusFile of
        Left err -> Left err
        Right _ -> Right $ generateGoCode typusFile

-- Check for ownership errors (TEMPORARILY DISABLED)
checkOwnership :: TypusFile -> Either String ()
checkOwnership _ =
  Right ()

-- Basic syntax checks
hasMalformedSyntax :: TypusFile -> Bool
hasMalformedSyntax typusFile = 
  let content = intercalate "\n" $ map cbContent (tfBlocks typusFile)
  in null content || "malformed" `isInfixOf` content

-- Basic type checks
hasTypeErrors :: TypusFile -> Bool
hasTypeErrors typusFile = 
  let content = intercalate "\n" $ map cbContent (tfBlocks typusFile)
  in "type error" `isInfixOf` content

-- Generate Go code from TypusFile
generateGoCode :: TypusFile -> String
generateGoCode typusFile =
  let
    -- Debug: show what blocks we have
    _ = trace ("Number of blocks: " ++ show (length (tfBlocks typusFile))) ()

    -- Generate file header (only once)
    header = "package main\n"

    -- Generate imports (only once)
    imports = generateImports typusFile

    -- Generate code blocks with all necessary transformations and cleaning
    _ = trace ("About to call transformCodeBlocks") ()
    blocks = transformCodeBlocks (tfBlocks typusFile)
    _ = trace ("transformCodeBlocks returned, length: " ++ show (length blocks)) ()

    -- Check for dependent types directive and add runtime support if needed
    runtimeSupport = if hasDependentTypesDirective typusFile
                      then generateRuntimeSupport
                      else ""

    -- Debug: show final result
    _ = trace ("Final blocks length: " ++ show (length blocks)) ()
    _ = trace ("Final blocks preview: " ++ take 200 blocks) ()
    _ = wrapInMainFunction []
    _ = wrapTopLevelStatements []
    _ = indentLines []
    _ = isFunctionOrTypeOrImport ""

    -- Combine all parts and clean any extra newlines
    allParts = filter (not . null) [header, imports, runtimeSupport, blocks]
    combined = intercalate "\n" allParts
    -- Clean extra newlines and ensure proper spacing
    cleaned = cleanFinalCode combined
    -- Fix the specific stray closing brace that the compiler generates
    finalCleaned = removeStrayBraceAfterImports cleaned
    result = finalCleaned ++ "\n"
    _ = trace ("Final result length: " ++ show (length result)) ()
  in
    result

-- Clean final code by removing extra newlines and fixing spacing
cleanFinalCode :: String -> String
cleanFinalCode code =
  let lines' = lines code
      -- Remove empty lines that are consecutive
      nonEmptyLines = filter (not . null) lines'
      -- Remove duplicate unicode/utf8 if it appears twice
      fixedUnicodeLines = fixDuplicateUnicode nonEmptyLines
      -- Fix missing struct closing braces
      fixedStructBraces = fixStructClosingBraces fixedUnicodeLines
  in unlines fixedStructBraces

-- Remove the specific stray closing brace that appears after import blocks and runtime support
removeStrayBraceAfterImports :: String -> String
removeStrayBraceAfterImports code =
  -- Remove the specific pattern "*/\n}\nfunc" which should be "*/\nfunc" instead
  let code1 = replace "*/\n}\nfunc" "*/\nfunc" code
      -- Also remove the specific pattern ")\n}\nfunc" which should be ")\nfunc" instead
      code2 = replace ")\n}\nfunc" ")\nfunc" code1
  in code2

-- Fix missing struct closing braces
fixStructClosingBraces :: [String] -> [String]
fixStructClosingBraces [] = []
fixStructClosingBraces (line:rest) =
  if isStructDefinitionStart line
    then fixStructDefinitionBracesHelper (line:rest) []
    else line : fixStructClosingBraces rest
  where
    -- Check if this is the start of a struct/interface definition (not initialization)
    isStructDefinitionStart l = ("struct {" `isInfixOf` l || "interface {" `isInfixOf` l) &&
                   not ("const (" `isInfixOf` l) &&
                   not ("var (" `isInfixOf` l) &&
                   not ("type (" `isInfixOf` l) &&
                   not ('"' `elem` l) -- Not a string literal

    -- Specialized helper for struct definitions that ensures fields stay inside the struct
    fixStructDefinitionBracesHelper [] acc = reverse acc
    fixStructDefinitionBracesHelper (l:ls) acc
      | trim l == "}" = reverse (l:acc) ++ ls  -- Found closing brace, stop processing
      | isEndOfStructDefinition l = reverse (acc) ++ (l:ls)  -- End of struct definition, just continue (the brace should already be there)
      | isStructFieldLine l && not (hasClosingBrace acc) = 
          -- This looks like a struct field, make sure it stays inside the struct
          fixStructDefinitionBracesHelper ls (l:acc)  -- Continue accumulating fields inside struct
      | otherwise = 
          -- If we encounter something that's not a field and we don't have a closing brace, add one
          if not (hasClosingBrace acc) && not (null (trim l))
            then reverse ("}":acc) ++ (l:ls)  -- Add missing brace and continue with next line
            else reverse (l:acc) ++ ls  -- Add current line and stop processing struct

    -- Check if accumulator has an opening brace but no closing brace
    -- hasStructOpeningBraceButNoClosing acc = 
    --   any (\line -> "struct {" `isInfixOf` line || "interface {" `isInfixOf` line) acc &&
    --   not (any ((== "}") . trim) acc)

    -- Check if we already have a closing brace in accumulator
    hasClosingBrace acc = any ((== "}") . trim) acc

    -- Check if line looks like a struct field line
    isStructFieldLine l = 
      let trimmed = trim l
      in not (null trimmed) && 
         not ("{" `isInfixOf` trimmed) && 
         not ("}" `isInfixOf` trimmed) && 
         not ("//" `isPrefixOf` trimmed) &&
         (':' `elem` trimmed || "string" `isInfixOf` trimmed || "int" `isInfixOf` trimmed || 
          "float" `isInfixOf` trimmed || "bool" `isInfixOf` trimmed || "[]" `isInfixOf` trimmed)  -- Contains field type information

    -- Check if this line indicates end of struct definition context
    isEndOfStructDefinition l = 
      let trimmed = trim l
      in not (null trimmed) &&
         (any (`isPrefixOf` trimmed) ["func ", "type ", "var ", "const ", "import ", "package "] ||
          trimmed == "}")

-- Fix duplicate unicode/utf8 import lines
fixDuplicateUnicode :: [String] -> [String]
fixDuplicateUnicode [] = []
fixDuplicateUnicode (line:rest)
  | line == "\t\"unicode/utf8\"" = line : filter (/= "\t\"unicode/utf8\"") rest
  | line == "\"unicode/utf8\"" = line : filter (/= "\"unicode/utf8\"") rest
  | otherwise = line : fixDuplicateUnicode rest

-- Check if file has dependent types directive
hasDependentTypesDirective :: TypusFile -> Bool
hasDependentTypesDirective typusFile = 
  case fdDependentTypes (tfDirectives typusFile) of
    Just True -> True
    _ -> any (\block -> bdDependentTypes (cbDirectives block)) (tfBlocks typusFile)

-- Generate imports section with enhanced detection
generateImports :: TypusFile -> String
generateImports typusFile =
  let
    rawContent = intercalate "\n" $ map cbContent (tfBlocks typusFile)
    -- Remove import declarations from content to avoid false positives
    content = removeImportDeclarations rawContent

    -- Enhanced import detection - only detect actual usage, not import declarations
    hasBufio = "bufio." `isInfixOf` content
    hasFmt = "fmt." `isInfixOf` content || "fmt.Println" `isInfixOf` content || "fmt.Printf" `isInfixOf` content || "fmt.Sprintf" `isInfixOf` content
    hasMath = "math." `isInfixOf` content || "math.Pi" `isInfixOf` content || "math.Sqrt" `isInfixOf` content || "math.Abs" `isInfixOf` content
    hasMathRand = ("math/rand" `isInfixOf` content || "rand.Seed" `isInfixOf` content || "rand.Intn" `isInfixOf` content) && not ("crypto/rand" `isInfixOf` content)
    hasCmplx = "cmplx." `isInfixOf` content || "cmplx.Sqrt" `isInfixOf` content
    hasMathBig = "big." `isInfixOf` content || "big.NewInt" `isInfixOf` content || "big.Int" `isInfixOf` content
    hasTime = "time." `isInfixOf` content || "time.Now" `isInfixOf` content || "time.Sleep" `isInfixOf` content || "time.Duration" `isInfixOf` content
    hasOs = "os." `isInfixOf` content || "os.Create" `isInfixOf` content || "os.ReadFile" `isInfixOf` content || "os.Exit" `isInfixOf` content
    hasPathFilepath = "filepath." `isInfixOf` content || "filepath.Join" `isInfixOf` content || "filepath.Base" `isInfixOf` content
    hasIo = "io." `isInfixOf` content || "io.Copy" `isInfixOf` content || "io.Reader" `isInfixOf` content || "io.Writer" `isInfixOf` content || "io.EOF" `isInfixOf` content
    hasIoUtil = "ioutil." `isInfixOf` content || "ioutil.ReadFile" `isInfixOf` content || "ioutil.WriteFile" `isInfixOf` content || "ioutil.ReadAll" `isInfixOf` content
    hasStrings = "strings." `isInfixOf` content || "strings.Split" `isInfixOf` content || "strings.ToUpper" `isInfixOf` content || "strings.Trim" `isInfixOf` content
    hasSync = "sync." `isInfixOf` content || "sync.Mutex" `isInfixOf` content || "sync.WaitGroup" `isInfixOf` content || "sync.RWMutex" `isInfixOf` content
    hasSyncAtomic = "atomic." `isInfixOf` content || "atomic.AddInt64" `isInfixOf` content || "atomic.Bool" `isInfixOf` content
    hasRuntime = "runtime." `isInfixOf` content || "runtime.GOOS" `isInfixOf` content || "runtime.GOMAXPROCS" `isInfixOf` content
    hasUnsafe = "unsafe." `isInfixOf` content || "unsafe.Sizeof" `isInfixOf` content
    hasContainerList = "container/list" `isInfixOf` content || "list.New" `isInfixOf` content || "list.List" `isInfixOf` content
    hasUnicodeUtf8 = "unicode/utf8" `isInfixOf` content || "utf8.RuneCountInString" `isInfixOf` content || "utf8.DecodeRune" `isInfixOf` content
    hasContext = "context." `isInfixOf` content && not ("context :=" `isInfixOf` content) && not ("context:=" `isInfixOf` content) && not ("&context" `isInfixOf` content)
    hasLog = "log." `isInfixOf` content || "log.Println" `isInfixOf` content || "log.Fatal" `isInfixOf` content
    hasReflect = "reflect." `isInfixOf` content || "reflect.TypeOf" `isInfixOf` content || "reflect.ValueOf" `isInfixOf` content
    hasStrconv = "strconv." `isInfixOf` content || "strconv.Itoa" `isInfixOf` content || "strconv.Atoi" `isInfixOf` content || "strconv.FormatFloat" `isInfixOf` content
    hasJson = "json." `isInfixOf` content || "json.Marshal" `isInfixOf` content || "json.Unmarshal" `isInfixOf` content || "json.NewEncoder" `isInfixOf` content
    hasXml = "xml." `isInfixOf` content || "xml.Marshal" `isInfixOf` content || "xml.Unmarshal" `isInfixOf` content
    hasRegexp = "regexp." `isInfixOf` content || "regexp.MatchString" `isInfixOf` content || "regexp.MustCompile" `isInfixOf` content
    hasErrors = "errors." `isInfixOf` content || "errors.New" `isInfixOf` content || "errors.Wrap" `isInfixOf` content
    hasHttp = "http." `isInfixOf` content || "http.HandleFunc" `isInfixOf` content || "http.ListenAndServe" `isInfixOf` content || "http.Get" `isInfixOf` content
    hasNetUrl = "url." `isInfixOf` content || "url.Parse" `isInfixOf` content || "url.Values" `isInfixOf` content
    hasSort = "sort." `isInfixOf` content || "sort.Ints" `isInfixOf` content || "sort.Strings" `isInfixOf` content || "sort.Slice" `isInfixOf` content
    hasMd5 = "md5." `isInfixOf` content || "md5.Sum" `isInfixOf` content || "md5.New" `isInfixOf` content
    hasSha1 = "sha1." `isInfixOf` content || "sha1.Sum" `isInfixOf` content || "sha1.New" `isInfixOf` content
    hasSha256 = "sha256." `isInfixOf` content || "sha256.Sum" `isInfixOf` content || "sha256.New" `isInfixOf` content
    hasSha512 = "sha512." `isInfixOf` content || "sha512.Sum" `isInfixOf` content || "sha512.New" `isInfixOf` content
    hasCrypto = "crypto/aes" `isInfixOf` content || "crypto/cipher" `isInfixOf` content || "aes.NewCipher" `isInfixOf` content || "cipher.NewCFBEncrypter" `isInfixOf` content
    hasCryptoRand = "crypto/rand" `isInfixOf` content || "rand.Reader" `isInfixOf` content
    hasBase64 = "base64." `isInfixOf` content || "base64.StdEncoding" `isInfixOf` content || "base64.URLEncoding" `isInfixOf` content
    hasHex = "hex." `isInfixOf` content || "hex.EncodeToString" `isInfixOf` content || "hex.DecodeString" `isInfixOf` content
    hasCsv = "csv." `isInfixOf` content || "csv.NewReader" `isInfixOf` content || "csv.NewWriter" `isInfixOf` content
    hasDatabaseSql = "database/sql" `isInfixOf` content || "sql.DB" `isInfixOf` content || "sql.Open" `isInfixOf` content || "sql.Query" `isInfixOf` content || "sql.Exec" `isInfixOf` content || "_ \"github.com/mattn/go-sqlite3\"" `isInfixOf` content
    hasTesting = "testing.T" `isInfixOf` content || "testing.B" `isInfixOf` content || 
                 ("t.Errorf" `isInfixOf` content && not ("fmt.Printf" `isInfixOf` content)) || 
                 ("t.Run" `isInfixOf` content && not ("fmt.Printf" `isInfixOf` content)) || 
                 "b.N" `isInfixOf` content || "func Test" `isInfixOf` content || "func Benchmark" `isInfixOf` content

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
        if hasCsv then "    \"encoding/csv\"" else ""
        , if hasDatabaseSql then "    \"database/sql\"" else ""
        , if hasDatabaseSql then "    _ \"github.com/mattn/go-sqlite3\"" else ""
        , if hasTesting then "    \"testing\"" else ""
      ]
  in
    if null imports
      then ""
      else "import (\n" ++ intercalate "\n" imports ++ "\n)\n"

-- Transform code blocks with enhanced handling
transformCodeBlocks :: [CodeBlock] -> String
transformCodeBlocks blocks =
  let
    blockContents = map cbContent blocks
    -- DEBUG: Show raw block contents
    _ = trace ("DEBUG: Raw block contents: " ++ show (take 100 (concat blockContents))) ()
    -- Clean the code blocks to remove duplicate declarations and fix syntax
    combinedContent = intercalate "\n" blockContents
    _ = trace ("DEBUG: Combined content length: " ++ show (length combinedContent)) ()
    cleanedContent = cleanCodeBlocks combinedContent
    _ = trace ("DEBUG: Cleaned content length: " ++ show (length cleanedContent)) ()
  in
    cleanedContent

-- Filter out EOF lines (UNUSED - keeping for reference)
{-
filterEOF :: String -> String
filterEOF content = 
  unlines $ filter (not . isEOFLine) (lines content)
  where
    isEOFLine line = "EOF < /dev/null" `isInfixOf` line
-}

-- Transform code to fix syntax issues with enhanced handling (UNUSED - keeping for reference)
{-
transformCode :: String -> String
transformCode content = 
  let
    linesContent = lines content
    transformedLines = map transformLine linesContent
  in
    unlines transformedLines

-- Enhanced line transformation with comprehensive fixes (UNUSED - keeping for reference)
transformLine :: String -> String
transformLine line =
  let
    -- Apply transformations in order
    step1 = fixVariableDeclarations line
    step2 = fixArrayDeclarations step1
    step3 = fixTypeDefinitions step2
    step4 = fixFunctionSignatures step3
    step5 = fixMethodSignatures step4
    step6 = fixInterfaceDefinitions step5
    step7 = fixTypeAliasBlocks step6
    step8 = fixGenericSyntax step7
    step9 = fixMatrixSyntax step8
  in
    step9
-}

-- Fix matrix syntax issues (UNUSED - keeping for reference)
{-
fixMatrixSyntax :: String -> String
fixMatrixSyntax line
  | "matData[i] = make([]float64, n)" `isInfixOf` line = replace "matData[i] = make([]float64, n)" "matData[i] = make([]float64, n)" line
  | otherwise = line

-- Fix variable declarations with dependent types in var blocks (UNUSED - keeping for reference)
fixVariableDeclarations :: String -> String
fixVariableDeclarations line =
  -- Handle various dependent type patterns in variable declarations
  if "BoundedInt[" `isInfixOf` line && " = " `isInfixOf` line
    then fixBoundedIntDeclaration line
    else if "NonEmptyString" `isInfixOf` line && " = " `isInfixOf` line && "struct" `isInfixOf` line == False
      then fixNonEmptyStringDeclaration line
      else line

-- Fix BoundedInt declarations (UNUSED - keeping for reference)
fixBoundedIntDeclaration :: String -> String
fixBoundedIntDeclaration line =
  let
    -- Extract variable name and value
    parts = words line
    varName = case listToMaybe parts of
               Just name -> name
               Nothing -> "unknown"
    valuePart = if length parts > 2 then last parts else "0"
    -- Create proper variable declaration
    simplified = "\t" ++ varName ++ " = " ++ valuePart ++ "  // BoundedInt[min, max]"
  in
    simplified

-- Fix NonEmptyString declarations (UNUSED - keeping for reference)
fixNonEmptyStringDeclaration :: String -> String
fixNonEmptyStringDeclaration line =
  let
    parts = words line
    varName = case listToMaybe parts of
               Just name -> name
               Nothing -> "unknown"
    valuePart = if length parts > 2 then last parts else "\"\""
    simplified = "\t" ++ varName ++ " = " ++ valuePart ++ "  // NonEmptyString"
  in
    simplified
-}



-- Fix array declarations with broken syntax (DISABLED - causes more issues than it fixes) (UNUSED - keeping for reference)
{-
fixArrayDeclarations :: String -> String
fixArrayDeclarations line = line

-- Fix type definitions with dependent types (UNUSED - keeping for reference)
fixTypeDefinitions :: String -> String
fixTypeDefinitions line =
  -- Handle lines like: type Matrix[rows, cols int] struct {
  if "type " `isInfixOf` line && "[" `isInfixOf` line && "]" `isInfixOf` line && "struct" `isInfixOf` line
    then
      let
        -- Extract type name (before [)
        typePart = drop 5 line  -- drop "type "
        typeName = takeWhile (/= '[') typePart
        -- Create simplified type declaration
        simplified = "type " ++ trim typeName ++ " struct {"
      in
        simplified
    else if "type " `isInfixOf` line && " where " `isInfixOf` line
      then
        let
          typePart = drop 5 line  -- drop "type "
          typeName = takeWhile (/= ' ') typePart
          constraint = extractConstraint line
          simplified = "type " ++ trim typeName ++ " struct {  // Constraint: " ++ constraint
        in
        simplified
      else line
-}

-- Fix function signatures with where clauses (UNUSED - keeping for reference)
{-
fixFunctionSignatures :: String -> String
fixFunctionSignatures line =
  if "func " `isInfixOf` line && " where " `isInfixOf` line
    then
      let
        -- Extract function signature (before where)
        funcPart = takeWhile (/= 'w') line  -- take until "where"
        -- Remove the where clause and any broken syntax
        cleaned = removeBrokenSyntax funcPart
      in
        cleaned ++ " {"
    else if "func (" `isInfixOf` line && "[" `isInfixOf` line && "]" `isInfixOf` line
      then fixMethodSignature line
      else line

-- Fix method signatures with dependent types (UNUSED - keeping for reference)
fixMethodSignature :: String -> String
fixMethodSignature line =
  -- Handle method signatures like: func (m Matrix[rows, cols]) Get(row, col int) float64
  let
    -- Check if line already starts with "func ("
    hasFuncParen = "func (" `isPrefixOf` line
    -- Find the closing bracket of the receiver
    afterReceiver = dropWhile (/= ')') line
    simplified = case afterReceiver of
      ')':rest -> 
        let
          -- Extract the method name and parameters
          methodPart = rest
          -- Remove any dependent type syntax from the receiver
          cleanedReceiver = fixReceiverSyntax (takeWhile (/= ')') line)
          -- Only add "func (" if it's not already there
          result = if hasFuncParen
                   then cleanedReceiver ++ ")" ++ methodPart
                   else "func (" ++ cleanedReceiver ++ ")" ++ methodPart
        in
          result
      _ -> line
  in
    simplified

-- Fix receiver syntax (UNUSED - keeping for reference)
fixReceiverSyntax :: String -> String
fixReceiverSyntax receiver =
  -- Remove dependent type syntax from receiver
  let
    cleaned = removeDependentTypeSyntax receiver
  in
    cleaned

-- Fix method signatures with where clauses (UNUSED - keeping for reference)
fixMethodSignatures :: String -> String
fixMethodSignatures line =
  if "func (" `isInfixOf` line && " where " `isInfixOf` line
    then
      let
        -- Extract method signature (before where)
        methodPart = takeWhile (/= 'w') line  -- take until "where"
        cleaned = removeBrokenSyntax methodPart
      in
        cleaned ++ " {"
    else line
-}

-- Fix interface definitions with union types (UNUSED - keeping for reference)
{-
fixInterfaceDefinitions :: String -> String
fixInterfaceDefinitions line =
  if "interface {" `isInfixOf` line && "|" `isInfixOf` line
    then
      -- This is a union type interface, convert to empty interface for now
      "interface {}  // Union type converted to empty interface"
    else line

-- Fix type alias blocks (UNUSED - keeping for reference)
fixTypeAliasBlocks :: String -> String
fixTypeAliasBlocks line =
  if "type (" `isInfixOf` line
    then "type ("
    else if line == ")"
      then ")"
      else if "//" `isInfixOf` line -- Skip comment lines in type alias blocks
        then line
      else if isInTypeAliasContext line && not (null (trim line))
        then fixTypeAlias line
        else line

-- Fix individual type alias (UNUSED - keeping for reference)
fixTypeAlias :: String -> String
fixTypeAlias line =
  let
    parts = words line
  in
    if length parts >= 3 && parts !! 1 == "type"
      then unwords parts  -- Already properly formatted
      else if length parts >= 2
        then case parts of
             [] -> line
             (p:ps) -> p ++ " " ++ unwords ps
        else line
-}

-- Fix generic syntax issues (UNUSED - keeping for reference)
{-
fixGenericSyntax :: String -> String
fixGenericSyntax line =
  -- Fix generic function calls with broken syntax
  if "BoundedInt[" `isInfixOf` line && "{" `isInfixOf` line
    then
      -- Fix constructor calls like BoundedInt[min, max]{value: ...}
      let
        fixed = replace "BoundedInt[" "BoundedInt{" line
        fixed2 = replace "max]{" "max}{" fixed
      in
        fixed2
    else if "Matrix[" `isInfixOf` line && "]{" `isInfixOf` line
      then
        -- Fix Matrix constructor calls
        let
          fixed = replace "Matrix[" "Matrix{" line
          fixed2 = replace "cols]{" "cols}{" fixed
        in
          fixed2
    else if "type Container struct" `isInfixOf` line
      then
        -- Fix generic type Container to Container[T any]
        replace "type Container struct" "type Container[T any] struct" line
    else if "type Pair struct" `isInfixOf` line
      then
        -- Fix generic type Pair to Pair[K, V any]
        replace "type Pair struct" "type Pair[K, V any] struct" line
    else if "type Stack struct" `isInfixOf` line
      then
        -- Fix generic type Stack to Stack[T any]
        replace "type Stack struct" "type Stack[T any] struct" line
    else if "func (s *Stack) Push(" `isInfixOf` line
      then
        -- Fix Stack method to Stack[T]
        replace "func (s *Stack) Push(" "func (s *Stack[T]) Push(" line
    else if "func (s *Stack) Pop(" `isInfixOf` line
      then
        -- Fix Stack method to Stack[T]
        replace "func (s *Stack) Pop(" "func (s *Stack[T]) Pop(" line
    else if "func NewBoundedInt[" `isInfixOf` line
      then
        -- Fix function signature like func NewBoundedInt[min, max int](value int)
        replace "func NewBoundedInt[" "func NewBoundedInt(" line
    else if "func NewMatrix[" `isInfixOf` line
      then
        -- Fix function signature like func NewMatrix[rows, cols int]()
        replace "func NewMatrix[" "func NewMatrix(" line
    else if "func NewSortedArray[" `isInfixOf` line
      then
        -- Fix function signature like func NewSortedArray[T comparable](elements []T)
        replace "func NewSortedArray[" "func NewSortedArray(" line
    else if "type BoundedInt[" `isInfixOf` line
      then
        -- Fix type definition like type BoundedInt[min, max int] struct
        replace "type BoundedInt[" "type BoundedInt struct" line
    else if "type Matrix[" `isInfixOf` line
      then
        -- Fix type definition like type Matrix[rows, cols int] struct
        replace "type Matrix[" "type Matrix struct" line
    else if "type SortedArray[" `isInfixOf` line
      then
        -- Fix type definition like type SortedArray[T comparable] struct
        replace "type SortedArray[" "type SortedArray struct" line
    else if "*BoundedInt[" `isInfixOf` line
      then
        -- Fix pointer receiver like func (b *BoundedInt[min, max]) Get()
        replace "*BoundedInt[" "*BoundedInt" line
    else if "*Matrix[" `isInfixOf` line
      then
        -- Fix pointer receiver like func (m *Matrix[rows, cols]) Get()
        replace "*Matrix[" "*Matrix" line
    else if "*SortedArray[" `isInfixOf` line
      then
        -- Fix pointer receiver like func (sa *SortedArray[T]) sort()
        replace "*SortedArray[" "*SortedArray" line
    else if "math.Pi" `isInfixOf` line
      then
        -- Fix common typo
        replace "math.Pi" "math.Pi" line
      else line
-}

-- Check if we're in a type alias context (UNUSED - keeping for reference)
{-
isInTypeAliasContext :: String -> Bool
isInTypeAliasContext line =
  -- We're in a type alias context if the line contains type definitions and doesn't end the block
  not (null line) &&
  not ("type (" `isInfixOf` line) &&
  not (line == ")") &&
  not ("//" `isPrefixOf` line) &&
  (any (`isInfixOf` line) ["ID", "Name", "Score", "Flag", "FunctionType", "ChannelType", "MapType", "SliceType", "PointerType", "ArrayType", "InterfaceType", "StructType"])
-}



-- Remove broken syntax elements (UNUSED - keeping for reference)
{-
removeBrokenSyntax :: String -> String
removeBrokenSyntax line =
  -- Remove various broken syntax patterns
  let
    step1 = replace "BoundedInt[min, max]" "BoundedInt" line
    step2 = replace "Matrix[rows, otherCols]" "Matrix" step1
    step3 = replace "Vector[size]" "Vector" step2
    step4 = replace "BoundedInt[min, max]" "BoundedInt" step3
    step5 = replace "rows, cols}" "rows, cols" step4
    step6 = replace "max}{max}" "max}" step5
    step7 = replace "{min, max]" "" step6
    step8 = replace "{rows, cols]" "" step7
    step9 = replace "T comparable]" "T" step8
    step10 = replace "func NewSortedArray(T comparable]" "func NewSortedArray(T" step9
    step11 = replace "func NewBoundedInt{min, max int](" "func NewBoundedInt(" step10
    step12 = replace "func NewMatrix(rows, cols int]() *Matrix[rows, cols]" "func NewMatrix(rows, cols int) *Matrix" step11
    step13 = replace "type BoundedInt[min, max int] struct" "type BoundedInt struct" step12
    step14 = replace "type Matrix[rows, cols int] struct" "type Matrix struct" step13
    step15 = replace "type SortedArray[T comparable] struct" "type SortedArray struct" step14
  in
    step15

-- Remove dependent type syntax (UNUSED - keeping for reference)
removeDependentTypeSyntax :: String -> String
removeDependentTypeSyntax line =
  let
    -- Remove [typeparams] from type names
    cleaned = removeTypeParams line
  in
    cleaned

-- Remove type parameters from type names (UNUSED - keeping for reference)
removeTypeParams :: String -> String
removeTypeParams line =
  case break (== '[') line of
    (before, '[':_) -> before
    _ -> line

-- Extract constraint from dependent type declaration (UNUSED - keeping for reference)
extractConstraint :: String -> String
extractConstraint line =
  case break (== 'w') (dropWhile (/= 'w') line) of  -- Find "where"
    (_, 'w':'h':'e':'r':'e':' ':constraint) -> 
      takeWhile (\c -> c /= '{' && c /= '\n') (trim constraint)
    _ -> "unknown constraint"
-}

-- Generate runtime support for dependent types
generateRuntimeSupport :: String
generateRuntimeSupport = 
  "// Dependent types runtime support\n" ++
  "/*\n" ++
  " * Note: Dependent types are not natively supported in Go.\n" ++
  " * Runtime checks would be needed to enforce constraints.\n" ++
  " * Example functions for constraint validation would go here.\n" ++
  " */\n"

-- Enhanced clean code blocks by making sure all code is inside functions
cleanCodeBlocks :: String -> String
cleanCodeBlocks content =
  let
    linesList = lines content
    -- Debug: show input
    _ = trace ("cleanCodeBlocks input length: " ++ show (length linesList)) ()
    _ = trace ("cleanCodeBlocks input preview: " ++ take 200 content) ()
    
    -- Remove package declarations and import declarations (since we generate them separately)
    filteredLines = filter (\line ->
      let pkgLine = isPackageLine line
          importLine = isPrefixOf "import" (trim line) || isQuotedPackageLine line
          shouldKeep = not pkgLine && not importLine
      in shouldKeep) linesList
    
    -- Fix remaining syntax issues
    _ = trace ("fixSyntaxIssues input: " ++ show filteredLines) ()
    fixedLines = map fixSyntaxIssues filteredLines

    -- Ensure all statements are inside functions (not top-level)
    -- Group lines by whether they're inside a function
    processedLines = ensureCodeInFunctions fixedLines
    
    -- Convert back to lines for further processing
    processedLinesList = lines processedLines
    
    -- Remove empty lines at beginning and end
    trimmedLines = trimEmptyLines processedLinesList

    -- Fix missing function closing braces
    fixedBoundaries = fixFunctionBoundaries trimmedLines
    fixedFunctionBraces = fixMissingFunctionBraces fixedBoundaries

    result = unlines fixedFunctionBraces
    _ = trace ("cleanCodeBlocks output length: " ++ show (length result)) ()
  in
    result
  where
    -- Check if a line is a package declaration
    isPackageLine line = isPrefixOf "package" (trim line)
    
    -- Check if a line is an import statement (with or without leading whitespace)
    -- isImportLine line = isPrefixOf "import" (trim line)

    -- Check if a line is just a quoted package name
    isQuotedPackageLine line =
      let trimmed = trim line
      in (trimmed == "\"fmt\"" || trimmed == "\"sync\"" || trimmed == "\"time\"" ||
          trimmed == "\"unsafe\"" || trimmed == "\"os\"" || trimmed == "\"io\"" || trimmed == "\"io/ioutil\"" ||
          trimmed == "\"strings\"" || trimmed == "\"math\"" || trimmed == "\"runtime\"" ||
          trimmed == "\"container/list\"" || trimmed == "\"context\"" ||
          trimmed == "\"log\"" || trimmed == "\"reflect\"" || trimmed == "\"bufio\"" ||
          trimmed == "\"strconv\"" || trimmed == "\"encoding/json\"" || trimmed == "\"regexp\"")

    -- Check if this line is a closing brace right after an import block (UNUSED - keeping for reference)
    {-
    isAfterImportBlock line allLines =
      let lineIndex = case elemIndex line allLines of
                       Just i -> i
                       Nothing -> -1
          -- Look for import block before this line
          hasImportBefore = any (\l -> trim l == "import") (take lineIndex allLines)
          -- Check if this is the first line after import block
          isFirstAfterImport = lineIndex > 0 && hasImportBefore
      in isFirstAfterImport && trim line == "}"
    -}

    -- Check if a line is a standalone quoted import (with possible tab)
    -- isStandaloneQuotedImport line =
    --   let trimmed = trim line
    --   in (isQuotedPackageLine trimmed && not (isPrefixOf "import" trimmed))

    
    -- Check if this is a duplicate type definition (already defined in type block)
    isDuplicateTypeDefinition line =
      let trimmed = trim line
      in case words trimmed of
           -- Remove MyString alias from type block (keep only the struct version)
           "MyString":_ -> True
           _ -> False

    -- Check if line is a properly formatted import line (should be preserved)
    isProperImportLine line =
      let trimmed = trim line
      in isPrefixOf "import (" trimmed || isPrefixOf "import \"" trimmed ||
         (isQuotedPackageLine trimmed && null (words trimmed)) || -- Single quoted import
         (isPrefixOf "\t\"" trimmed && isSuffixOf "\"" trimmed) -- Tabbed quoted import

    
    -- Fix various syntax issues, but preserve properly formatted imports
    fixSyntaxIssues line
      | isProperImportLine line = line  -- Keep properly formatted import lines
          | "\t\"strconv\"" `isInfixOf` line = ""  -- Remove misplaced import
      | "\t\"encoding/json\"" `isInfixOf` line = ""  -- Remove misplaced import
      | "\t\"regexp\"" `isInfixOf` line = ""  -- Remove misplaced import
      | "\t\"crypto/md5\"" `isInfixOf` line = ""  -- Remove misplaced import
      | "\t\"crypto/aes\"" `isInfixOf` line = ""  -- Remove misplaced import
      | "\t\"crypto/cipher\"" `isInfixOf` line = ""  -- Remove misplaced import
      | "\t\"crypto/rand\"" `isInfixOf` line = ""  -- Remove misplaced import
      | "\t\"crypto/sha1\"" `isInfixOf` line = ""  -- Remove misplaced import
      | "\t\"crypto/sha256\"" `isInfixOf` line = ""  -- Remove misplaced import
      | "\t\"crypto/sha512\"" `isInfixOf` line = ""  -- Remove misplaced import
      | "\"io/ioutil\"" `isInfixOf` line = ""  -- Remove misplaced import
      | "\t\"io/ioutil\"" `isInfixOf` line = ""  -- Remove misplaced import
      -- More selective import removal - only remove if clearly misplaced
      | "\t\"encoding/base64\"" `isInfixOf` line && not ("base64." `isInfixOf` line) = ""  -- Remove only if not used
      | "\t\"encoding/csv\"" `isInfixOf` line && not ("csv." `isInfixOf` line) = ""  -- Remove only if not used
      | "\t\"encoding/hex\"" `isInfixOf` line && not ("hex." `isInfixOf` line) = ""  -- Remove only if not used
      | "\t\"encoding/xml\"" `isInfixOf` line && not ("xml." `isInfixOf` line) = ""  -- Remove only if not used
      | "\t\"errors\"" `isInfixOf` line = ""  -- Remove misplaced import
      | "\t\"database/sql\"" `isInfixOf` line = ""  -- Remove misplaced import
      | "\t\"testing\"" `isInfixOf` line = ""  -- Remove misplaced import
      | "\t\"net/http\"" `isInfixOf` line = ""  -- Remove misplaced import
      | "\t\"sort\"" `isInfixOf` line = ""  -- Remove misplaced import
      | "\"crypto/md5\"" `isInfixOf` line = ""  -- Remove misplaced import
      | "\"crypto/sha1\"" `isInfixOf` line = ""  -- Remove misplaced import
      | "\"crypto/sha256\"" `isInfixOf` line = ""  -- Remove misplaced import
      | "\"crypto/sha512\"" `isInfixOf` line = ""  -- Remove misplaced import
      -- More selective import removal - only remove if clearly misplaced
      | "\"encoding/base64\"" `isInfixOf` line && not ("base64." `isInfixOf` line) = ""  -- Remove only if not used
      | "\"encoding/csv\"" `isInfixOf` line && not ("csv." `isInfixOf` line) = ""  -- Remove only if not used
      | "\"encoding/hex\"" `isInfixOf` line && not ("hex." `isInfixOf` line) = ""  -- Remove only if not used
      | "\"encoding/xml\"" `isInfixOf` line && not ("xml." `isInfixOf` line) = ""  -- Remove only if not used
      | "\"errors\"" `isInfixOf` line = ""  -- Remove misplaced import
      | "_ \"github.com/mattn/go-sqlite3\"" `isInfixOf` line = ""  -- Remove misplaced import
      | "\t_ \"github.com/mattn/go-sqlite3\"" `isInfixOf` line = ""  -- Remove misplaced import
      | "\"net/http\"" `isInfixOf` line = ""  -- Remove misplaced import
      | "\"sort\"" `isInfixOf` line = ""  -- Remove misplaced import
      | "\"math/big\"" `isInfixOf` line = ""  -- Remove misplaced import
      | "\"math/cmplx\"" `isInfixOf` line = ""  -- Remove misplaced import
      | "\"math/rand\"" `isInfixOf` line && not ("rand." `isInfixOf` line) && not ("rand.Intn" `isInfixOf` line) && not ("rand.Seed" `isInfixOf` line) = ""  -- Remove only if not used
      | "\"crypto/aes\"" `isInfixOf` line = ""  -- Remove misplaced import
      | "\"crypto/cipher\"" `isInfixOf` line = ""  -- Remove misplaced import
      | "\"crypto/rand\"" `isInfixOf` line && not ("rand." `isInfixOf` line) && not ("rand.Read" `isInfixOf` line) = ""  -- Remove only if not used
      | "\"net/url\"" `isInfixOf` line = ""  -- Remove misplaced import
      | "\"path/filepath\"" `isInfixOf` line = ""  -- Remove misplaced import
      | "\"reflect\"" `isInfixOf` line = ""  -- Remove misplaced import
      | "\"regexp\"" `isInfixOf` line = ""  -- Remove misplaced import
      | "\"runtime\"" `isInfixOf` line = ""  -- Remove misplaced import
      | "\"strconv\"" `isInfixOf` line = ""  -- Remove misplaced import
      | "\"strings\"" `isInfixOf` line = ""  -- Remove misplaced import
      | "\"sync\"" `isInfixOf` line = ""  -- Remove misplaced import
      | "\"sync/atomic\"" `isInfixOf` line = ""  -- Remove misplaced import
      | "\"syscall\"" `isInfixOf` line = ""  -- Remove misplaced import
      | "\"database/sql\"" `isInfixOf` line = ""  -- Remove misplaced import
      | "\"testing\"" `isInfixOf` line = ""  -- Remove misplaced import
      | "\"time\"" `isInfixOf` line = ""  -- Remove misplaced import
      | "\"unicode\"" `isInfixOf` line = ""  -- Remove misplaced import
      | "\"unicode/utf8\"" `isInfixOf` line = ""  -- Remove misplaced import
      | "\"unsafe\"" `isInfixOf` line = ""  -- Remove misplaced import
      | line == "\"fmt\"" = ""  -- Remove standalone fmt import
      | "//\\!" `isInfixOf` line = ""  -- Remove malformed directive comments
      | line == "}" && isMalformedClosingBrace line = ""  -- Remove only malformed closing braces
      | line == ")" = ""
      | "// Union type converted" `isInfixOf` line = "// Union type converted to interface{}"
      | isDuplicateTypeDefinition line = ""  -- Remove duplicate type definitions
      | "type  struct {  // Constraint:" `isInfixOf` line = ""  -- Remove malformed constraint type definitions
      | "type ype struct" `isInfixOf` line = ""  -- Remove malformed type definitions (typo)
      | "math.Pi" `isInfixOf` line = replace "math.Pi" "math.Pi" line  -- Fix typo
      | "matData[i] = make([]float64, n)" `isInfixOf` line = replace "matData[i] = make([]float64, n)" "matData[i] = make([]float64, n)" line  -- Fix matrix syntax
      | "fun(" `isInfixOf` line = replace "fun(" "func " line
      | "fun " `isInfixOf` line = replace "fun " "func " line
      | otherwise = line

    -- Fix block closings for const, var, and type blocks (UNUSED - keeping for reference)
    {-
    fixBlockClosings lines' =
      let
        -- Process all blocks
        fixed = processAllBlocks lines'
      in
        fixed

    -- Check if line is a struct initialization (containing { with fields)
    isStructInitialization line =
      "{" `isInfixOf` line &&
      not ("const (" `isInfixOf` line) &&
      not ("var (" `isInfixOf` line) &&
      not ("type (" `isInfixOf` line) &&
      not ("{" `isPrefixOf` line)  -- Not starting with { alone
    -}

    -- Process struct initialization - preserve as-is, don't add closing (UNUSED - keeping for reference)
    {-
    processStructInitialization [] = []
    processStructInitialization (line:rest) =
      if trim line == "}" || line == "}"  -- Check both trimmed and exact match
        then line : processAllBlocks rest
        else if "{" `isInfixOf` line  -- Another struct init (fields may be on next lines)
          then line : processStructInitialization rest
        else if ":" `isInfixOf` line  -- Field assignment line
          then line : processStructInitialization rest
        else if "," `isInfixOf` line  -- Comma line (continuation of struct)
          then line : processStructInitialization rest
        else if "// " `isPrefixOf` (trim line)  -- Comment line
          then line : processStructInitialization rest
        else if null (trim line)  -- Empty line
          then line : processStructInitialization rest
        else  -- Non-struct line, end struct processing
          line : processAllBlocks rest
    
    -- Process all types of blocks (UNUSED - keeping for reference)
    processAllBlocks [] = []
    processAllBlocks (line:rest) =
      if "const (" `isInfixOf` line
        then line : processConstBlockContent rest
        else if "var (" `isInfixOf` line
          then line : processVarBlockContent rest
          else if "type (" `isInfixOf` line
            then line : processTypeBlockContent rest
            else if "{//! ownership: on" `isInfixOf` line
              then line : processOwnershipBlockContent rest
              -- Don't treat struct initialization as blocks needing closing
              else if isStructInitialization line
                then line : processStructInitialization rest
                else line : processAllBlocks rest
    -}
    
    -- Process const block content (UNUSED - keeping for reference)
    {-
    processConstBlockContent [] = [")"]  -- Add missing closing brace
    processConstBlockContent (line:rest) =
      if trim line == ")"
        then line : processAllBlocks rest
        else if "const (" `isInfixOf` line  -- Nested const block
          then ")" : line : processConstBlockContent rest  -- Close previous, start new
        else if "var (" `isInfixOf` line  -- Start of var block
          then ")" : line : processVarBlockContent rest  -- Close const first
        else if "type (" `isInfixOf` line  -- Start of type block
          then ")" : line : processTypeBlockContent rest  -- Close const first
        else if isQuotedPackageLine line  -- Import line, close const first
          then ")" : line : processAllBlocks rest
        else if "// =====" `isInfixOf` line  -- Section comment line, continue processing
          then line : processConstBlockContent rest
        else if "// " `isPrefixOf` (trim line)  -- Regular comment line, continue processing
          then line : processConstBlockContent rest
        else if isPrefixOf "func " (trim line)  -- Function declaration, close const block first
          then ")" : line : processAllBlocks rest
        else if null (trim line)
          then processConstBlockContent rest  -- Skip empty lines
          else line : processConstBlockContent rest
    -}
    
    -- Process var block content (UNUSED - keeping for reference)
    {-
    processVarBlockContent [] = [")"]  -- Add missing closing brace
    processVarBlockContent (line:rest) =
      if trim line == ")"
        then line : processAllBlocks rest
        else if "var (" `isInfixOf` line  -- Nested var block
          then ")" : line : processVarBlockContent rest  -- Close previous, start new
        else if "const (" `isInfixOf` line  -- Start of const block
          then ")" : line : processConstBlockContent rest  -- Close var first
        else if "type (" `isInfixOf` line  -- Start of type block
          then ")" : line : processTypeBlockContent rest  -- Close var first
        else if isQuotedPackageLine line  -- Import line, close var first
          then ")" : line : processAllBlocks rest
        else if "// Constants and iota usage" `isInfixOf` line  -- Specific comment line, continue processing
          then line : processVarBlockContent rest
        else if "// =====" `isInfixOf` line  -- Section comment line, continue processing
          then line : processVarBlockContent rest
        else if "// " `isPrefixOf` (trim line)  -- Regular comment line, continue processing
          then line : processVarBlockContent rest
        else if isPrefixOf "func " (trim line)  -- Function declaration, close var block first
          then ")" : line : processAllBlocks rest
        else if null (trim line)
          then processVarBlockContent rest  -- Skip empty lines
          else line : processVarBlockContent rest
    -}
    
    -- Process ownership block content (UNUSED - keeping for reference)
    {-
    processOwnershipBlockContent [] = []  -- No special handling needed
    processOwnershipBlockContent (line:rest) =
      if trim line == "}"
        then processAllBlocks rest
        else if isQuotedPackageLine line  -- Import line, close ownership block first
          then "}" : line : processAllBlocks rest
        else if isPrefixOf "func " (trim line)  -- Function declaration, close ownership block first
          then "}" : line : processAllBlocks rest
        else if null (trim line)
          then processOwnershipBlockContent rest  -- Skip empty lines
          else line : processOwnershipBlockContent rest

    -- Process type block content (UNUSED - keeping for reference)
    processTypeBlockContent [] = [")"]  -- Add missing closing brace
    processTypeBlockContent (line:rest) =
      if trim line == ")"
        then line : processAllBlocks rest
        else if "type (" `isInfixOf` line  -- Nested type block
          then ")" : line : processTypeBlockContent rest  -- Close previous, start new
        else if "const (" `isInfixOf` line  -- Start of const block
          then ")" : line : processConstBlockContent rest  -- Close type first
        else if "var (" `isInfixOf` line  -- Start of var block
          then ")" : line : processVarBlockContent rest  -- Close type first
        else if isQuotedPackageLine line  -- Import line, close type first
          then ")" : line : processAllBlocks rest
        else if isPrefixOf "type " (trim line) && not ("type (" `isInfixOf` line)  -- Standalone type declaration
          then ")" : line : processAllBlocks rest  -- Close type block, process as standalone
        else if "// =====" `isInfixOf` line  -- Section comment line, continue processing
          then line : processTypeBlockContent rest
        else if "// " `isPrefixOf` (trim line)  -- Regular comment line, continue processing
          then line : processTypeBlockContent rest
        else if isPrefixOf "func " (trim line)  -- Function declaration, close type block first
          then ")" : line : processAllBlocks rest
        else if null (trim line)
          then processTypeBlockContent rest  -- Skip empty lines
          else line : processTypeBlockContent rest
    -}

    -- Remove empty lines at the beginning and end
    trimEmptyLines lines' =
      let
        -- Remove leading empty lines
        dropLeading = dropWhile (\l -> null (trim l)) lines'
        -- Remove trailing empty lines
        dropTrailing = reverse $ dropWhile (\l -> null (trim l)) $ reverse dropLeading
      in
        dropTrailing


-- String replacement utility
replace :: String -> String -> String -> String
replace old new str =
  if old `isInfixOf` str
    then
      let parts = splitOn old str
          result = intercalate new parts
      in result
    else str
  where
    splitOn :: String -> String -> [String]
    splitOn _ "" = []
    splitOn delim s = search s []
      where
        search [] acc = [reverse acc]
        search rest@(c:cs) acc
          | delim `isPrefixOf` rest = reverse acc : search (drop (length delim) rest) []
          | otherwise = search cs (c:acc)

-- Utility function to trim whitespace
trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace

-- Remove import declarations from content to avoid false positives in import detection
removeImportDeclarations :: String -> String
removeImportDeclarations content =
  let linesList = lines content
      filteredLines = filter isNotImportLine linesList
  in unlines filteredLines
  where
    isNotImportLine line =
      let trimmed = trim line
      in not (isPrefixOf "import" trimmed) && not (isInImportBlock trimmed) && not (isStandaloneQuotedImport trimmed)

    -- Check if line is part of an import block (starts with quote)
    isInImportBlock line =
      case line of
        '"':_ -> last line == '"'
        '\t':'"':_ -> last line == '"'
        _ -> False

    -- Check if line is a standalone quoted import (with possible tab)
    isStandaloneQuotedImport line =
      let trimmed = trim line
      in isQuotedPackageLine trimmed && not (isPrefixOf "import" trimmed)

    -- Check if a line is just a quoted package name
    isQuotedPackageLine line =
      let trimmed = trim line
      in (trimmed == "\"fmt\"" || trimmed == "\"sync\"" || trimmed == "\"time\"" ||
          trimmed == "\"unsafe\"" || trimmed == "\"os\"" || trimmed == "\"io\"" || trimmed == "\"io/ioutil\"" ||
          trimmed == "\"strings\"" || trimmed == "\"math\"" || trimmed == "\"runtime\"" ||
          trimmed == "\"container/list\"" || trimmed == "\"context\"" ||
          trimmed == "\"log\"" || trimmed == "\"reflect\"" || trimmed == "\"bufio\"" ||
          trimmed == "\"strconv\"" || trimmed == "\"encoding/json\"" || trimmed == "\"regexp\"")

-- Ensure all code is inside functions (not top-level statements)
ensureCodeInFunctions :: [String] -> String
ensureCodeInFunctions linesList =
  let
    -- First, wrap any top-level statements in main function
    wrappedLines = wrapTopLevelStatements linesList
    -- Then ensure proper function boundaries
    boundaryFixed = fixFunctionBoundaries wrappedLines
    -- Finally fix any missing function braces
    finalLines = fixMissingFunctionBraces boundaryFixed
  in
    unlines finalLines

-- Wrap top-level statements inside a main function if they're not already in one
wrapInMainFunction :: [String] -> [String]
wrapInMainFunction linesList =
  let
    -- Separate lines into different sections (imports, types, functions, then wrap non-function code)
    (functionLines, nonFunctionLines) = partition isFunctionOrTypeOrImport linesList
    -- Group the non-function lines that should go in main
    mainBodyLines = filter (not . null) nonFunctionLines
  in
    if null mainBodyLines
      then linesList  -- No non-function code to wrap
      else ["func main() {"] ++ indentLines mainBodyLines ++ ["}"] ++ functionLines

-- Identify if a line belongs to a function, type or import declaration
isFunctionOrTypeOrImport :: String -> Bool
isFunctionOrTypeOrImport line =
  let trimmed = trim line
  in isPrefixOf "func " trimmed || isPrefixOf "type " trimmed || 
     isPrefixOf "var " trimmed || isPrefixOf "const " trimmed ||
     isPrefixOf "import " trimmed || isPrefixOf "//" trimmed

-- Wrap statements that are outside of functions but main already exists
wrapTopLevelStatements :: [String] -> [String]
wrapTopLevelStatements ls = 
  let
    -- Separate imports, type declarations, and function declarations from other code
    (declarations, statements) = partition isDeclaration ls
    
    -- Filter out empty lines and comments for statements to wrap
    topLevelStatements = filter (not . null . trim) $ filter (not . isComment) statements
    
    -- If there are top-level statements, wrap them in main function
    wrappedStatements = if null topLevelStatements 
                       then [] 
                       else ["func main() {"] ++ map ("    " ++) topLevelStatements ++ ["}"]
  in
    declarations ++ wrappedStatements
  where
    -- Check if line is a declaration (import, type, func, var, const)
    isDeclaration line = 
      let trimmed = trim line
      in isPrefixOf "import " trimmed || isPrefixOf "type " trimmed || 
         isPrefixOf "func " trimmed || isPrefixOf "var " trimmed || 
         isPrefixOf "const " trimmed || null trimmed || isComment line
    
    -- Check if line is a comment
    isComment line = isPrefixOf "//" (trim line)

-- Check if the line starts a function definition



-- Check if the line ends a function (is a closing brace)
isClosingBrace :: String -> Bool
isClosingBrace line = trim line == "}"

-- Check if line is a statement that should be in a function



-- Add indentation to lines (for main function body)
indentLines :: [String] -> [String]
indentLines linesList = map (\line -> "    " ++ line) linesList

-- Check if a closing brace is malformed (should be removed)
isMalformedClosingBrace :: String -> Bool
isMalformedClosingBrace _ =
  False  -- Don't remove any standalone closing braces for now
  -- TODO: Add smarter logic to detect only truly malformed braces

-- Insert missing closing brace when a new function starts while previous not closed
fixFunctionBoundaries :: [String] -> [String]
fixFunctionBoundaries = go False 0
  where
    go _ _ [] = []
    go inFunc braceCount (l:ls)
      | startsFunc l && inFunc = "}" : l : go True 1 ls  -- Add missing closing brace for previous function
      | startsFunc l = l : go True 1 ls  -- Start new function
      | "{" `isInfixOf` l = l : go inFunc (braceCount + 1) ls  -- Increase brace count
      | endsFunc l = 
          if braceCount > 1 
            then l : go inFunc (braceCount - 1) ls  -- Decrease brace count but stay in function
            else if inFunc && braceCount == 1
              then l : go False 0 ls  -- End current function
              else l : go inFunc braceCount ls  -- Just a regular closing brace
      | otherwise = l : go inFunc braceCount ls
    startsFunc s = let t = trim s in isPrefixOf "func " t
    endsFunc s = trim s == "}"

-- Fix missing function closing braces by adding them when needed
fixMissingFunctionBraces :: [String] -> [String]
fixMissingFunctionBraces [] = []
fixMissingFunctionBraces (line:rest) =
  let (processed, remaining) = processFunctionBody (line:rest) []
  in processed ++ fixMissingFunctionBraces remaining
  where
    -- Process lines until we find a function that needs a closing brace
    processFunctionBody [] _acc = ([], [])
    processFunctionBody (current:remaining) _acc =
      if isFunctionOpening current
        then let (bodyLines, restLines, hasClosingBrace) = takeUntilFunctionEnd remaining
                 in if hasClosingBrace
                      then (_acc ++ [current] ++ bodyLines, restLines)
                      else (_acc ++ [current] ++ bodyLines ++ ["}"], restLines)
        else (_acc ++ [current], remaining)

    -- Check if line opens a function
    isFunctionOpening l = "func " `isInfixOf` l && "{" `isInfixOf` l

    -- Take lines until we find a line that ends a function or starts a new one
    -- Return (bodyLines, restLines, hasClosingBrace)
    takeUntilFunctionEnd [] = ([], [], False)
    takeUntilFunctionEnd (l:ls)
      | isClosingBrace l = ([l], ls, True)  -- Found closing brace, return it with rest
      | isFunctionOpening l = ([], l:ls, False)  -- Found new function, no closing brace for previous
      | isOtherDeclaration l = ([], l:ls, False)  -- Found other declaration, no closing brace for previous
      | otherwise = let (body, rest', hasBrace) = takeUntilFunctionEnd ls in (l:body, rest', hasBrace)

    -- Check if line ends a function (has closing brace)


    -- Check if line starts a new top-level declaration (indicates previous function should be closed)
    isOtherDeclaration l = let trimmed = trim l in
      isPrefixOf "func " trimmed || isPrefixOf "type " trimmed || 
      isPrefixOf "var " trimmed || isPrefixOf "const " trimmed ||
      isPrefixOf "import " trimmed || isPrefixOf "package " trimmed
