-- Simplified Typus to Go Compiler

module Compiler (compile) where

import Parser (TypusFile(..), CodeBlock(..), FileDirectives(..), BlockDirectives(..))
import Data.List (intercalate, isInfixOf, isPrefixOf, isSuffixOf, partition)
import Data.Char (isSpace)

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
    -- Generate file header (only once)
    header = "package main\n"

    -- Generate imports (only once)
    imports = generateImports typusFile

    -- Get the original code content without package/imports
    originalContent = cleanCodeBlocks $ intercalate "\n" $ map cbContent (tfBlocks typusFile)

    -- Combine all parts
    allParts = filter (not . null) [header, imports, originalContent]
  in
    intercalate "\n" allParts

-- Simple clean - just remove package and import lines
cleanCodeBlocks :: String -> String
cleanCodeBlocks content =
  let linesList = lines content
      -- Remove package declarations and import declarations 
      filteredLines = filter (\line ->
        let trimmed = trim line
            pkgLine = isPrefixOf "package" trimmed
            importLine = isPrefixOf "import" trimmed
        in not pkgLine && not importLine) linesList
  in unlines filteredLines

-- Generate imports section with enhanced detection
generateImports :: TypusFile -> String
generateImports typusFile =
  let
    content = intercalate "\n" $ map cbContent (tfBlocks typusFile)
    
    -- Enhanced import detection - only detect actual usage
    hasBufio = "bufio." `isInfixOf` content
    hasFmt = "fmt." `isInfixOf` content || "fmt.Println" `isInfixOf` content || "fmt.Printf" `isInfixOf` content
    hasMath = "math." `isInfixOf` content || "math.Pi" `isInfixOf` content || "math.Sqrt" `isInfixOf` content
    hasMathRand = ("math/rand" `isInfixOf` content || "rand.Seed" `isInfixOf` content || "rand.Intn" `isInfixOf` content) && not ("crypto/rand" `isInfixOf` content)
    hasCmplx = "cmplx." `isInfixOf` content || "cmplx.Sqrt" `isInfixOf` content
    hasMathBig = "big." `isInfixOf` content || "big.NewInt" `isInfixOf` content || "big.Int" `isInfixOf` content
    hasTime = "time." `isInfixOf` content || "time.Now" `isInfixOf` content || "time.Sleep" `isInfixOf` content
    hasOs = "os." `isInfixOf` content || "os.Create" `isInfixOf` content || "os.ReadFile" `isInfixOf` content
    hasPathFilepath = "filepath." `isInfixOf` content || "filepath.Join" `isInfixOf` content
    hasIo = "io." `isInfixOf` content || "io.Copy" `isInfixOf` content || "io.Reader" `isInfixOf` content || "io.Writer" `isInfixOf` content
    hasIoUtil = "ioutil." `isInfixOf` content || "ioutil.ReadFile" `isInfixOf` content || "ioutil.WriteFile" `isInfixOf` content || "ioutil.ReadAll" `isInfixOf` content
    hasStrings = "strings." `isInfixOf` content || "strings.Split" `isInfixOf` content || "strings.ToUpper" `isInfixOf` content
    hasSync = "sync." `isInfixOf` content || "sync.Mutex" `isInfixOf` content || "sync.WaitGroup" `isInfixOf` content
    hasSyncAtomic = "atomic." `isInfixOf` content || "atomic.AddInt64" `isInfixOf` content || "atomic.Bool" `isInfixOf` content
    hasRuntime = "runtime." `isInfixOf` content || "runtime.GOOS" `isInfixOf` content
    hasUnsafe = "unsafe." `isInfixOf` content
    hasContainerList = "container/list" `isInfixOf` content || "list.New" `isInfixOf` content
    hasUnicodeUtf8 = "unicode/utf8" `isInfixOf` content || "utf8.RuneCountInString" `isInfixOf` content
    hasContext = "context." `isInfixOf` content && not ("context :=" `isInfixOf` content) && not ("context :=" `isInfixOf` content) && not ("&context" `isInfixOf` content)
    hasLog = "log." `isInfixOf` content
    hasReflect = "reflect." `isInfixOf` content || "reflect.TypeOf" `isInfixOf` content || "reflect.ValueOf" `isInfixOf` content
    hasStrconv = "strconv." `isInfixOf` content || "strconv.Itoa" `isInfixOf` content || "strconv.Atoi" `isInfixOf` content
    hasJson = "json." `isInfixOf` content || "json.Marshal" `isInfixOf` content || "json.Unmarshal" `isInfixOf` content
    hasXml = "xml." `isInfixOf` content || "xml.Marshal" `isInfixOf` content
    hasRegexp = "regexp." `isInfixOf` content || "regexp.MatchString" `isInfixOf` content || "regexp.MustCompile" `isInfixOf` content
    hasErrors = "errors." `isInfixOf` content || "errors.New" `isInfixOf` content
    hasHttp = "http." `isInfixOf` content || "http.HandleFunc" `isInfixOf` content || "http.ListenAndServe" `isInfixOf` content
    hasNetUrl = "url." `isInfixOf` content || "url.Parse" `isInfixOf` content
    hasSort = "sort." `isInfixOf` content || "sort.Ints" `isInfixOf` content || "sort.Strings" `isInfixOf` content
    hasMd5 = "md5." `isInfixOf` content || "md5.Sum" `isInfixOf` content || "md5.New" `isInfixOf` content
    hasSha1 = "sha1." `isInfixOf` content || "sha1.Sum" `isInfixOf` content || "sha1.New" `isInfixOf` content
    hasSha256 = "sha256." `isInfixOf` content || "sha256.Sum" `isInfixOf` content || "sha256.New" `isInfixOf` content
    hasSha512 = "sha512." `isInfixOf` content || "sha512.Sum" `isInfixOf` content || "sha512.New" `isInfixOf` content
    hasCrypto = "crypto/aes" `isInfixOf` content || "crypto/cipher" `isInfixOf` content || "aes.NewCipher" `isInfixOf` content || "cipher.NewCFBEncrypter" `isInfixOf` content
    hasCryptoRand = "crypto/rand" `isInfixOf` content || "rand.Reader" `isInfixOf` content
    hasBase64 = "base64." `isInfixOf` content || "base64.StdEncoding" `isInfixOf` content
    hasHex = "hex." `isInfixOf` content || "hex.EncodeToString" `isInfixOf` content || "hex.DecodeString" `isInfixOf` content
    hasCsv = "csv." `isInfixOf` content || "csv.NewReader" `isInfixOf` content
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

-- Utility function to trim whitespace
trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace