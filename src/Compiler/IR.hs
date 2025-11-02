{-# LANGUAGE RecordWildCards #-}

module Compiler.IR (
    SourceIR(..),
    SemanticIR(..),
    GoIR(..),
    buildSourceIR,
    buildSemanticIR,
    emitGo,
    rawSourceFromTypus,
    moduleFromTypus
) where

import Parser (TypusFile(..), CodeBlock(..))
import Compiler.Error
import Compiler.GoAst
import SourceLocation (locatedValue)

import Data.Char (isSpace)
import Data.Function (on)
import Data.List (intercalate, isInfixOf, isPrefixOf, nubBy)

-- | Lightweight representation of the parsed Typus source prior to any
-- analysis. It keeps the parsed file together with the raw Go-like source
-- extracted from code blocks.
data SourceIR = SourceIR
    { sourceTypusFile :: TypusFile
    , sourceText :: String
    }

-- | Semantic IR captures the Go AST after structural rewrites have been
-- applied (imports inferred, main function synthesised, ...).
data SemanticIR = SemanticIR
    { semanticTypusFile :: TypusFile
    , semanticModule :: GoModule
    }

-- | Final Go artefact ready to be rendered to source code.
data GoIR = GoIR
    { goModule :: GoModule
    , goSource :: String
    }

buildSourceIR :: TypusFile -> SourceIR
buildSourceIR typusFile = SourceIR
    { sourceTypusFile = typusFile
    , sourceText = rawSourceFromTypus typusFile
    }

buildSemanticIR :: SourceIR -> Either CompilationError SemanticIR
buildSemanticIR ir = do
    goMod <- moduleFromTypus (sourceTypusFile ir)
    pure SemanticIR
        { semanticTypusFile = sourceTypusFile ir
        , semanticModule = goMod
        }

emitGo :: SemanticIR -> GoIR
emitGo ir = GoIR
    { goModule = semanticModule ir
    , goSource = renderGoModule (semanticModule ir)
    }

moduleFromTypus :: TypusFile -> Either CompilationError GoModule
moduleFromTypus typusFile = do
    let rawSource = rawSourceFromTypus typusFile
    parsed <- case parseGoModule (lines rawSource) of
        Left err -> Left $ mkCompilationError GoGenerationErrorKind ("Failed to parse Go module: " ++ err) []
        Right goMod -> Right goMod
    let buildTags = tfBuildTags typusFile
        module0 = parsed { gmBuildTags = if null buildTags
                                         then gmBuildTags parsed
                                         else map locatedValue buildTags
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
          case takeUntilMatching '>' xs (0 :: Int) [] of
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

splitByComma :: String -> [String]
splitByComma s = case break (== ',') s of
    (a, []) -> [a]
    (a, _:b) -> a : splitByComma b

trim :: String -> String
trim = dropWhile isSpace . dropWhileEnd isSpace

dropWhileEnd :: (a -> Bool) -> [a] -> [a]
dropWhileEnd p = reverse . dropWhile p . reverse

-- Import inference -----------------------------------------------------------

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
  , simpleDetector "context" ["context."] `withGuard` (\txt -> not ("context :=" `isInfixOf` txt || "&context" `isInfixOf` txt))
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
