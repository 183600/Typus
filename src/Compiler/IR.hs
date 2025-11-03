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
import EnhancedErrorHandler
    ( CompilerError
    , CompilerResult
    , CompilationPhase(..)
    )
import ErrorHandler (ErrorCategory(..), ErrorSeverity(..))
import Compiler.EnhancedErrors (mkCompilerError)
import Compiler.GoAst
import Compiler.GoLexer (GoToken(..), GoTokenKind(..), tokenizeGo, isWhitespaceToken, isCommentToken)
import Compiler.ValueAnalysis (ValueInfo, analyzeValueSemantics)
import SourceLocation (locatedValue)

import Data.Char (isSpace)
import Data.Function (on)
import Data.List (intercalate, isInfixOf, isPrefixOf, nubBy)
import qualified Data.Set as Set
import qualified Data.Text as T

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
    , semanticValueInfo :: [ValueInfo]
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

buildSemanticIR :: SourceIR -> CompilerResult SemanticIR
buildSemanticIR ir = do
    goMod <- moduleFromTypus (sourceTypusFile ir)
    pure SemanticIR
        { semanticTypusFile = sourceTypusFile ir
        , semanticModule = goMod
        , semanticValueInfo = analyzeValueSemantics goMod
        }

emitGo :: SemanticIR -> GoIR
emitGo ir = GoIR
    { goModule = semanticModule ir
    , goSource = renderGoModule (semanticModule ir)
    }

moduleFromTypus :: TypusFile -> CompilerResult GoModule
moduleFromTypus typusFile =
    let rawSource = rawSourceFromTypus typusFile
    in case parseGoModule (lines rawSource) of
        Left err -> Left [goModuleParseError err]
        Right parsedModule ->
            let buildTags = tfBuildTags typusFile
                module0 = parsedModule { gmBuildTags = if null buildTags
                                                         then gmBuildTags parsedModule
                                                         else map locatedValue buildTags
                                      }
                module1 = applyGenerics module0
                module2 = ensurePackageDecl module1
                module3 = ensureMainFunction module2
                module4 = attachInferredImports module3
            in Right module4

goModuleParseError :: String -> CompilerError
goModuleParseError errMsg =
    mkCompilerError
        "GO0001"
        (T.pack ("Failed to parse generated Go module: " ++ errMsg))
        CodeGenerationPhase
        Integration
        Error
        Nothing
        Nothing
        (map T.pack
            [ "Inspect the generated Go block for syntax issues"
            , "Ensure embedded Go code in Typus files is valid"
            ])
        []
        Nothing

rawSourceFromTypus :: TypusFile -> String
rawSourceFromTypus TypusFile{..} = intercalate "\n" $ map cbContent tfBlocks

applyGenerics :: GoModule -> GoModule
applyGenerics goModule =
    goModule { gmDecls = map convertDecl (gmDecls goModule) }
  where
    convertDecl decl =
      let withTypeParams = mapDeclLines convertTypeLine decl
      in rewriteDecl withTypeParams

    convertTypeLine line
      | "type " `isPrefixOf` trim line = convertTypeDeclLine line
      | otherwise = line

    rewriteDecl d = case d of
      GoFunc (FuncDecl ls)            -> GoFunc (FuncDecl (rewriteLines ls))
      GoType (TypeDecl ls g)          -> GoType (TypeDecl (rewriteLines ls) g)
      GoVar (VarDecl ls g)            -> GoVar (VarDecl (rewriteLines ls) g)
      GoConst (ConstDecl ls g)        -> GoConst (ConstDecl (rewriteLines ls) g)
      GoStatement (StatementBlock ls) -> GoStatement (StatementBlock (rewriteLines ls))
      GoRaw block                     -> GoRaw block

    rewriteLines [] = []
    rewriteLines ls =
      let combined = intercalate "\n" ls
          converted = replaceGenericAngles combined
      in splitLinesPreserving (length ls) converted

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
replaceGenericAngles input =
    concatMap tokenText (rewriteTokens (tokenizeGo input))
  where
    rewriteTokens [] = []
    rewriteTokens (tok:rest)
      | tokenKind tok == TokIdentifier =
          let (spaces, restAfterSpaces) = spanInlineWhitespace rest
          in case restAfterSpaces of
               (ltTok:afterLt)
                 | isLessToken ltTok ->
                     let (inside, restAfter, success) = consumeGenericTokens 0 [] afterLt
                     in if success && hasNonTrivial inside
                           then
                             let innerConverted = rewriteTokens inside
                                 prefix = tok : mkSymbol "[" : innerConverted
                             in prefix ++ (mkSymbol "]" : rewriteTokens restAfter)
                           else tok : (spaces ++ (ltTok : rewriteTokens afterLt))
               _ -> tok : rewriteTokens rest
      | otherwise = tok : rewriteTokens rest

    spanInlineWhitespace = span isInlineSpace

    isInlineSpace tok =
      isWhitespaceToken tok && not (containsNewline (tokenText tok))

    containsNewline = any (`elem` "\n\r")

    isLessToken GoToken{ tokenKind = TokSymbol, tokenText = "<" } = True
    isLessToken _ = False

    mkSymbol sym = GoToken { tokenKind = TokSymbol, tokenText = sym }

    hasNonTrivial = any (\t -> not (isWhitespaceToken t || isCommentToken t))

    consumeGenericTokens _ acc [] = (reverse acc, [], False)
    consumeGenericTokens depth acc (tok':rest')
      | tokenKind tok' == TokSymbol && tokenText tok' == "<" =
          consumeGenericTokens (depth + 1) (tok':acc) rest'
      | tokenKind tok' == TokSymbol && tokenText tok' == ">" =
          if depth == 0
             then (reverse acc, rest', True)
             else consumeGenericTokens (depth - 1) (tok':acc) rest'
      | otherwise = consumeGenericTokens depth (tok':acc) rest'

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

splitLinesPreserving :: Int -> String -> [String]
splitLinesPreserving 0 _ = []
splitLinesPreserving 1 s = [s]
splitLinesPreserving n s =
  let (line, rest) = break (== '\n') s
      remainder = case rest of
        []     -> ""
        (_:xs) -> xs
  in line : splitLinesPreserving (n - 1) remainder

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

data ImportUsage = ImportUsage
  { usagePackages    :: Set.Set String
  , usageQualified   :: Set.Set (String, String)
  , usageIdentifiers :: Set.Set String
  , usageRawText     :: String
  }

data ImportDetector = ImportDetector
  { detectorAlias      :: Maybe String
  , detectorPath       :: String
  , detectorPredicate  :: ImportUsage -> Bool
  }

importDetectors :: [ImportDetector]
importDetectors =
  [ packageDetector "bufio" "bufio"
  , packageDetector "container/list" "list"
  , packageDetector "context" "context"
  , packageDetector "log" "log"
  , packageDetector "reflect" "reflect"
  , packageDetector "fmt" "fmt"
  , packageDetector "math" "math"
  , packageDetector "math/cmplx" "cmplx"
  , packageDetector "math/big" "big"
  , symbolDetector "math/rand" "rand" mathRandSymbols
  , packageDetector "time" "time"
  , packageDetector "os" "os"
  , packageDetector "path/filepath" "filepath"
  , packageDetector "io" "io"
  , packageDetector "io/ioutil" "ioutil"
  , packageDetector "strings" "strings"
  , packageDetector "sync" "sync"
  , packageDetector "sync/atomic" "atomic"
  , packageDetector "runtime" "runtime"
  , packageDetector "unicode/utf8" "utf8"
  , packageDetector "unsafe" "unsafe"
  , packageDetector "strconv" "strconv"
  , packageDetector "encoding/json" "json"
  , packageDetector "encoding/xml" "xml"
  , packageDetector "regexp" "regexp"
  , packageDetector "errors" "errors"
  , packageDetector "net/http" "http"
  , packageDetector "net/url" "url"
  , packageDetector "net" "net"
  , packageDetector "sort" "sort"
  , packageDetector "crypto/md5" "md5"
  , packageDetector "crypto/sha1" "sha1"
  , packageDetector "crypto/sha256" "sha256"
  , packageDetector "crypto/sha512" "sha512"
  , packageDetector "crypto/aes" "aes"
  , packageDetector "crypto/cipher" "cipher"
  , symbolDetector "crypto/rand" "rand" cryptoRandSymbols
  , packageDetector "encoding/base64" "base64"
  , packageDetector "encoding/hex" "hex"
  , packageDetector "encoding/csv" "csv"
  , packageDetector "bytes" "bytes"
  , packageDetector "encoding/binary" "binary"
  , packageDetector "compress/gzip" "gzip"
  , packageDetector "container/ring" "ring"
  , packageDetector "math/bits" "bits"
  , packageDetector "hash/fnv" "fnv"
  , packageDetector "syscall" "syscall"
  , packageDetector "database/sql" "sql"
  , customDetector (Just "_") "github.com/mattn/go-sqlite3" (\usage -> "github.com/mattn/go-sqlite3" `isInfixOf` usageRawText usage)
  , packageDetector "testing" "testing"
  ]

mathRandSymbols :: [String]
mathRandSymbols =
  [ "Seed"
  , "Int31"
  , "Int31n"
  , "Int63"
  , "Int63n"
  , "Intn"
  , "Float32"
  , "Float64"
  , "Perm"
  , "Shuffle"
  , "New"
  , "NewSource"
  , "NewZipf"
  , "ExpFloat64"
  , "NormFloat64"
  ]

cryptoRandSymbols :: [String]
cryptoRandSymbols = ["Reader", "Read", "Prime"]

packageDetector :: String -> String -> ImportDetector
packageDetector path pkg = ImportDetector Nothing path (\usage -> packageUsed usage pkg)

symbolDetector :: String -> String -> [String] -> ImportDetector
symbolDetector path pkg symbols =
  ImportDetector Nothing path (\usage -> any (qualifiedSymbolUsed usage pkg) symbols)

customDetector :: Maybe String -> String -> (ImportUsage -> Bool) -> ImportDetector
customDetector alias path predicate = ImportDetector alias path predicate

detectImports :: String -> [ImportDecl]
detectImports content =
  let usage = buildImportUsage content
  in [ ImportDecl (detectorAlias det) (detectorPath det)
     | det <- importDetectors
     , detectorPredicate det usage
     ]

buildImportUsage :: String -> ImportUsage
buildImportUsage raw =
  let tokens = tokenizeGo raw
      (qualifiedPairs, packages) = collectQualifiedUsage tokens
      identifiers = Set.fromList [ tokenText tok | tok <- tokens, tokenKind tok == TokIdentifier ]
  in ImportUsage
       { usagePackages = packages
       , usageQualified = Set.fromList qualifiedPairs
       , usageIdentifiers = identifiers
       , usageRawText = raw
       }

collectQualifiedUsage :: [GoToken] -> ([(String, String)], Set.Set String)
collectQualifiedUsage = go [] Set.empty
  where
    go acc packages [] = (reverse acc, packages)
    go acc packages (tok:rest) =
      case tok of
        GoToken { tokenKind = TokIdentifier, tokenText = pkg } ->
          let rest1 = dropTrivialTokens rest
          in case rest1 of
               GoToken { tokenKind = TokSymbol, tokenText = "." } : rest2 ->
                 let packages' = Set.insert pkg packages
                     rest3 = dropTrivialTokens rest2
                 in case rest3 of
                      GoToken { tokenKind = TokIdentifier, tokenText = sym } : _ ->
                        go ((pkg, sym):acc) packages' rest
                      GoToken { tokenKind = TokKeyword, tokenText = sym } : _ ->
                        go ((pkg, sym):acc) packages' rest
                      _ -> go acc packages' rest
               _ -> go acc packages rest
        _ -> go acc packages rest

dropTrivialTokens :: [GoToken] -> [GoToken]
dropTrivialTokens = dropWhile (\tok -> isWhitespaceToken tok || isCommentToken tok)

packageUsed :: ImportUsage -> String -> Bool
packageUsed usage pkg = pkg `Set.member` usagePackages usage

qualifiedSymbolUsed :: ImportUsage -> String -> String -> Bool
qualifiedSymbolUsed usage pkg sym = (pkg, sym) `Set.member` usageQualified usage
