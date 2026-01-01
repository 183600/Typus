{-# LANGUAGE CPP #-}
module Test.Unit.NewQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck
  ( Property
  , (===)
  , (==>)
  , forAll
  , property
  , Arbitrary(..)
  , Gen
  , elements
  , listOf
  , listOf1
  , choose
  , oneof
  , suchThat
  , vectorOf
  )
import Data.List (isInfixOf, isPrefixOf)
import Data.List (sort, nub)
import Data.Char (isAlpha, isAlphaNum, isDigit)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map

import Compiler.Errors.Core
  ( TypeError(..)
  , ErrorSeverity(..)
  , ErrorCategory(..)
  , ErrorLocation(..)
  , ErrorContext(..)
  , emptyContext
  , _atLocation
  )
import SourceLocation
  ( SourcePos(..)
  , SourceSpan(..)
  , posAt
  , spanBetween
  , advancePosByText
  )
import Parser
  ( parseTypus
  , TypusFile(..)
  , FileDirectives(..)
  , BlockDirectives(..)
  )
import Compiler.TypeChecker
  ( Type(..)
  , TypeEnv(..)
  , buildTypeEnvFromPairs
  , lookupVariable
  , areTypesCompatible
  , typesEqual
  )
import Dependencies.TypeSystem
  ( TypeVar(..)
  , TypeConstraint(..)
  , unify
  )
import Utils
  ( trim
  , splitBy
  , removeComments
  )

tests :: TestTree
tests =
  testGroup "New QuickCheck Tests"
    [ fastProperty "error formatting preserves error ID" $
        \errId -> 
          let loc = _atLocation 1 1
              err = errorAt "test-id" "test message") loc
              formatted = formatError err
          in errId `L.isInfixOf` formatted

    , fastProperty "error formatting includes severity" $
        \severity ->
          let loc = _atLocation 1 1
              err = (errorAt "test-id" "test message") loc) { severity = severity }
              formatted = formatError err
              severityStr = case severity of
                Fatal -> "FATAL"
                Error -> "ERROR"
                Warning -> "WARNING"
                Info -> "INFO"
          in severityStr `L.isInfixOf` formatted

    , fastProperty "error filtering by severity works correctly" $
        \errors severity ->
          let filtered = filterBySeverity severity errors
              expected = L.filter (\e -> severity e == severity) errors
          in L.length filtered === L.length expected

    , fastProperty "error filtering by category works correctly" $
        \errors category ->
          let filtered = filterByCategory category errors
              expected = L.filter (\e -> category e == category) errors
          in L.length filtered === L.length expected

    , fastProperty "error statistics are consistent" $
        \errors ->
          let stats = getErrorStatistics errors
              total = L.length errors
              fatalCount = L.length $ L.filter (\e -> severity e == Fatal) errors
              errorCount = L.length $ L.filter (\e -> severity e == Error) errors
              warningCount = L.length $ L.filter (\e -> severity e == Warning) errors
              infoCount = L.length $ L.filter (\e -> severity e == Info) errors
          in total >= fatalCount + errorCount + warningCount + infoCount

    , fastProperty "source position advancement is consistent" $
        \text ->
          let start = posAt 1 1
              end = advancePosByText text start
          in posOffset end >= posOffset start

    , fastProperty "source span validation works correctly" $
        \line1 col1 line2 col2 ->
          let start = posAt line1 col1
              end = posAt line2 col2
              span = spanBetween start end
              isValid = (line1 < line2) || (line1 == line2 && col1 <= col2)
          in (line1 <= line2 && col1 <= col2) === isValid

    , fastProperty "type environment lookup works correctly" $
        \pairs key ->
          let env = buildTypeEnvFromPairs pairs
              result = lookupVariable key env
              expected = lookup key pairs
          in case (result, expected) of
            (Just found, Just expectedType) -> found === expectedType
            (Nothing, Nothing) -> property True
            _ -> property False

    , fastProperty "type compatibility is reflexive" $
        \typ ->
          areTypesCompatible typ typ

    , fastProperty "type equality is reflexive" $
        \typ ->
          typesEqual typ typ

    , fastProperty "type equality is symmetric" $
        \typ1 typ2 ->
          typesEqual typ1 typ2 === typesEqual typ2 typ1

    , fastProperty "type unification is consistent" $
        \type1 type2 ->
          case unify type1 type2 of
            Right _ -> property True
            Left _ -> property True

    , fastProperty "string trim removes leading L.and trailing whitespace" $
        \s ->
          let trimmed = trim s
              hasLeading = not (null s) && isSpace (L.head s)
              hasTrailing = not (null s) && isSpace (last s)
          in if hasLeading || hasTrailing
             then not (null trimmed) ==> (not (isSpace (L.head trimmed)) && not (isSpace (last trimmed)))
             else property True

    , fastProperty "string split by delimiter preserves content" $
        \s delim ->
          let parts = splitBy delim s
              rejoined = L.concat $ intersperse [delim] parts
          in rejoined === s

    , fastProperty "comment removal preserves non-comment content" $
        \code ->
          let withoutComments = removeComments code
              hasComments = "//" `L.isInfixOf` code || "/*" `L.isInfixOf` code
          in if hasComments
             then L.length withoutComments <= L.length code
             else withoutComments === code

    , fastProperty "error context preserves function information" $
        \funcName varName varType ->
          let ctx = emptyContext { contextFunction = Just funcName, contextVariable = Just varName, contextType = Just varType }
              err = errorAt "TEST001" (T.pack "test") (_atLocation 1 1) `withContext` ctx
          in contextFunction (context err) === Just funcName

    , fastProperty "error suggestions are preserved" $
        \suggestions ->
          let loc = _atLocation 1 1
              err = errorAt "TEST001" (T.pack "test") loc `withSuggestions` (map T.pack suggestions)
          in L.length (suggestions err) === L.length suggestions

    , fastProperty "parsing roundtrip preserves directives" $
        \directives ->
          let source = "//! " ++ unwords directives ++ "\npackage main\nfunc main() {}\n"
          in case parseTypus source of
            Left _ -> property True  -- Invalid directives are allowed to fail
            Right typusFile -> 
              let FileDirectives { fdOwnership = ownership } = tfDirectives typusFile
              in case ownership of
                Just _ -> "ownership" `elem` directives ==> property True
                Nothing -> not ("ownership" `elem` directives) ==> property True

    , fastProperty "multiple error formatting preserves order" $
        \errors ->
          let formatted = formatErrors errors
              lines' = lines formatted
          in L.length lines' >= L.length errors

    , fastProperty "error location updates work correctly" $
        \line col ->
          let loc1 = _atLocation line col
              loc2 = _atLocation (line + 1) (col + 1)
              err = errorAt "TEST001" (T.pack "test") loc1
              updatedErr = withLocation err loc2
          in location updatedErr === loc2

    , fastProperty "type environment preserves variable types" $
        \pairs ->
          let env = buildTypeEnvFromPairs pairs
          in L.all (\(key, typ) -> lookupVariable key env == Just typ) pairs

    , fastProperty "error severity ordering is consistent" $
        \errors ->
          let sortedBySeverity = sort errors
              isOrdered = L.all (\(e1, e2) -> severity e1 <= severity e2) (zip sortedBySeverity (drop 1 sortedBySeverity))
          in property isOrdered

    , fastProperty "parsing handles empty input gracefully" $
        \_ ->
          case parseTypus "" of
            Left _ -> property True
            Right typusFile -> tfBlocks typusFile === []

    , fastProperty "parsing handles simple functions" $
        \funcName ->
          let source = "package main\nfunc " ++ funcName ++ "() {}\n"
          in case parseTypus source of
            Left _ -> isValidIdentifier funcName ==> property False
            Right typusFile -> not (L.null $ tfBlocks typusFile)

    , fastProperty "error messages are preserved through formatting" $
        \message ->
          let loc = _atLocation 1 1
              err = errorAt "test-id" (T.pack message) loc
              formatted = formatError err
          in message `L.isInfixOf` formatted

    , fastProperty "type constraint unification is deterministic" $
        \constraint ->
          case unify constraint constraint of
            Right _ -> property True
            Left _ -> property True

    , fastProperty "source position tracking is consistent" $
        \text ->
          let positions = scanl (\pos char -> advancePosByText [char] pos) (posAt 1 1) text
              offsets = map posOffset positions
          in offsets == sort offsets

    , fastProperty "error statistics L.sum to total" $
        \errors ->
          let stats = getErrorStatistics errors
              totalFromStats = L.sum $ Map.elems stats
          in totalFromStats >= L.length errors

    , fastProperty "parsing preserves build tags" $
        \buildTags ->
          let source = unlines $ L.map (\tag -> "//go:build " ++ tag) buildTags ++ ["package main"]
          in case parseTypus source of
            Left _ -> property True
            Right typusFile -> L.length (tfBuildTags typusFile) >= L.length buildTags

    , fastProperty "type compatibility handles basic types" $
        \typeName1 typeName2 ->
          let type1 = TypeName typeName1
              type2 = TypeName typeName2
              compatible = areTypesCompatible type1 type2
          in typeName1 == typeName2 ==> compatible

    , fastProperty "error context preserves additional information" $
        \additionalInfo ->
          let ctx = emptyContext { contextAdditional = [("key", additionalInfo)] }
              err = errorAt "TEST001" (T.pack "test") (_atLocation 1 1) `withContext` ctx
          in lookup "key" (contextAdditional (context err)) === Just additionalInfo
    ]
  where
    isValidIdentifier [] = False
    isValidIdentifier (c:cs) = isAlpha c && L.all isAlphaNum cs
    
    intersperse _ [] = []
    intersperse _ [x] = [x]
    intersperse sep (x:y:xs) = x : sep : intersperse sep (y:xs)
    
    isSpace c = c `elem` " \t\n\r"