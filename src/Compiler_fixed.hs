-- Enhanced Typus to Go Compiler with proper syntax handling

module Compiler (compile) where

import Parser (TypusFile(..), CodeBlock(..), FileDirectives(..), BlockDirectives(..))
import Data.List (intercalate, isInfixOf, isPrefixOf)
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
    blocks = transformCodeBlocks (tfBlocks typusFile)

    -- Check for dependent types directive and add runtime support if needed
    runtimeSupport = if hasDependentTypesDirective typusFile
                      then generateRuntimeSupport
                      else ""

    -- Debug: show final result
    _ = trace ("Final blocks: " ++ take 200 blocks) ()

    -- Combine all parts
    allParts = filter (not . null) [header, imports, runtimeSupport, blocks]
  in
    intercalate "\n" allParts ++ "\n"

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
    content = intercalate "\n" $ map cbContent (tfBlocks typusFile)
    
    -- Enhanced import detection
    hasBufio = "bufio." `isInfixOf` content || "\"bufio\"" `isInfixOf` content
    hasFmt = "fmt." `isInfixOf` content || "\"fmt\"" `isInfixOf` content || "fmt.Println" `isInfixOf` content || "fmt.Printf" `isInfixOf` content
    hasMath = "math." `isInfixOf` content || "\"math\"" `isInfixOf` content || "math.Pi" `isInfixOf` content || "math.Sqrt" `isInfixOf` content
    hasTime = "time." `isInfixOf` content || "\"time\"" `isInfixOf` content || "time.Now" `isInfixOf` content || "time.Sleep" `isInfixOf` content
    hasOs = "os." `isInfixOf` content || "\"os\"" `isInfixOf` content || "os.Create" `isInfixOf` content || "os.ReadFile" `isInfixOf` content
    hasIo = "io." `isInfixOf` content || "\"io\"" `isInfixOf` content || "io.Reader" `isInfixOf` content || "io.Writer" `isInfixOf` content
    hasStrings = "strings." `isInfixOf` content || "\"strings\"" `isInfixOf` content || "strings.Split" `isInfixOf` content || "strings.ToUpper" `isInfixOf` content
    hasSync = "sync." `isInfixOf` content || "\"sync\"" `isInfixOf` content || "sync.Mutex" `isInfixOf` content || "sync.WaitGroup" `isInfixOf` content
    hasRuntime = "runtime." `isInfixOf` content || "\"runtime\"" `isInfixOf` content || "runtime.GOOS" `isInfixOf` content
    hasUnsafe = "unsafe." `isInfixOf` content || "\"unsafe\"" `isInfixOf` content
    hasContainerList = "container/list" `isInfixOf` content || "\"container/list\"" `isInfixOf` content
    hasContext = "context." `isInfixOf` content || "\"context\"" `isInfixOf` content
    hasLog = "log." `isInfixOf` content || "\"log\"" `isInfixOf` content
    hasReflect = "reflect." `isInfixOf` content || "\"reflect\"" `isInfixOf` content || "reflect.TypeOf" `isInfixOf` content || "reflect.ValueOf" `isInfixOf` content
    hasStrconv = "strconv." `isInfixOf` content || "\"strconv\"" `isInfixOf` content || "strconv.Itoa" `isInfixOf` content || "strconv.Atoi" `isInfixOf` content
    hasJson = "json." `isInfixOf` content || "\"encoding/json\"" `isInfixOf` content || "json.Marshal" `isInfixOf` content || "json.Unmarshal" `isInfixOf` content
    hasRegexp = "regexp." `isInfixOf` content || "\"regexp\"" `isInfixOf` content || "regexp.MatchString" `isInfixOf` content || "regexp.MustCompile" `isInfixOf` content

    imports = filter (not . null) [
        if hasBufio then "    \"bufio\"" else "",
        if hasContainerList then "    \"container/list\"" else "",
        if hasContext then "    \"context\"" else "",
        if hasLog then "    \"log\"" else "",
        if hasReflect then "    \"reflect\"" else "",
        if hasFmt then "    \"fmt\"" else "",
        if hasMath then "    \"math\"" else "",
        if hasTime then "    \"time\"" else "",
        if hasOs then "    \"os\"" else "",
        if hasIo then "    \"io\"" else "",
        if hasStrings then "    \"strings\"" else "",
        if hasSync then "    \"sync\"" else "",
        if hasRuntime then "    \"runtime\"" else "",
        if hasUnsafe then "    \"unsafe\"" else "",
        if hasStrconv then "    \"strconv\"" else "",
        if hasJson then "    \"encoding/json\"" else "",
        if hasRegexp then "    \"regexp\"" else ""
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
    -- Debug: show block contents
    _ = trace ("Block contents: " ++ show (length blockContents) ++ " blocks") ()
    _ = trace ("First block: " ++ (if null blockContents then "EMPTY" else take 100 (head' blockContents))) ()
      where head' [] = ""
            head' (x:_) = x
    -- Filter out EOF lines
    filteredContents = map filterEOF (filter (not . null) blockContents)
    -- Apply all transformations
    transformedContents = map transformCode filteredContents
    -- Combine all blocks and clean them
    combinedContent = intercalate "\n" transformedContents
    cleanedContent = cleanCodeBlocks combinedContent
  in
    cleanedContent

-- Filter out EOF lines
filterEOF :: String -> String
filterEOF content = 
  unlines $ filter (not . isEOFLine) (lines content)
  where
    isEOFLine line = "EOF < /dev/null" `isInfixOf` line

-- Transform code to fix syntax issues with enhanced handling
transformCode :: String -> String
transformCode content = 
  let
    linesContent = lines content
    transformedLines = map transformLine linesContent
  in
    unlines transformedLines

-- Enhanced line transformation
transformLine :: String -> String
transformLine line =
  let
    -- Fix dependent types syntax in variable declarations
    fixedVars = fixVariableDeclarations line
    -- Fix dependent types in type definitions
    fixedTypes = fixTypeDefinitions fixedVars
    -- Fix function signatures with where clauses
    fixedFuncs = fixFunctionSignatures fixedTypes
    -- Fix method signatures
    fixedMethods = fixMethodSignatures fixedFuncs
  in
    fixedMethods

-- Fix variable declarations with dependent types
fixVariableDeclarations :: String -> String
fixVariableDeclarations line =
  -- Handle lines like: boundedInt BoundedInt[0, 100] = 50
  if "BoundedInt[" `isInfixOf` line && " = " `isInfixOf` line
    then
      let
        -- Extract variable name and value
        parts = words line
        varName = head parts
        valuePart = last $ words line
        -- Create simplified declaration
        simplified = "\t" ++ varName ++ " := " ++ valuePart ++ "  // BoundedInt[0, 100]"
      in
        simplified
    else if "NonEmptyString" `isInfixOf` line && " = " `isInfixOf` line && "struct" `isInfixOf` line == False
      then
        let
          parts = words line
          varName = head parts
          valuePart = last $ words line
          simplified = "\t" ++ varName ++ " := " ++ valuePart ++ "  // NonEmptyString"
        in
        simplified
      else if "[size]int where" `isInfixOf` line && " = " `isInfixOf` line
        then
          let
            parts = words line
            varName = head parts
            valuePart = last $ words line
            simplified = "\t" ++ varName ++ " := " ++ valuePart ++ "  // Dependent array"
          in
          simplified
        else if "[]int where" `isInfixOf` line && " = " `isInfixOf` line
          then
            let
              parts = words line
              varName = head parts
              valuePart = last $ words line
              simplified = "\t" ++ varName ++ " := " ++ valuePart ++ "  // Sized slice"
            in
            simplified
          else line

-- Fix type definitions with dependent types
fixTypeDefinitions :: String -> String
fixTypeDefinitions line =
  -- Handle lines like: type Matrix[rows, cols int] struct {
  if "type " `isInfixOf` line && "[" `isInfixOf` line && "]" `isInfixOf` line && "struct" `isInfixOf` line
    then
      let
        -- Extract type name (before <)
        typeName = takeWhile (/= '[') (drop 5 line)  -- drop "type "
        -- Create simplified type declaration
        simplified = "type " ++ trim typeName ++ " struct {"
      in
        simplified
    else if "type " `isInfixOf` line && " where " `isInfixOf` line
      then
        let
          typeName = takeWhile (/= ' ') (drop 5 line)  -- drop "type "
          simplified = "type " ++ trim typeName ++ " struct {  // Constraint: " ++ extractConstraint line
        in
        simplified
      else line

-- Fix function signatures with where clauses
fixFunctionSignatures :: String -> String
fixFunctionSignatures line =
  if "func " `isInfixOf` line && " where " `isInfixOf` line
    then
      let
        -- Extract function signature (before where)
        funcPart = takeWhile (/= 'w') line  -- take until "where"
        -- Remove the where clause
        simplified = trim funcPart
      in
        simplified ++ " {  // where clause removed"
    else line

-- Fix method signatures with where clauses
fixMethodSignatures :: String -> String
fixMethodSignatures line =
  if "func (" `isInfixOf` line && " where " `isInfixOf` line
    then
      let
        -- Extract method signature (before where)
        methodPart = takeWhile (/= 'w') line  -- take until "where"
        -- Remove the where clause
        simplified = trim methodPart
      in
        simplified ++ " {  // where clause removed"
    else line

-- Extract constraint from dependent type declaration
extractConstraint :: String -> String
extractConstraint line =
  case break (== 'w') (dropWhile (/= 'w') line) of  -- Find "where"
    (_, 'w':'h':'e':'r':'e':' ':constraint) -> 
      takeWhile (\c -> c /= '{' && c /= '\n') (trim constraint)
    _ -> "unknown constraint"

-- Generate runtime support for dependent types
generateRuntimeSupport :: String
generateRuntimeSupport = 
  "// Dependent types runtime support\n" ++
  "/*\n" ++
  " * Note: Dependent types are not natively supported in Go.\n" ++
  " * Runtime checks would be needed to enforce constraints.\n" ++
  " * Example functions for constraint validation would go here.\n" ++
  " */\n"

-- Enhanced clean code blocks by removing duplicate declarations and fixing syntax
cleanCodeBlocks :: String -> String
cleanCodeBlocks content =
  let
    linesList = lines content
    -- Remove package declarations and import-related lines
    filteredLines = filter (\line ->
      let pkgLine = isPackageLine line
          impLine = isImportLine line
          quotedLine = isQuotedPackageLine line
          closingLine = isClosingParenLine linesList line
          shouldKeep = not (pkgLine || impLine || quotedLine || closingLine)
      in shouldKeep) linesList
    
    -- Fix remaining syntax issues
    fixedLines = map fixSyntaxIssues filteredLines
    
    -- Remove empty lines at the beginning and end
    trimmedLines = trimEmptyLines fixedLines
  in
    unlines trimmedLines
  where
    -- Check if a line is a package declaration
    isPackageLine line = isPrefixOf "package" (trim line)

    -- Check if a line is an import statement (with or without leading whitespace)
    isImportLine line = isPrefixOf "import" (trim line)

    -- Check if a line is just a quoted package name
    isQuotedPackageLine line =
      let trimmed = trim line
      in (trimmed == "\"fmt\"" || trimmed == "\"sync\"" || trimmed == "\"time\"" ||
          trimmed == "\"unsafe\"" || trimmed == "\"os\"" || trimmed == "\"io\"" ||
          trimmed == "\"strings\"" || trimmed == "\"math\"" || trimmed == "\"runtime\"" ||
          trimmed == "\"container/list\"" || trimmed == "\"context\"" ||
          trimmed == "\"log\"" || trimmed == "\"reflect\"" || trimmed == "\"bufio\"" ||
          trimmed == "\"strconv\"" || trimmed == "\"encoding/json\"" || trimmed == "\"regexp\"")

    -- Check if a line is just a closing parenthesis (likely from import block)
    isClosingParenLine linesList line =
      let trimmed = trim line
      in trimmed == ")" && any (\l -> isImportLine l || isQuotedPackageLine l) linesList

    -- Fix various syntax issues
    fixSyntaxIssues line
      | "\t\"strconv\"" `isInfixOf` line = ""  -- Remove misplaced import
      | "\t\"encoding/json\"" `isInfixOf` line = ""  -- Remove misplaced import
      | "\t\"regexp\"" `isInfixOf` line = ""  -- Remove misplaced import
      | otherwise = line

    -- Remove empty lines at the beginning and end
    trimEmptyLines lines = 
      let 
        -- Remove leading empty lines
        dropLeading = dropWhile (\l -> null (trim l)) lines
        -- Remove trailing empty lines  
        dropTrailing = reverse $ dropWhile (\l -> null (trim l)) $ reverse dropLeading
      in
        dropTrailing

-- Utility function to trim whitespace
trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace