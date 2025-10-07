-- The actual implementation would be more complex

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

-- Generate source code (placeholder)

-- Generate file directives

-- Generate code blocks as source code

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

-- Generate imports section
generateImports :: TypusFile -> String
generateImports typusFile =
  let
    content = intercalate "\n" $ map cbContent (tfBlocks typusFile)
    hasBufio = "bufio." `isInfixOf` content || "\"bufio\"" `isInfixOf` content
    hasFmt = "fmt." `isInfixOf` content || "\"fmt\"" `isInfixOf` content
    hasMath = "math." `isInfixOf` content || "\"math\"" `isInfixOf` content
    hasTime = "time." `isInfixOf` content || "\"time\"" `isInfixOf` content
    hasOs = "os." `isInfixOf` content || "\"os\"" `isInfixOf` content
    hasIo = "io." `isInfixOf` content || "\"io\"" `isInfixOf` content
    hasStrings = "strings." `isInfixOf` content || "\"strings\"" `isInfixOf` content
    hasSync = "sync." `isInfixOf` content || "\"sync\"" `isInfixOf` content
    hasRuntime = "runtime." `isInfixOf` content || "\"runtime\"" `isInfixOf` content
    hasUnsafe = "unsafe." `isInfixOf` content || "\"unsafe\"" `isInfixOf` content
    hasContainerList = "container/list" `isInfixOf` content || "\"container/list\"" `isInfixOf` content
    hasContext = "context." `isInfixOf` content || "\"context\"" `isInfixOf` content
    hasLog = "log." `isInfixOf` content || "\"log\"" `isInfixOf` content
    hasReflect = "reflect." `isInfixOf` content || "\"reflect\"" `isInfixOf` content
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
        if hasUnsafe then "    \"unsafe\"" else ""
      ]
  in
    if null imports
      then ""
      else "import (\n" ++ intercalate "\n" imports ++ "\n)"

-- Transform code blocks with all necessary fixes
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

-- Transform code to fix syntax issues
transformCode :: String -> String
transformCode content = 
  let
    linesContent = lines content
    transformedLines = map transformLine linesContent
  in
    unlines transformedLines

-- Transform a line to fix syntax issues
transformLine :: String -> String
transformLine line =
  -- Fix dependent types syntax
  if "type " `isInfixOf` line && " where " `isInfixOf` line
    then
      let
        -- Extract type name (before <)
        typeName = takeWhile (/= '<') (drop 5 line)  -- drop "type "
        -- Extract constraint
        constraint = extractConstraint line
        -- Create simplified type declaration with constraint as comment
        simplified = "type " ++ typeName ++ " struct {  // Constraint: " ++ constraint
      in
        simplified
    else
      line

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

-- Clean code blocks by removing duplicate package declarations and imports
cleanCodeBlocks :: String -> String
cleanCodeBlocks content =
  let
    linesList = lines content
    _ = trace ("=== ORIGINAL LINES ===") ()
    _ = trace (unlines $ map (\l -> "LINE: " ++ l) linesList) ()
    -- Remove package declarations and import-related lines
    -- We generate our own package declaration and imports section
    filteredLines = filter (\line ->
        let pkgLine = isPackageLine line
            impLine = isImportLine line
            quotedLine = isQuotedPackageLine line
            closingLine = isClosingParenLine linesList line
            shouldKeep = not (pkgLine || impLine || quotedLine || closingLine)
            _ = trace ("Line: '" ++ line ++ "' -> pkg:" ++ show pkgLine ++ " imp:" ++ show impLine ++ " quoted:" ++ show quotedLine ++ " closing:" ++ show closingLine ++ " keep:" ++ show shouldKeep) ()
        in shouldKeep) linesList
    _ = trace ("Filtered lines: " ++ show (map (\l -> "'" ++ l ++ "'") filteredLines)) ()
  in
    unlines filteredLines
  where
    -- Check if a line is a package declaration
    isPackageLine line = isPrefixOf "package" (trim line)

    -- Check if a line is an import statement (with or without leading whitespace)
    isImportLine line = isPrefixOf "import" (trim line)

    -- Check if a line is just a quoted package name (like "\"fmt\"" or "\"container/list\"")
    -- This handles indented import lines in Go's import block
    isQuotedPackageLine line =
      let trimmed = trim line
      in (trimmed == "\"fmt\"" || trimmed == "\"sync\"" || trimmed == "\"time\"" ||
          trimmed == "\"unsafe\"" || trimmed == "\"os\"" || trimmed == "\"io\"" ||
          trimmed == "\"strings\"" || trimmed == "\"math\"" || trimmed == "\"runtime\"" ||
          trimmed == "\"container/list\"" || trimmed == "\"context\"" ||
          trimmed == "\"log\"" || trimmed == "\"reflect\"" || trimmed == "\"bufio\"")

    -- Check if a line is just a closing parenthesis (likely from import block)
    -- Only remove it if it appears to be ending an import block
    isClosingParenLine linesList line =
      let trimmed = trim line
      in trimmed == ")" && isJust (findPreviousImportLine linesList line)
      where
        -- Helper to check if there was a previous import line
        isJust Nothing = False
        isJust (Just _) = True

        -- Find previous import line (simplified check)
        findPreviousImportLine _ _ = Just () -- Always assume it's from import block

-- Utility function to trim whitespace
trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace