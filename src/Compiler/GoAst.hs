{-# LANGUAGE RecordWildCards #-}

module Compiler.GoAst
  ( GoModule(..)
  , PackageDecl(..)
  , ImportDecl(..)
  , GoDecl(..)
  , FuncDecl(..)
  , TypeDecl(..)
  , VarDecl(..)
  , ConstDecl(..)
  , StatementBlock(..)
  , RawBlock(..)
  , parseGoModule
  , renderGoModule
  , isMainFunction
  , flattenDeclLines
  ) where

import Data.Char (isSpace)
import Data.List (dropWhileEnd, intercalate, isPrefixOf, partition)

-- | In-memory representation of a Go source file.
data GoModule = GoModule
  { gmBuildTags :: [String]
  , gmPackage   :: Maybe PackageDecl
  , gmImports   :: [ImportDecl]
  , gmDecls     :: [GoDecl]
  } deriving (Eq, Show)

newtype PackageDecl = PackageDecl { packageName :: String }
  deriving (Eq, Show)

data ImportDecl = ImportDecl
  { importAlias :: Maybe String
  , importPath  :: String
  } deriving (Eq, Show)

data GoDecl
  = GoFunc FuncDecl
  | GoType TypeDecl
  | GoVar VarDecl
  | GoConst ConstDecl
  | GoStatement StatementBlock
  | GoRaw RawBlock
  deriving (Eq, Show)

newtype FuncDecl = FuncDecl { funcLines :: [String] }
  deriving (Eq, Show)

data TypeDecl = TypeDecl
  { typeLines   :: [String]
  , typeIsGroup :: Bool
  } deriving (Eq, Show)

data VarDecl = VarDecl
  { varLines   :: [String]
  , varIsGroup :: Bool
  } deriving (Eq, Show)

data ConstDecl = ConstDecl
  { constLines   :: [String]
  , constIsGroup :: Bool
  } deriving (Eq, Show)

newtype StatementBlock = StatementBlock { statementLines :: [String] }
  deriving (Eq, Show)

newtype RawBlock = RawBlock { rawLines :: [String] }
  deriving (Eq, Show)

-------------------------------------------------------------------------------
-- Parsing
-------------------------------------------------------------------------------

-- | Parse a Go module from a list of source lines. The parser only reasons
-- about top-level structure and preserves all original text for round-tripping.
parseGoModule :: [String] -> Either String GoModule
parseGoModule lines0 = do
  let (leading, rest) = spanLeading lines0
      (buildTagLines, leadingRemainder) = partition isBuildTagLine leading
      leadingDecls = if null leadingRemainder then [] else [GoRaw (RawBlock leadingRemainder)]
  case rest of
    [] -> do
      decls <- parseDecls []
      pure GoModule
            { gmBuildTags = buildTagLines
            , gmPackage   = Nothing
            , gmImports   = []
            , gmDecls     = leadingDecls ++ decls
            }
    rs@(line:after)
      | isPackageLine line -> do
          let pkgDecl = PackageDecl (parsePackageName line)
              afterPkg = dropWhile isBlankLine after
              (imports, afterImports) = parseImportSection afterPkg
          decls <- parseDecls afterImports
          pure GoModule
                { gmBuildTags = buildTagLines
                , gmPackage   = Just pkgDecl
                , gmImports   = imports
                , gmDecls     = leadingDecls ++ decls
                }
      | otherwise -> do
          decls <- parseDecls rs
          pure GoModule
                { gmBuildTags = buildTagLines
                , gmPackage   = Nothing
                , gmImports   = []
                , gmDecls     = leadingDecls ++ decls
                }
  where
    spanLeading = takeWhileInclusive isPreambleLine

    isPreambleLine line =
      let t = trim line
      in null t || isCommentLine t

    takeWhileInclusive p = go []
      where
        go acc [] = (reverse acc, [])
        go acc xs@(y:ys)
          | p y = go (y:acc) ys
          | otherwise = (reverse acc, xs)

-- Parse zero or more import declarations at the top of a file.
parseImportSection :: [String] -> ([ImportDecl], [String])
parseImportSection = go [] . dropWhile isBlankLine
  where
    go acc [] = (reverse acc, [])
    go acc xs@(x:rest)
      | not (isImportStart x) = (reverse acc, xs)
      | isImportGroupLine x =
          let (groupLines, remaining) = consumeImportGroup rest
              specs = mapMaybe (parseImportSpec . trim) (filter (not . isBlankLine) groupLines)
          in go (reverse specs ++ acc) (dropWhile isBlankLine remaining)
      | otherwise =
          case parseImportSpec (trim x) of
            Nothing   -> go acc rest
            Just spec -> go (spec : acc) (dropWhile isBlankLine rest)

    consumeImportGroup ys =
      let (inside, remainder) = spanUntilClosingParen 1 ys
      in (inside, remainder)

    isImportGroupLine line = trim line == "import ("

    mapMaybe f = foldr step []
      where
        step x acc = case f x of
          Nothing -> acc
          Just y  -> y : acc

-- Parse remaining top-level declarations/statements.
parseDecls :: [String] -> Either String [GoDecl]
parseDecls = go []
  where
    go acc [] = pure (reverse acc)
    go acc ls@(line:_rest)
      | isBlankLine line =
          let (blankRun, remaining) = span isBlankLine ls
          in go (GoRaw (RawBlock blankRun) : acc) remaining
      | isTypeGroupStart line = do
          (block, remaining) <- consumeParensBlock ls
          go (GoType (TypeDecl block True) : acc) remaining
      | isVarGroupStart line = do
          (block, remaining) <- consumeParensBlock ls
          go (GoVar (VarDecl block True) : acc) remaining
      | isConstGroupStart line = do
          (block, remaining) <- consumeParensBlock ls
          go (GoConst (ConstDecl block True) : acc) remaining
      | isTypeDeclStart line = do
          (block, remaining) <- consumeBracesBlock ls
          go (GoType (TypeDecl block False) : acc) remaining
      | isVarDeclStart line = do
          (block, remaining) <- consumeSimpleBlock ls
          go (GoVar (VarDecl block False) : acc) remaining
      | isConstDeclStart line = do
          (block, remaining) <- consumeSimpleBlock ls
          go (GoConst (ConstDecl block False) : acc) remaining
      | isFuncDeclStart line = do
          (block, remaining) <- consumeBracesBlock ls
          go (GoFunc (FuncDecl block) : acc) remaining
      | isCommentLine (trim line) =
          let (commentBlock, remaining) = span isCommentLine ls
          in go (GoRaw (RawBlock commentBlock) : acc) remaining
      | otherwise =
          let (stmBlock, remaining) = consumeStatementBlock ls
          in go (GoStatement (StatementBlock stmBlock) : acc) remaining

consumeParensBlock :: [String] -> Either String ([String], [String])
consumeParensBlock [] = Left "Unexpected end of input while parsing parenthesised block"
consumeParensBlock (x:xs) =
  let initialDepth = parenDelta x
  in if initialDepth <= 0
       then pure ([x], xs)
       else gather initialDepth [x] xs
  where
    gather depth acc []
      | depth <= 0 = pure (reverse acc, [])
      | otherwise  = Left "Unbalanced parentheses in declaration block"
    gather depth acc (y:ys)
      | depth' <= 0 = pure (reverse (y:acc), ys)
      | otherwise   = gather depth' (y:acc) ys
      where
        depth' = depth + parenDelta y

consumeBracesBlock :: [String] -> Either String ([String], [String])
consumeBracesBlock [] = Left "Unexpected end of input while parsing braces block"
consumeBracesBlock (x:xs) =
  let initialDepth = braceDelta x
  in if initialDepth <= 0
       then pure ([x], xs)
       else gather initialDepth [x] xs
  where
    gather _ _ [] = Left "Unbalanced braces in declaration block"
    gather depth acc (y:ys)
      | depth' <= 0 = pure (reverse (y:acc), ys)
      | otherwise   = gather depth' (y:acc) ys
      where
        depth' = depth + braceDelta y

consumeSimpleBlock :: [String] -> Either String ([String], [String])
consumeSimpleBlock [] = Left "Unexpected end of input while parsing declaration"
consumeSimpleBlock (x:xs) =
  let (continuation, rest) = span continue xs
  in pure (x : continuation, rest)
  where
    continue line =
      let t = trim line
      in not (null t) && not (isPackageLine line || isImportStart line || isDeclStart line)

consumeStatementBlock :: [String] -> ([String], [String])
consumeStatementBlock [] = ([], [])
consumeStatementBlock (x:xs) =
  let (body, rest) = span continue xs
  in (x : body, rest)
  where
    continue line =
      let t = trim line
      in not (null t) && not (isDeclStart line || isImportStart line || isPackageLine line)

spanUntilClosingParen :: Int -> [String] -> ([String], [String])
spanUntilClosingParen _ [] = ([], [])
spanUntilClosingParen depth (x:xs)
  | depth' <= 0 = ([x], xs)
  | otherwise   = let (ys, rest) = spanUntilClosingParen depth' xs in (x:ys, rest)
  where
    depth' = depth + parenDelta x

-------------------------------------------------------------------------------
-- Rendering
-------------------------------------------------------------------------------

renderGoModule :: GoModule -> String
renderGoModule GoModule{..} =
  let pkgLine = "package " ++ packageName (maybe (PackageDecl "main") id gmPackage)
      importSection = renderImports gmImports
      declLines = concatMap renderDecl gmDecls
      buildTags = case gmBuildTags of
        []   -> []
        tags -> tags ++ [""]
      pieces = concat
        [ buildTags
        , [pkgLine]
        , if null importSection then [] else "" : importSection
        , if null declLines then [] else "" : declLines
        ]
      normalised = dropTrailingWhile null pieces
  in intercalate "\n" normalised ++ "\n"

renderImports :: [ImportDecl] -> [String]
renderImports [] = []
renderImports [ImportDecl alias path] = ["import " ++ renderSpec alias path]
renderImports imports =
  "import (" : map renderLine imports ++ [")"]
  where
    renderLine decl = "    " ++ renderSpec (importAlias decl) (importPath decl)

renderSpec :: Maybe String -> String -> String
renderSpec alias path =
  let prefix = maybe "" (++ " ") alias
  in prefix ++ show path

renderDecl :: GoDecl -> [String]
renderDecl (GoFunc (FuncDecl ls))            = ls
renderDecl (GoType (TypeDecl ls _))          = ls
renderDecl (GoVar (VarDecl ls _))            = ls
renderDecl (GoConst (ConstDecl ls _))        = ls
renderDecl (GoStatement (StatementBlock ls)) = ls
renderDecl (GoRaw (RawBlock ls))             = ls

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

-- | Determine whether a function declaration corresponds to the Go entry point.
isMainFunction :: FuncDecl -> Bool
isMainFunction (FuncDecl [])     = False
isMainFunction (FuncDecl (l : _)) =
  let t = trim l
  in "func main" `isPrefixOf` t && not ("func main_test" `isPrefixOf` t)

-- | Collect all raw source lines contained within a declaration tree.
flattenDeclLines :: GoDecl -> [String]
flattenDeclLines (GoFunc (FuncDecl ls))            = ls
flattenDeclLines (GoType (TypeDecl ls _))          = ls
flattenDeclLines (GoVar (VarDecl ls _))            = ls
flattenDeclLines (GoConst (ConstDecl ls _))        = ls
flattenDeclLines (GoStatement (StatementBlock ls)) = ls
flattenDeclLines (GoRaw (RawBlock ls))             = ls

-------------------------------------------------------------------------------
-- Predicates & utility functions
-------------------------------------------------------------------------------

isPackageLine :: String -> Bool
isPackageLine line = "package " `isPrefixOf` trim line

isImportStart :: String -> Bool
isImportStart line = "import" `isPrefixOf` trim line

isTypeGroupStart :: String -> Bool
isTypeGroupStart line = "type (" `isPrefixOf` trim line

isVarGroupStart :: String -> Bool
isVarGroupStart line = "var (" `isPrefixOf` trim line

isConstGroupStart :: String -> Bool
isConstGroupStart line = "const (" `isPrefixOf` trim line

isTypeDeclStart :: String -> Bool
isTypeDeclStart line = "type " `isPrefixOf` trim line

isVarDeclStart :: String -> Bool
isVarDeclStart line = "var " `isPrefixOf` trim line

isConstDeclStart :: String -> Bool
isConstDeclStart line = "const " `isPrefixOf` trim line

isFuncDeclStart :: String -> Bool
isFuncDeclStart line = "func " `isPrefixOf` trim line

isDeclStart :: String -> Bool
isDeclStart line = any ($ line)
  [ isTypeGroupStart
  , isVarGroupStart
  , isConstGroupStart
  , isTypeDeclStart
  , isVarDeclStart
  , isConstDeclStart
  , isFuncDeclStart
  ]

isCommentLine :: String -> Bool
isCommentLine line = "//" `isPrefixOf` trim line

isBuildTagLine :: String -> Bool
isBuildTagLine line =
  let t = trim line
  in "//go:build" `isPrefixOf` t || "// +build" `isPrefixOf` t

trim :: String -> String
trim = dropWhile isSpace . dropWhileEnd isSpace

isBlankLine :: String -> Bool
isBlankLine = null . trim

parsePackageName :: String -> String
parsePackageName line =
  let rest = drop (length "package") (trim line)
  in takeWhile (not . isSpace) (trim rest)

parseImportSpec :: String -> Maybe ImportDecl
parseImportSpec rawLine =
  case dropWhile (== "import") (words rawLine) of
    [alias, path]
      | isQuoted path -> Just (ImportDecl (Just alias) (stripQuotes path))
    [path]
      | isQuoted path -> Just (ImportDecl Nothing (stripQuotes path))
    _ -> Nothing
  where
    isQuoted ('"':xs) =
      case unsnoc xs of
        Just (_, '"') -> True
        _              -> False
    isQuoted _ = False

    stripQuotes ('"':xs) =
      case unsnoc xs of
        Just (inner, '"') -> inner
        _                 -> xs
    stripQuotes s = s

    unsnoc [] = Nothing
    unsnoc (y:ys) =
      case ys of
        [] -> Just ([], y)
        _  -> do
          (rest, lastChar) <- unsnoc ys
          pure (y:rest, lastChar)

parenDelta :: String -> Int
parenDelta line = count '(' line - count ')' line

braceDelta :: String -> Int
braceDelta line = count '{' line - count '}' line

count :: Char -> String -> Int
count c = length . filter (== c)

-- | Drop trailing elements while a predicate holds.
dropTrailingWhile :: (a -> Bool) -> [a] -> [a]
dropTrailingWhile _ [] = []
dropTrailingWhile p xs = reverse (dropWhile p (reverse xs))
