module Ownership.Parser
  ( Name
  , AssignOp(..)
  , UnaryOp(..)
  , Directive(..)
  , Expr(..)
  , Stmt(..)
  , Program(..)
  , parseProgram
  ) where

import Data.Char (isSpace)
import Data.Maybe (mapMaybe)
import qualified Data.Map.Strict as Map

import Ownership.Common.Lexer (Pos(..), Token(..), TokenKind(..))
import Ownership.Lexer (Keyword(..), Sym(..), OwnershipToken)

--------------------------------------------------------------------------------
-- Parser data types
--------------------------------------------------------------------------------

type Name = String

data AssignOp = OpAssign | OpWalrus deriving (Eq, Show)

data UnaryOp = UBorrow | UMutBorrow deriving (Eq, Show)

data Directive = Directive (Map.Map String String) deriving (Eq, Show)

data Expr
  = EIdent Name Pos
  | ECall Name [Expr] Pos
  | EUnary UnaryOp Expr Pos
  | ELitStr String Pos
  | ELitNum String Pos
  | EUnknown [OwnershipToken] Pos
  deriving (Eq, Show)

getExprPos :: Expr -> Pos
getExprPos e = case e of
  EIdent _ p   -> p
  ECall _ _ p  -> p
  EUnary _ _ p -> p
  ELitStr _ p  -> p
  ELitNum _ p  -> p
  EUnknown _ p -> p

data Stmt
  = SVarDecl Name (Maybe Expr) Pos
  | SLetDecl Name (Maybe Expr) Pos
  | SAssignStmt [Name] AssignOp Expr Pos
  | SExpr Expr Pos
  | SBlock [Stmt] Pos
  | SFunc [Stmt] Pos
  | SFor [Stmt] Pos
  | SDirectiveLine Directive Pos
  | SDirectiveBlock Directive [Stmt] Pos
  deriving (Eq, Show)

data Program = Program [Stmt] deriving (Eq, Show)

--------------------------------------------------------------------------------
-- Public entry point
--------------------------------------------------------------------------------

parseProgram :: [OwnershipToken] -> Program
parseProgram toks = Program (parseManyTop toks)

--------------------------------------------------------------------------------
-- Internal parsing helpers
--------------------------------------------------------------------------------

parseManyTop :: [OwnershipToken] -> [Stmt]
parseManyTop = go (0 :: Int)
  where
    go depth xs = case skipNL xs of
      [] -> []
      ts ->
        if depth > 1000
        then []
        else let (st, rest) = parseStmt ts
             in st : go (depth + 1) rest

parseStmt :: [OwnershipToken] -> (Stmt, [OwnershipToken])
parseStmt xs0 =
  let xs = skipNL xs0
  in case xs of
    (t:rest)
      | Just (cmt, True, p) <- tokComment t
      , isDirectiveText cmt ->
          let dir = parseDirectiveText cmt
          in (SDirectiveLine dir p, rest)

    (Token (TSym SLBrace) pOpen : rest) ->
      let (maybeDir, afterDir) = parseOptionalLeadingDirective rest
          (body, rest') = parseBlockBody afterDir
      in case maybeDir of
          Just dir -> (SDirectiveBlock dir body pOpen, rest')
          Nothing  -> (SBlock body pOpen, rest')

    (t:rest) | isKw KwFor t ->
      let (_, afterSig) = consumeUntilLBrace rest
          (blockStmt, rest') = case afterSig of
            [] -> (SBlock [] (Pos 0 0), [])
            (t':rest'') -> parseStmt (t' : rest'')
      in case blockStmt of
         SBlock body p -> (SFor body p, rest')
         SDirectiveBlock dir body p -> (SFor [SDirectiveBlock dir body p] p, rest')
         _ -> (blockStmt, rest')

    (t:rest) | isKw KwType t || isKw KwInterface t ->
      let (_, afterSig) = consumeUntilLBrace rest
          rest' = skipBalancedBlock afterSig
      in (SExpr (EUnknown [] (Pos 0 0)) (Pos 0 0), rest')

    (t:rest) | isKw KwFunc t ->
      let (_, afterSig) = consumeUntilLBrace rest
          (blockStmt, rest') = case afterSig of
            [] -> (SBlock [] (Pos 0 0), [])
            (t':rest'') -> parseStmt (t' : rest'')
      in case blockStmt of
         SBlock body p -> (SFunc body p, rest')
         SDirectiveBlock dir body p -> (SFunc [SDirectiveBlock dir body p] p, rest')
         _ -> (blockStmt, rest')

    (t:rest) | isKw KwIf t ->
      let (condTokens, afterCond) = consumeUntilLBrace rest
          initStmts = extractLeadingInitializers condTokens
          (blockStmt, rest') = case afterCond of
            [] -> (SBlock [] (Pos 0 0), [])
            (t':rest'') -> parseStmt (t' : rest'')
      in (prependInitializers initStmts blockStmt, rest')

    (t1:t2:rest)
      | isKw KwVar t1
      , Just (name, pName) <- tokId t2 ->
          let (mInit, rest') = parseVarDeclWithOptionalType rest
          in (SVarDecl name mInit pName, rest')

    (t1:t2:rest)
      | isKw KwConst t1
      , Just (name, pName) <- tokId t2 ->
          let (mInit, rest') = parseVarDeclWithOptionalType rest
          in (SVarDecl name mInit pName, rest')

    (t1:t2:rest)
      | isKw KwLet t1
      , Just (name, pName) <- tokId t2 ->
          let (mInit, rest') = parseOptionalInit rest
          in (SLetDecl name mInit pName, rest')

    xsWithId@(Token (TId _) _ : _) ->
      case parseIdentifierList xsWithId of
        Just (names, pName, Token (TSym SWalrus) _ : restAfterOp) ->
          let (rhs, rest') = parseExprUntilEnd restAfterOp
          in (SAssignStmt names OpWalrus rhs pName, rest')
        Just (names, pName, Token (TSym SAssign) _ : restAfterOp) ->
          let (rhs, rest') = parseExprUntilEnd restAfterOp
          in (SAssignStmt names OpAssign rhs pName, rest')
        _ -> parseAsExpr xsWithId

    (Token (TSym SSemicolon) _ : rest) ->
      let (nextStmt, rest') = case skipNL rest of
            [] -> (SExpr (EUnknown [] (Pos 0 0)) (Pos 0 0), [])
            ts -> parseStmt ts
      in (nextStmt, rest')

    _ -> parseAsExpr xs
  where
    parseAsExpr ts =
      let (expr, restTokens) = parseExprUntilEnd ts
          pos = getExprPos expr
      in (SExpr expr pos, restTokens)

parseBlockBody :: [OwnershipToken] -> ([Stmt], [OwnershipToken])
parseBlockBody xs = go [] xs (0 :: Int)
  where
    go acc ts depth = case skipNL ts of
      (t:rest) | isSym SRBrace t -> (reverse acc, rest)
      [] -> (reverse acc, [])
      ts' ->
        if depth > 1000
          then (reverse acc, ts')
          else let (st, rest') = parseStmt ts'
               in go (st:acc) rest' (depth + 1)

skipBalancedBlock :: [OwnershipToken] -> [OwnershipToken]
skipBalancedBlock = go (0 :: Int) False
  where
    go _ _ [] = []
    go depth started (tok:rest)
      | isSym SLBrace tok = go (depth + 1) True rest
      | isSym SRBrace tok =
          let depth' = max 0 (depth - 1)
          in if depth' == 0 && started
                then rest
                else go depth' started rest
      | otherwise = go depth started rest

consumeUntilLBrace :: [OwnershipToken] -> ([OwnershipToken], [OwnershipToken])
consumeUntilLBrace xs = go [] xs
  where
    go acc (t:rest)
      | isSym SLBrace t = (reverse acc, t:rest)
      | otherwise = go (t:acc) rest
    go acc [] = (reverse acc, [])

parseOptionalLeadingDirective :: [OwnershipToken] -> (Maybe Directive, [OwnershipToken])
parseOptionalLeadingDirective xs0 =
  let xs = skipNL xs0
  in case xs of
     (t:rest)
       | Just (cmt, True, _) <- tokComment t
       , isDirectiveText cmt -> (Just (parseDirectiveText cmt), rest)
     _ -> (Nothing, xs0)

parseOptionalInit :: [OwnershipToken] -> (Maybe Expr, [OwnershipToken])
parseOptionalInit xs0 =
  let xs = skipNL xs0
  in case xs of
      (Token (TSym SAssign) _ : rest) ->
        let (e, rest') = parseExprUntilEnd rest
        in (Just e, rest')
      _ -> (Nothing, xs0)

parseVarDeclWithOptionalType :: [OwnershipToken] -> (Maybe Expr, [OwnershipToken])
parseVarDeclWithOptionalType xs0 =
  let xs = skipNL xs0
  in case xs of
      (Token (TSym SAssign) _ : rest) ->
        let (e, rest') = parseExprUntilEnd rest
        in (Just e, rest')
      (t:rest) ->
        case t of
          Token (TId _) _ ->
            let restAfterType = skipNL rest
            in case restAfterType of
                 (Token (TSym SAssign) _ : restAfterAssign) ->
                   let (e, rest') = parseExprUntilEnd restAfterAssign
                   in (Just e, rest')
                 _ -> (Nothing, xs0)
          _ -> (Nothing, xs0)
      _ -> (Nothing, xs0)

  parseIdentifierList :: [OwnershipToken] -> Maybe ([Name], Pos, [OwnershipToken])
  parseIdentifierList (Token (TId name) pos : rest) = Just (collect [name] pos rest)
        where
          collect names firstPos (Token (TSym SComma) _ : Token (TId next) _ : xs) = collect (names ++ [next]) firstPos xs
          collect names firstPos xs = (names, firstPos, xs)
      parseIdentifierList _ = Nothing

  parseSimpleWalrus :: [OwnershipToken] -> Maybe ([Name], Expr, Pos)
  parseSimpleWalrus tokens = do
        (names, pos, restAfterNames) <- parseIdentifierList tokens
        case restAfterNames of
          Token (TSym SWalrus) _ : rhsTokens ->
            let (expr, _) = parseExprUntilEnd rhsTokens
            in Just (names, expr, pos)
          _ -> Nothing

  extractLeadingInitializers :: [OwnershipToken] -> [Stmt]
      extractLeadingInitializers tokens =
        case break (isSym SSemicolon) tokens of
          (beforeSemi, _) ->
            case parseSimpleWalrus beforeSemi of
              Just (names, expr, pos) -> [SAssignStmt names OpWalrus expr pos]
              Nothing -> []

  parseExprUntilEnd :: [OwnershipToken] -> (Expr, [OwnershipToken])
      parseExprUntilEnd xs =
        let (ts, rest) = takeExprTokens xs
        in (tokensToExpr ts, rest)

  prependInitializers :: [Stmt] -> Stmt -> Stmt
  prependInitializers [] stmt = stmt
      prependInitializers initStmts (SBlock body p) = SBlock (initStmts ++ body) p
      prependInitializers initStmts (SDirectiveBlock dir body p) =
        SBlock (initStmts ++ [SDirectiveBlock dir body p]) p
      prependInitializers initStmts stmt =
        SBlock (initStmts ++ [stmt]) (stmtPos stmt)

  stmtPos :: Stmt -> Pos
      stmtPos st = case st of
        SVarDecl _ _ p -> p
        SLetDecl _ _ p -> p
        SAssignStmt _ _ _ p -> p
        SExpr _ p -> p
        SBlock _ p -> p
        SFunc _ p -> p
        SFor _ p -> p
        SDirectiveLine _ p -> p
        SDirectiveBlock _ _ p -> p


takeExprTokens = go (0 :: Int) (0 :: Int) (0 :: Int) []
  where
    stopTok t
      | isSym SNewline t   = True
      | isSym SSemicolon t = True
      | otherwise          = False

    go _ _ _ acc [] = (reverse acc, [])
    go paren bracket brace acc ts@(t:rest)
      | (stopTok t || isSym SRBrace t) && paren == 0 && bracket == 0 && brace == 0
          = (reverse acc, ts)
      | isSym SLParen t    = go (paren + 1) bracket brace (t:acc) rest
      | isSym SRParen t    = go (max 0 (paren - 1)) bracket brace (t:acc) rest
      | isSym SLBracket t  = go paren (bracket + 1) brace (t:acc) rest
      | isSym SRBracket t  = go paren (max 0 (bracket - 1)) brace (t:acc) rest
      | isSym SLBrace t    = go paren bracket (brace + 1) (t:acc) rest
      | isSym SRBrace t    = go paren bracket (max 0 (brace - 1)) (t:acc) rest
      | otherwise          = go paren bracket brace (t:acc) rest

tokensToExpr :: [OwnershipToken] -> Expr
tokensToExpr [] = EUnknown [] (Pos 0 0)
tokensToExpr (t:ts) =
  case (t, ts) of
    (Token (TSym SAmp) p, Token (TKw KwMut) _ : Token (TId x) p2 : rest)
       | null rest -> EUnary UMutBorrow (EIdent x p2) p
    (Token (TSym SAmp) p, Token (TId x) p2 : rest)
       | null rest -> EUnary UBorrow (EIdent x p2) p
    (Token (TId f) p, Token (TSym SLParen) _ : more) ->
      case splitTopLevelArgs more of
        Just (argsTs, afterRParen) ->
          let rest = dropTrailingNoise afterRParen
              exprs = map tokensToExpr argsTs
          in if null rest
                then ECall f exprs p
                else EUnknown (t:ts) p
        _ -> EUnknown (t:ts) p
    (Token (TId base) p, Token (TSym SDot) _ : Token (TId meth) _ : Token (TSym SLParen) _ : more) ->
      case splitTopLevelArgs more of
        Just (argsTs, afterRParen)
          | null afterRParen ->
              let exprs = map tokensToExpr argsTs
              in ECall meth (EIdent base p : exprs) p
        _ -> EUnknown (t:ts) p
    (Token (TId x) p, []) -> EIdent x p
    (Token (TString s) p, []) -> ELitStr s p
    (Token (TNum n) p, [])    -> ELitNum n p
    (Token (TKw KwTrue) p, [])  -> ELitNum "1" p
    (Token (TKw KwFalse) p, []) -> ELitNum "0" p
    _ -> EUnknown (t:ts) (tkPos t)

--------------------------------------------------------------------------------
-- Directive parsing helpers
--------------------------------------------------------------------------------

isDirectiveText :: String -> Bool
isDirectiveText s =
  let s' = dropWhile isSpace s
  in take 1 s' == "!" || take 3 s' == "//!"

parseDirectiveText :: String -> Directive
parseDirectiveText s =
  let s1 = dropWhile isSpace s
      s2 = dropWhile (== '!') s1
      s3 = dropWhile isSpace s2
      pairs = splitOn ',' s3
      kvs = mapMaybe parseKV pairs
  in Directive (Map.fromList kvs)
  where
    parseKV chunk =
      let (k, v0) = break (== ':') chunk
      in case v0 of
        (':':rest) ->
          let k' = trim k
              v' = map toLowerStr (trim rest)
          in if null k' then Nothing else Just (k', v')
        _ -> Nothing

    toLowerStr c
      | 'A' <= c && c <= 'Z' = toEnum (fromEnum c + 32)
      | otherwise            = c

--------------------------------------------------------------------------------
-- Small lexical helpers
--------------------------------------------------------------------------------

skipNL :: [OwnershipToken] -> [OwnershipToken]
skipNL (Token (TSym SNewline) _:xs)   = skipNL xs
skipNL (Token (TSym SSemicolon) _:xs) = skipNL xs
skipNL xs = xs

isSym :: Sym -> OwnershipToken -> Bool
isSym s (Token (TSym s') _) = s == s'
isSym _ _ = False

isKw :: Keyword -> OwnershipToken -> Bool
isKw k (Token (TKw k') _) = k == k'
isKw _ _ = False

tokId :: OwnershipToken -> Maybe (String, Pos)
tokId (Token (TId s) p) = Just (s, p)
tokId _ = Nothing





tokComment :: OwnershipToken -> Maybe (String, Bool, Pos)
tokComment (Token (TComment s isLine) p) = Just (s, isLine, p)
tokComment _ = Nothing

--------------------------------------------------------------------------------
-- Token utilities reused by parser internals
--------------------------------------------------------------------------------

dropTrailingNoise :: [OwnershipToken] -> [OwnershipToken]
dropTrailingNoise = reverse . dropWhile isNoise . reverse
  where
    isNoise (Token (TComment _ _) _)    = True
    isNoise (Token (TSym SSemicolon) _) = True
    isNoise (Token (TSym SNewline) _)   = True
    isNoise _                           = False

splitTopLevelArgs :: [OwnershipToken] -> Maybe ([[OwnershipToken]], [OwnershipToken])
splitTopLevelArgs ts = go [] [] (0 :: Int) (0 :: Int) ts
  where
    go acc cur paren bracket xs = case xs of
      [] -> Nothing
      (t:rest)
        | isSym SRParen t && paren == 0 && bracket == 0
            -> Just (reverse (reverse cur : acc), rest)
        | isSym SComma t && paren == 0 && bracket == 0
            -> go (reverse cur : acc) [] paren bracket rest
        | isSym SLParen t   -> go acc (t:cur) (paren+1) bracket rest
        | isSym SRParen t   -> go acc (t:cur) (paren-1) bracket rest
        | isSym SLBracket t -> go acc (t:cur) paren (bracket+1) rest
        | isSym SRBracket t -> go acc (t:cur) paren (bracket-1) rest
        | otherwise         -> go acc (t:cur) paren bracket rest

--------------------------------------------------------------------------------
-- Generic helpers
--------------------------------------------------------------------------------

trim :: String -> String
trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse

splitOn :: Char -> String -> [String]
splitOn _ "" = []
splitOn ch xs =
  let (a,b) = break (== ch) xs
  in case b of
    []     -> [a]
    (_:ys) -> a : splitOn ch ys
