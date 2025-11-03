module Compiler.GoLexer
  ( GoToken(..)
  , GoTokenKind(..)
  , tokenizeGo
  , isWhitespaceToken
  , isCommentToken
  , isStringToken
  , isIdentifierToken
  ) where

import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)
import Data.List (isPrefixOf)

-- | Simple token representation for Go-like sources. Tokens retain the exact
-- text they were derived from so that the original source can be reconstructed
-- after transformations.
data GoToken = GoToken
  { tokenKind :: GoTokenKind
  , tokenText :: String
  } deriving (Eq, Show)

data GoTokenKind
  = TokIdentifier
  | TokKeyword
  | TokNumber
  | TokString
  | TokComment
  | TokOperator
  | TokSymbol
  | TokWhitespace
  | TokOther
  deriving (Eq, Show)

-- | Lexically classify a Go-like source string into a flat token stream.
-- The lexer is intentionally lightweight – it recognises identifiers, strings,
-- comments, operators and punctuation while leaving the text untouched. This
-- makes it suitable for downstream transformations that want to reason about
-- syntactic structure without building a full parser.
tokenizeGo :: String -> [GoToken]
tokenizeGo = go
  where
    go [] = []
    go input@(c:cs)
      -- Whitespace (including newlines)
      | isSpace c =
          let (ws, rest) = span isSpace input
          in GoToken TokWhitespace ws : go rest

      -- Line comments
      | "//" `isPrefixOf` input =
          let (commentBody, rest) = spanLineComment (drop 2 input)
          in GoToken TokComment ("//" ++ commentBody) : go rest

      -- Block comments (no nesting)
      | "/*" `isPrefixOf` input =
          let (commentBody, rest) = spanBlockComment (drop 2 input)
          in GoToken TokComment ("/*" ++ commentBody) : go rest

      -- String literals
      | c == '"' =
          let (strToken, rest) = spanString '"' cs
          in GoToken TokString ('"' : strToken) : go rest

      -- Character literals
      | c == '\'' =
          let (charToken, rest) = spanString '\'' cs
          in GoToken TokString ('\'' : charToken) : go rest

      -- Raw string literals
      | c == '`' =
          let (rawToken, rest) = spanRawString cs
          in GoToken TokString ('`' : rawToken) : go rest

      -- Identifiers / keywords
      | isIdentifierStart c =
          let (ident, rest) = span isIdentifierChar cs
              text = c : ident
              kind = if isKeyword text then TokKeyword else TokIdentifier
          in GoToken kind text : go rest

      -- Numbers (including floats)
      | isDigit c || (c == '.' && not (null cs) && isDigit (head cs)) =
          let (numberText, rest) = spanNumber input
          in GoToken TokNumber numberText : go rest

      -- Multi-character operators
      | Just op <- matchPrefix multiOperators input =
          GoToken TokOperator op : go (drop (length op) input)

      -- Multi-character symbols
      | Just sym <- matchPrefix multiSymbols input =
          GoToken TokSymbol sym : go (drop (length sym) input)

      -- Single-character operators
      | c `elem` singleOperators =
          GoToken TokOperator [c] : go cs

      -- Punctuation / structural symbols
      | c `elem` symbolChars =
          GoToken TokSymbol [c] : go cs

      -- Fallback
      | otherwise = GoToken TokOther [c] : go cs

-- ---------------------------------------------------------------------------
-- Token predicates
-- ---------------------------------------------------------------------------

isWhitespaceToken :: GoToken -> Bool
isWhitespaceToken GoToken{ tokenKind = TokWhitespace } = True
isWhitespaceToken _ = False

isCommentToken :: GoToken -> Bool
isCommentToken GoToken{ tokenKind = TokComment } = True
isCommentToken _ = False

isStringToken :: GoToken -> Bool
isStringToken GoToken{ tokenKind = TokString } = True
isStringToken _ = False

isIdentifierToken :: GoToken -> Bool
isIdentifierToken GoToken{ tokenKind = TokIdentifier } = True
isIdentifierToken _ = False

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

spanLineComment :: String -> (String, String)
spanLineComment [] = ([], [])
spanLineComment input =
  let (body, rest) = break (== '\n') input
  in case rest of
       []      -> (body, rest)
       (_:xs)  -> (body ++ "\n", xs)

spanBlockComment :: String -> (String, String)
spanBlockComment = go []
  where
    go acc [] = (reverse acc, [])
    go acc ('*':'/':rest) = (reverse ('/':'*':acc), rest)
    go acc (x:xs) = go (x:acc) xs

spanString :: Char -> String -> (String, String)
spanString delim = go []
  where
    go acc [] = (reverse acc, [])
    go acc (x:xs)
      | x == '\\' =
          case xs of
            []     -> (reverse (x:acc), [])
            (y:ys) -> go (y:x:acc) ys
      | x == delim = (reverse acc ++ [x], xs)
      | otherwise = go (x:acc) xs

spanRawString :: String -> (String, String)
spanRawString = go []
  where
    go acc [] = (reverse acc, [])
    go acc (x:xs)
      | x == '`' = (reverse acc ++ [x], xs)
      | otherwise = go (x:acc) xs

spanNumber :: String -> (String, String)
spanNumber input =
  let (numText, rest) = span isNumberChar input
  in (numText, rest)

matchPrefix :: [String] -> String -> Maybe String
matchPrefix patterns input =
  let matches = filter (`isPrefixOf` input) patterns
  in case matches of
       []    -> Nothing
       (p:_) -> Just p

isIdentifierStart :: Char -> Bool
isIdentifierStart ch = isAlpha ch || ch == '_'

isIdentifierChar :: Char -> Bool
isIdentifierChar ch = isAlphaNum ch || ch == '_'

isNumberChar :: Char -> Bool
isNumberChar ch = isAlphaNum ch || ch == '_' || ch == '.' || ch == 'x' || ch == 'X' || ch == 'p' || ch == 'P' || ch == '+' || ch == '-'

isKeyword :: String -> Bool
isKeyword txt = txt `elem`
  [ "break", "default", "func", "interface", "select"
  , "case", "defer", "go", "map", "struct"
  , "chan", "else", "goto", "package", "switch"
  , "const", "fallthrough", "if", "range", "type"
  , "continue", "for", "import", "return", "var"
  ]

multiOperators :: [String]
multiOperators =
  [ "<<=", ">>=", "&^=", "+=", "-=", "*=", "/=", "%=", "&=", "|=", "^=", "==", "!=", "<=", ">=", "<<", ">>", "<-", "&&", "||", "&^", ":=" ]

multiSymbols :: [String]
multiSymbols = ["..."]

singleOperators :: [Char]
singleOperators = "+-*/%&|^!=~"

symbolChars :: [Char]
symbolChars = "(){}[];,.:<>?@#$"
