{-# LANGUAGE ScopedTypeVariables #-}

module Ownership.Common.Lexer (
    Pos(..),
    Token(..),
    TokenKind(..),
    LexerSpec(..),
    lexWithSpec
) where

import Data.Char (isSpace)
import Data.List (isPrefixOf)

-- | Source position tracked during lexing.
data Pos = Pos
    { pLine :: !Int
    , pCol  :: !Int
    } deriving (Eq, Show)

-- | Token payload shared across all ownership analyzers. The parameterisation
-- over @kw@ and @sym@ keeps the generic lexer reusable while each analyzer can
-- define its own keyword and symbol enumerations.
data TokenKind kw sym
    = TId String
    | TKw kw
    | TSym sym
    | TString String
    | TNum String
    | TComment String Bool  -- ^ comment content and whether it was line based
    deriving (Eq, Show)

-- | Token annotated with source position information.
data Token kw sym = Token
    { tkKind :: !(TokenKind kw sym)
    , tkPos  :: !Pos
    } deriving (Eq, Show)

-- | Configuration passed to the generic lexer describing the keyword table and
-- the symbol inventory for a particular analyzer variant.
data LexerSpec kw sym = LexerSpec
    { specKeywords     :: String -> Maybe kw
    , specMultiSymbols :: [(String, sym)]  -- ^ ordered by priority/length
    , specSingleSymbols :: [(Char, sym)]
    , specNewlineSymbol :: sym
    , specIsNumChar     :: Char -> Bool
    , specIsIdentStart  :: Char -> Bool
    , specIsIdentChar   :: Char -> Bool
    }

-- | Lex the provided source code string according to the supplied specification.
lexWithSpec :: forall kw sym. LexerSpec kw sym -> String -> [Token kw sym]
lexWithSpec spec = go (Pos 1 1)
  where
    newlineSym :: sym
    newlineSym = specNewlineSymbol spec

    go :: Pos -> String -> [Token kw sym]
    go _ [] = []
    go pos s@(c:cs)
        -- Newline
        | c == '\n' =
            Token (TSym newlineSym) pos : go (Pos (pLine pos + 1) 1) cs
        -- Whitespace (except newline)
        | isSpace c && c /= '\n' =
            let ws = takeWhile (\x -> isSpace x && x /= '\n') s
                wsLen = length ws
                wsPos = bump pos wsLen
                rest = drop wsLen s
            in if null ws
               then go (bump pos 1) cs
               else Token (TId ws) pos : go wsPos rest
        -- Line comment
        | "//" `isPrefixOf` s =
            let (comment, rest, consumedNL, newPos) = readLineComment pos s
            in Token (TComment comment True) pos :
               (if consumedNL
                    then Token (TSym newlineSym) newPos : go newPos rest
                    else go newPos rest)
        -- Block comment
        | "/*" `isPrefixOf` s =
            let (comment, rest, newPos) = readBlockComment pos s
            in Token (TComment comment False) pos : go newPos rest
        -- String literal
        | c == '"' =
            let (str, rest, newPos) = readString pos cs
            in Token (TString str) pos : go newPos rest
        -- Character literal (treated as string for simplicity)
        | c == '\'' =
            let (ch, rest, newPos) = readChar pos cs
            in Token (TString ch) pos : go newPos rest
        -- Multi-character symbols (e.g. :=, <=)
        | Just (sym, len) <- matchMultiSymbol s =
            Token (TSym sym) pos : go (bump pos len) (drop len s)
        -- Single-character symbols
        | Just sym <- lookup c (specSingleSymbols spec) =
            Token (TSym sym) pos : go (bump pos 1) cs
        -- Number literal
        | specIsNumChar spec c =
            let (num, rest) = span (specIsNumChar spec) s
                newPos = bump pos (length num)
            in Token (TNum num) pos : go newPos rest
        -- Identifier / keyword
        | specIsIdentStart spec c =
            let (ident, rest) = span (specIsIdentChar spec) s
                newPos = bump pos (length ident)
                tk :: TokenKind kw sym
                tk = maybe (TId ident) TKw (specKeywords spec ident)
            in Token tk pos : go newPos rest
        -- Fallback: generate token for unrecognised character
        | otherwise = Token (TId [c]) pos : go (bump pos 1) cs

    bump :: Pos -> Int -> Pos
    bump (Pos l c) n = Pos l (c + n)

    matchMultiSymbol :: String -> Maybe (sym, Int)
    matchMultiSymbol input = goMatch (specMultiSymbols spec)
      where
        goMatch :: [([Char], a)] -> Maybe (a, Int)
        goMatch [] = Nothing
        goMatch ((pattern, sym):restPatterns)
            | pattern `isPrefixOf` input = Just (sym, length pattern)
            | otherwise = goMatch restPatterns

    readString :: Pos -> String -> (String, String, Pos)
    readString p s' = goStr [] p s'
      where
        goStr acc posN [] = (reverse acc, [], posN)
        goStr acc posN (x:xs)
            | x == '\\' = case xs of
                (y:ys) -> goStr (y:'\\':acc) (bump posN 2) ys
                []     -> (reverse ('\\':acc), [], bump posN 1)
            | x == '"'  = (reverse acc, xs, bump posN 1)
            | x == '\n' = goStr ('\n':acc) (Pos (pLine posN + 1) 1) xs
            | otherwise = goStr (x:acc) (bump posN 1) xs

    readChar :: Pos -> String -> (String, String, Pos)
    readChar p s' = goChr [] p s'
      where
        goChr acc posN [] = (reverse acc, [], posN)
        goChr acc posN (x:xs)
            | x == '\\' = case xs of
                (y:ys) -> goChr (y:'\\':acc) (bump posN 2) ys
                []     -> (reverse ('\\':acc), [], bump posN 1)
            | x == '\'' = (reverse acc, xs, bump posN 1)
            | x == '\n' = goChr ('\n':acc) (Pos (pLine posN + 1) 1) xs
            | otherwise = goChr (x:acc) (bump posN 1) xs

    readLineComment :: Pos -> String -> (String, String, Bool, Pos)
    readLineComment pos0 xs0 =
        let (_, rest0) = splitAt 2 xs0  -- //
            (content, rest) = break (== '\n') rest0
            consumed = not (null rest)
            rest' = dropWhile (== '\n') rest
            newPos = if consumed
                        then Pos (pLine pos0 + 1) 1
                        else bump pos0 (2 + length content)
        in (content, rest', consumed, newPos)

    readBlockComment :: Pos -> String -> (String, String, Pos)
    readBlockComment pos0 xs0 =
        let (_, rest0) = splitAt 2 xs0  -- /*
        in goBC [] pos0 rest0
      where
        goBC acc posN [] = (reverse acc, [], posN)
        goBC acc posN (x:xs)
            | x == '*' && "/" `isPrefixOf` xs =
                (reverse acc, drop 1 xs, bump posN 2)
            | x == '\n' = goBC ('\n':acc) (Pos (pLine posN + 1) 1) xs
            | otherwise = goBC (x:acc) (bump posN 1) xs
