module Ownership.Lexer (
    Keyword(..),
    Sym(..),
    OwnershipToken,
    ownershipLexerSpec,
    lexAll
) where

import Data.Char (isDigit, isAlpha)

import Ownership.Common.Lexer (Token(..), LexerSpec(..), lexWithSpec)

data Keyword
  = KwVar | KwLet | KwFunc | KwReturn | KwIf | KwElse | KwFor
  | KwPackage | KwImport | KwType | KwStruct | KwInterface | KwConst
  | KwMut | KwTrue | KwFalse | KwGo | KwDefer
  deriving (Eq, Show)

data Sym
  = SLBrace | SRBrace | SLParen | SRParen | SLBracket | SRBracket
  | SSemicolon | SComma | SColon | SAssign | SWalrus | SAmp | SDot
  | SNewline
  deriving (Eq, Show)

type OwnershipToken = Token Keyword Sym

kwFromStr :: String -> Maybe Keyword
kwFromStr s = case s of
  "var"     -> Just KwVar
  "let"     -> Just KwLet
  "func"    -> Just KwFunc
  "return"  -> Just KwReturn
  "if"      -> Just KwIf
  "else"    -> Just KwElse
  "for"     -> Just KwFor
  "package" -> Just KwPackage
  "import"  -> Just KwImport
  "type"    -> Just KwType
  "struct"  -> Just KwStruct
  "interface"->Just KwInterface
  "const"   -> Just KwConst
  "mut"     -> Just KwMut
  "true"    -> Just KwTrue
  "false"   -> Just KwFalse
  "go"      -> Just KwGo
  "defer"   -> Just KwDefer
  _         -> Nothing

ownershipLexerSpec :: LexerSpec Keyword Sym
ownershipLexerSpec = LexerSpec
  { specKeywords = kwFromStr
  , specMultiSymbols =
      [ (":=", SWalrus)
      ]
  , specSingleSymbols =
      [ ('=', SAssign)
      , ('{', SLBrace)
      , ('}', SRBrace)
      , ('(', SLParen)
      , (')', SRParen)
      , ('[', SLBracket)
      , (']', SRBracket)
      , (';', SSemicolon)
      , (',', SComma)
      , (':', SColon)
      , ('&', SAmp)
      , ('.', SDot)
      ]
  , specNewlineSymbol = SNewline
  , specIsNumChar = \x -> isDigit x || x == '.' || x == '_'
  , specIsIdentStart = \x -> x == '_' || isAlpha x
  , specIsIdentChar = \x -> x == '_' || isAlpha x || isDigit x
  }

lexAll :: String -> [OwnershipToken]
lexAll = lexWithSpec ownershipLexerSpec
