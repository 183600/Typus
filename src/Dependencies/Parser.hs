{-# LANGUAGE OverloadedStrings #-}

module Dependencies.Parser (
  Parser,
  grammarDefinition,
  parseProgram,
  parseStatement,
  parseTypeExpr,
  parseConstraint,
  runParser
) where

import Control.Monad (void)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec (Parsec, (<|>), choice, many, eof, try, optional)
import qualified Text.Megaparsec as MP
import Text.Megaparsec.Char (alphaNumChar, letterChar, char, string, space1)
import qualified Text.Megaparsec.Char.Lexer as L

import Dependencies.AST

-- | The Megaparsec parser type specialised for Typus sources.
type Parser = Parsec Void Text

-- | Human readable grammar definition for documentation and debugging.
grammarDefinition :: String
grammarDefinition = unlines
  [ "Typus Language BNF Grammar:",
    "",
    "<program>       ::= <statement>*",
    "<statement>     ::= <typeDef> | <varDecl> | <funcDecl> | <constraintDef>",
    "<typeDef>       ::= \"type\" <identifier> [<typeParams>] [<whereClause>]",
    "<typeParams>    ::= \"<\" <identifier> (\",\" <identifier>)* \">\"",
    "<whereClause>   ::= \"where\" <constraint> (\",\" <constraint>)*",
    "<varDecl>       ::= (\"var\" | \"const\") <identifier> \":\" <typeExpr>",
    "<funcDecl>      ::= \"func\" <identifier> \"(" <paramList> \")\" [\":\" <typeExpr>]",
    "<paramList>     ::= <param> (\",\" <param>)* | ε",
    "<param>         ::= <identifier> \":\" <typeExpr>",
    "<constraintDef> ::= \"constraint\" <identifier> \"=\" <constraintExpr>",
    "<typeExpr>      ::= <simpleType> | <genericType> | <dependentType> | <functionType>",
    "<simpleType>    ::= <identifier>",
    "<genericType>   ::= <identifier> \"<\" <typeExpr> (\",\" <typeExpr>)* \">\"",
    "<dependentType> ::= <typeExpr> \"where\" <constraint> (\",\" <constraint>)*",
    "<functionType>  ::= \"func\" \"(" <paramList> \")\" \":\" <typeExpr>",
    "<constraint>    ::= <sizeConstraint> | <rangeConstraint> | <predicateConstraint>",
    "<sizeConstraint> ::= <identifier> (\">\" | \">=\") <integer>",
    "<rangeConstraint> ::= <identifier> \"<\" <integer> \",\" <integer> \">\"",
    "<predicateConstraint> ::= <identifier> \"(" <argList> \")\"",
    "<argList>       ::= <typeExpr> (\",\" <typeExpr>)* | ε",
    "<integer>       ::= [0-9]+",
    "<identifier>    ::= [A-Za-z_][A-Za-z0-9_]*"
  ]

-- Common lexing helpers -------------------------------------------------------

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

rword :: Text -> Parser ()
rword w = void ((lexeme . try) (string w *> notFollowedByIdentChar))
  where
    notFollowedByIdentChar :: Parser ()
    notFollowedByIdentChar = MP.notFollowedBy (alphaNumChar <|> char '_')

rws :: [Text]
rws = ["type","where","var","const","func","constraint","forall","exists","protocol","capability"]

identifier :: Parser Text
identifier = (lexeme . try) $ do
  x <- letterChar <|> char '_' <|> char '\''
  xs <- many (alphaNumChar <|> char '_' <|> char '\'')
  let s = T.pack (x:xs)
  if s `elem` rws then fail ("reserved word " <> T.unpack s) else pure s

integer :: Parser Int
integer = lexeme (fromInteger <$> L.decimal)

angles :: Parser a -> Parser a
angles = MP.between (symbol "<") (symbol ">")

parens :: Parser a -> Parser a
parens = MP.between (symbol "(") (symbol ")")

commaSep :: Parser a -> Parser [a]
commaSep p = p `MP.sepBy1` symbol ","

-- Parser ---------------------------------------------------------------------

parseProgram :: Parser AST
parseProgram = sc *> (Program <$> many parseStatement) <* eof

parseStatement :: Parser Statement
parseStatement = choice
  [ parseTypeDef
  , parseVarDecl
  , parseFuncDecl
  , parseConstraintDef
  , parseExistsDecl
  , parseProtocolDecl
  , parseCapabilityDecl
  ]

parseTypeDef :: Parser Statement
parseTypeDef = do
  rword "type"
  name <- identifier
  params <- optional (angles (commaSep parseTypeParam))
  optionalType <- optional $ symbol "="
  case optionalType of
    Just _ -> do
      target <- parseTypeExpr
      cs <- optional parseWhereClause
      pure $ STypeAlias name target (maybe [] id cs)
    Nothing -> do
      cs <- optional parseWhereClause
      pure $ STypeDef name (maybe [] id params) (maybe [] id cs)
  where
    parseTypeParam :: Parser Text
    parseTypeParam = do
      pname <- identifier
      _ <- optional (symbol ":" *> parseTypeExpr)
      pure pname

parseWhereClause :: Parser [Constraint]
parseWhereClause = do
  rword "where"
  parseConstraintsSep
  where
    parseConstraintsSep = do
      c1 <- parseConstraint
      more <- many ((symbol "&&" <|> symbol ",") *> parseConstraint)
      pure (c1 : more)

parseVarDecl :: Parser Statement
parseVarDecl = do
  _ <- rword "var" <|> rword "const"
  name <- identifier
  _ <- symbol ":"
  t <- parseTypeExpr
  pure $ SVarDecl name t

parseFuncDecl :: Parser Statement
parseFuncDecl = do
  rword "func"
  name <- identifier
  _ <- optional (angles (commaSep identifier))
  params <- parens parseParamList
  rt <- optional (symbol ":" *> parseTypeExpr)
  pure $ SFuncDecl name params rt

parseParamList :: Parser [(Text, TypeExpr)]
parseParamList = (commaSep parseParam) <|> pure []
  where
    parseParam = try namedParam <|> typeOnlyParam

    namedParam = do
      n <- identifier
      _ <- symbol ":"
      t <- parseTypeExpr
      pure (n, t)

    typeOnlyParam :: Parser (Text, TypeExpr)
    typeOnlyParam = do
      t <- parseTypeExpr
      pure ("_", t)

parseConstraintDef :: Parser Statement
parseConstraintDef = do
  rword "constraint"
  name <- identifier
  _ <- symbol "="
  c <- parseConstraint
  pure $ SConstraintDef name c

parseProtocolDecl :: Parser Statement
parseProtocolDecl = do
  rword "protocol"
  _ <- identifier
  _ <- many (try protocolEntry)
  pure (SConstraintDef "protocol" (PredC "protocol" []))
  where
    protocolEntry = do
      _ <- identifier
      _ <- symbol ":"
      _ <- parseTypeExpr
      pure ()

parseCapabilityDecl :: Parser Statement
parseCapabilityDecl = do
  rword "capability"
  _ <- identifier
  pure (SConstraintDef "capability" (PredC "capability" []))

parseExistsDecl :: Parser Statement
parseExistsDecl = do
  rword "exists"
  vars <- commaSep identifier
  _ <- symbol "."
  stmt <- parseStatement
  pure $ SExistsDecl vars stmt

parseConstraint :: Parser Constraint
parseConstraint = do
  c <- choice [ try parseTypeClass
              , try parseEquality
              , try parseRelOpExtended
              , try parseRange
              , parsePredicateExt
              ]
  pure c
  where
    parseTypeClass = do
      n <- identifier
      _ <- symbol ":"
      t <- parseTypeExpr
      pure $ PredC ":" [SimpleT n, t]

    parseEquality = do
      lhs <- identifier
      _ <- symbol "=="
      rhsId <- optional identifier
      rhsVal <- case rhsId of
        Just r  -> pure (SimpleT r)
        Nothing -> SimpleT . T.pack . show <$> integer
      pure $ PredC "==" [SimpleT lhs, rhsVal]

    parseRelOpExtended = do
      lhs <- parseTermLHS
      op <- symbol ">=" *> pure ">="
         <|> symbol ">"  *> pure ">"
         <|> symbol "<=" *> pure "<="
         <|> symbol "<"  *> pure "<"
      rhs <- parseTermRHS
      pure $ PredC (T.pack op) [lhs, rhs]

    parseTermLHS = try (GenericT <$> identifier <*> parens ((commaSep parseTypeExpr) <|> pure []))
               <|> SimpleT <$> identifier
               <|> SimpleT . T.pack . show <$> integer

    parseTermRHS = try (GenericT <$> identifier <*> parens ((commaSep parseTypeExpr) <|> pure []))
               <|> SimpleT <$> identifier
               <|> SimpleT . T.pack . show <$> integer

    parseRange = do
      n <- identifier
      _ <- symbol "<"
      a <- integer
      _ <- symbol ","
      b <- integer
      _ <- symbol ">"
      pure $ RangeC n a b

    parsePredicateExt = do
      p <- identifier
      args <- optional (parens ( (commaSep parseTypeExpr) <|> pure [] ))
      pure $ PredC p (maybe [] id args)

parseTypeExpr :: Parser TypeExpr
parseTypeExpr = try parseConditionalType
            <|> try parseForallType
            <|> try parseExistsType
            <|> try parseRefType
            <|> try parseFuncType
            <|> parseRefineOrApp
  where
    parseForallType = do
      rword "forall"
      _ <- commaSep identifier
      _ <- symbol "."
      parseTypeExpr

    parseExistsType = do
      rword "exists"
      _ <- commaSep identifier
      _ <- symbol "."
      parseTypeExpr

    parseFuncType = do
      rword "func"
      ps <- parens parseParamList
      _ <- symbol ":"
      rt <- parseTypeExpr
      let ft = FuncT ps rt
      cs <- optional parseWhereClause
      case cs of
        Nothing -> pure ft
        Just cs' -> pure (RefineT ft cs')

    parseRefineOrApp = do
      base <- parseAppOrSimple
      cs <- optional parseWhereClause
      case cs of
        Nothing -> pure base
        Just cs' -> pure (RefineT base cs')

    parseAppOrSimple = do
      n <- identifier
      mags <- optional (angles (commaSep parseTypeArg))
      case mags of
        Nothing -> pure (SimpleT n)
        Just args -> pure (GenericT n args)

    parseTypeArg = try parseBoolConstraintAsType <|> parseTypeExpr

    parseBoolConstraintAsType = do
      c <- parseRelOrPredExpr
      pure (RefineT (SimpleT "bool") [c])

    parseRelOrPredExpr = try parseBinaryRel <|> parseBarePredicate

    parseBinaryRel = do
      lhs <- parseTerm
      op <- symbol ">=" *> pure ">="
         <|> symbol ">"  *> pure ">"
         <|> symbol "<=" *> pure "<="
         <|> symbol "<"  *> pure "<"
         <|> symbol "==" *> pure "=="
      rhs <- parseTerm
      case lhs of
        SimpleT l -> pure $ PredC (T.pack op) [SimpleT l, rhs]
        _         -> pure $ PredC (T.pack op) [lhs, rhs]

    parseBarePredicate = do
      p <- identifier
      args <- optional (parens ((commaSep parseTypeExpr) <|> pure []))
      pure $ PredC p (maybe [] id args)

    parseTerm = try (SimpleT . T.pack . show <$> integer)
           <|> try (do f <- identifier
                        args <- parens ((commaSep parseTypeExpr) <|> pure [])
                        pure (GenericT f args))
           <|> (SimpleT <$> identifier)

    parseConditionalType = do
      cond <- parseAppOrSimple
      _ <- symbol "?"
      tThen <- parseTypeExpr
      _ <- symbol ":"
      tElse <- parseTypeExpr
      let result = GenericT "If" [cond, tThen, tElse]
      pure result

    parseRefType = do
      _ <- symbol "&"
      mlt <- optional (char '\'' *> identifier)
      mmut <- optional (rword "mut")
      t <- parseTypeExpr
      case (mmut, mlt) of
        (Just _, Just lt) -> pure (GenericT "RefMut" [SimpleT (T.cons '\'' lt), t])
        (Just _, Nothing) -> pure (GenericT "RefMut" [t])
        (Nothing, Just lt) -> pure (GenericT "Ref" [SimpleT (T.cons '\'' lt), t])
        _ -> pure (GenericT "Ref" [t])

-- | Convenience wrapper that mirrors the old API by parsing from a String.
runParser :: String -> Either String AST
runParser input =
  case MP.runParser parseProgram "(source)" (T.pack input) of
    Left e   -> Left (MP.errorBundlePretty e)
    Right ast -> Right ast
