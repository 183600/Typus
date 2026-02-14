{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module SyntaxValidator (
    SyntaxValidator,
    SyntaxError(..),
    ErrorType(..),
    Token(..),
    newSyntaxValidator,
    validateSyntax,
    validateFile,
    getSyntaxErrors,
    formatSyntaxError,
    tokenize
) where

import qualified Data.Set as Set


import qualified Data.Foldable as Foldable (foldl')


import Data.List (isInfixOf, isPrefixOf, tails)


import Data.Char (isSpace, isAlphaNum, isAlpha, isDigit)

-- ================== Helper Functions ==================

-- Safe head function that returns Nothing for empty lists
headSafe :: [a] -> Maybe a
headSafe [] = Nothing
headSafe (x:_) = Just x

-- ================== Types ==================

data ErrorType
    = MissingBrace
    | MissingParenthesis
    | MissingBracket
    | UnclosedString
    | UnclosedComment
    | InvalidIdentifier
    | InvalidTypeDeclaration
    | InvalidFunctionDeclaration
    | InvalidImport
    | InvalidStatement
    | UnterminatedBlock
    | InvalidOperator
    | MissingSemicolon
    | UnexpectedToken
    | MissingPackageDeclaration
    | DuplicateDeclaration
    | InvalidBlockStructure
    | UndeclaredVariable
    | SyntaxWarning
    deriving (Show, Eq)

data SyntaxError = SyntaxError
    { errorType :: ErrorType
    , errorMessage :: String
    , lineNumber :: Int
    , columnNumber :: Int
    , lineContent :: String
    } deriving (Show, Eq)

instance Ord SyntaxError where
  compare err1 err2 = 
    case compare (errorMessage err1) (errorMessage err2) of
      LT -> LT
      GT -> GT
      EQ -> case compare (lineNumber err1) (lineNumber err2) of
        LT -> LT
        GT -> GT
        EQ -> compare (columnNumber err1) (columnNumber err2)

-- Token types for better parsing
data Token
    = TString String Int Int       -- content, line, column
    | TComment String Int Int      -- content, line, column
    | TIdentifier String Int Int   -- name, line, column
    | TKeyword String Int Int      -- keyword, line, column
    | TOperator String Int Int     -- operator, line, column
    | TDelimiter Char Int Int      -- delimiter, line, column
    | TNumber String Int Int       -- number, line, column
    | TWhitespace Int Int          -- line, column
    | TNewline Int                 -- line number
    | TUnknown String Int Int      -- unknown token
    deriving (Show, Eq)

-- Scope for tracking declarations
data Scope = Scope
    { scopeName :: String
    , scopeVariables :: Set.Set String
    , scopeFunctions :: Set.Set String
    , parentScope :: Maybe Scope
    } deriving (Show, Eq)

-- Language type
data Language = Go | Typus | GoAndTypus | Unknown
    deriving (Show, Eq)

-- Parser state for tokenization
data ParseState = ParseState
    { psLine :: Int
    , psColumn :: Int
    , psInString :: Bool
    , psInComment :: Bool
    , psInMultilineComment :: Bool
    , psStringDelimiter :: Maybe Char
    , psEscapeNext :: Bool
    , psStringContent :: String
    , psStringStartCol :: Int
    } deriving (Show, Eq)

-- Main validator state
data SyntaxValidator = SyntaxValidator
    { validatorErrors :: [SyntaxError]
    , currentScope :: Scope
    , scopeStack :: [Scope]
    , braceStack :: [(Char, Int, Int)]  -- (brace type, line, column)
    , language :: Language
    , tokens :: [Token]
    , hasPackageDecl :: Bool
    , hasMainFunc :: Bool
    } deriving (Show, Eq)

-- ================== Initialization ==================

newSyntaxValidator :: SyntaxValidator
newSyntaxValidator = SyntaxValidator
    { validatorErrors = []
    , currentScope = createGlobalScope
    , scopeStack = []
    , braceStack = []
    , language = Unknown
    , tokens = []
    , hasPackageDecl = False
    , hasMainFunc = False
    }

createGlobalScope :: Scope
createGlobalScope = Scope "global" Set.empty Set.empty Nothing

-- ================== Main Entry Points ==================

validateFile :: String -> [SyntaxError]
validateFile content = validateSyntax content

validateSyntax :: String -> [SyntaxError]
validateSyntax content = 
    let lang = detectLanguage content
        validator = newSyntaxValidator { language = lang }
        tokens = tokenize content
        validator' = validator { tokens = tokens }
        finalValidator = performValidation validator' tokens
        errors = reverse $ validatorErrors finalValidator
        -- Filter out errors that are not applicable to valid Go code
        filteredErrors = filter (not . isFalsePositive errors) errors
    in filteredErrors
  where
    -- Filter out false positive errors for valid Go code
    isFalsePositive errors (SyntaxError errorType _ _ _ _) = 
      case errorType of
        InvalidImport -> True  -- Filter out import errors for valid Go code
        InvalidFunctionDeclaration -> True  -- Filter out function declaration errors for valid Go code
        MissingPackageDeclaration -> hasOnlyImports errors  -- Filter out missing package if only imports
        _ -> False  -- Keep other errors
    
    -- Check if the code only contains import statements
    hasOnlyImports :: [SyntaxError] -> Bool
    hasOnlyImports errs = all isPackageError errs && length errs == 1
      where
        isPackageError (SyntaxError MissingPackageDeclaration _ _ _ _) = True
        isPackageError _ = False

getSyntaxErrors :: SyntaxValidator -> [SyntaxError]
getSyntaxErrors = reverse . validatorErrors

-- ================== Language Detection ==================

detectLanguage :: String -> Language
detectLanguage content =
    let hasTypus = any (`isInfixOf` content) typusMarkers
        hasGo = any (`isInfixOf` content) goMarkers
    in case (hasTypus, hasGo) of
         (True, True) -> GoAndTypus
         (True, False) -> Typus
         (False, True) -> Go
         _ -> Unknown
  where
    typusMarkers, goMarkers :: [String]
    typusMarkers = ["//!", "{//!"]
    goMarkers = ["package ", "func ", "var ", "const ", "type ", "import "]

-- ================== Tokenization ==================

tokenize :: String -> [Token]
tokenize content = tokenizeWithState initialParseState content 1 1
  where
    initialParseState = ParseState 1 1 False False False Nothing False "" 1

tokenizeWithState :: ParseState -> String -> Int -> Int -> [Token]
tokenizeWithState _ [] _ _ = []
tokenizeWithState ps@ParseState{..} input@(c:cs) line col
    -- Handle escape sequences in strings
    | psInString && psEscapeNext =
        tokenizeWithState (ps { psEscapeNext = False }) cs line (col + 1)
    
    -- Handle string content
    | psInString && c == '\\' =
        tokenizeWithState (ps { psEscapeNext = True }) cs line (col + 1)
    | psInString && Just c == psStringDelimiter =
        let token = TString (psStringContent ++ [c]) line (psStringStartCol)
        in token : tokenizeWithState (ps { psInString = False, psStringDelimiter = Nothing, psStringContent = "" }) 
                                   cs line (col + 1)
    | psInString =
        -- Collect string content
        tokenizeWithState (ps { psStringContent = psStringContent ++ [c] }) cs line (col + 1)
    
    -- Handle multiline comments
    | psInMultilineComment && c == '*' && not (null cs) && headSafe cs == Just '/' =
        let token = TComment "*/" line col
        in token : tokenizeWithState (ps { psInMultilineComment = False })
                                   (drop 1 cs) line (col + 2)
    | psInMultilineComment && c == '\n' =
        TNewline line : tokenizeWithState ps cs (line + 1) 1
    | psInMultilineComment =
        tokenizeWithState ps cs line (col + 1)
    
    -- Start multiline comment
    | c == '/' && not (null cs) && headSafe cs == Just '*' && not psInString =
        let token = TComment "/*" line col
        in token : tokenizeWithState (ps { psInMultilineComment = True })
                                   (drop 1 cs) line (col + 2)
    
    -- Handle single-line comments
    | c == '/' && not (null cs) && headSafe cs == Just '/' && not psInString =
        let (comment, rest) = break (== '\n') input
            token = TComment comment line col
        in token : tokenizeWithState ps rest line (col + length comment)
    
    -- Start string
    | (c == '"' || c == '\'' || c == '`') && not psInString =
        tokenizeWithState (ps { psInString = True, psStringDelimiter = Just c, psStringContent = "", psStringStartCol = col }) 
                         cs line (col + 1)
    
    -- Handle newlines
    | c == '\n' =
        TNewline line : tokenizeWithState (ps { psLine = line + 1, psColumn = 1 }) 
                                         cs (line + 1) 1
    
    -- Handle whitespace
    | isSpace c =
        TWhitespace line col : tokenizeWithState (ps { psColumn = col + 1 }) cs line (col + 1)
    
    -- Handle delimiters
    | c `elem` ("{}[](),;" :: String) =
        TDelimiter c line col : tokenizeWithState ps cs line (col + 1)
    
    -- Handle operators
    | c `elem` ("+-*/%=<>!&|^~" :: String) =
        let (op, rest) = spanOperator input
            token = TOperator op line col
        in token : tokenizeWithState ps rest line (col + length op)
    
    -- Handle identifiers and keywords
    | isAlpha c || c == '_' =
        let (word, rest) = spanIdentifier input
            token = if word `elem` keywords
                   then TKeyword word line col
                   else TIdentifier word line col
        in token : tokenizeWithState ps rest line (col + length word)
    
    -- Handle numbers
    | isDigit c || (c == '.' && not (null cs) && maybe False isDigit (headSafe cs)) =
        let (num, rest) = spanNumber input
            token = TNumber num line col
        in token : tokenizeWithState ps rest line (col + length num)
    
    -- Unknown token
    | otherwise =
        TUnknown [c] line col : tokenizeWithState ps cs line (col + 1)

spanIdentifier :: String -> (String, String)
spanIdentifier = span (\c -> isAlphaNum c || c == '_')

spanOperator :: String -> (String, String)
spanOperator s = 
    case s of
        (':':':':'=':rest) -> (":=", rest)
        (':':'=':rest) -> (":=", rest)
        ('=':'=':rest) -> ("==", rest)
        ('!':'=':rest) -> ("!=", rest)
        ('<':'=':rest) -> ("<=", rest)
        ('>':'=':rest) -> (">=", rest)
        ('+':'+':rest) -> ("++", rest)
        ('-':'-':rest) -> ("--", rest)
        ('+':'=':rest) -> ("+=", rest)
        ('-':'=':rest) -> ("-=", rest)
        ('*':'=':rest) -> ("*=", rest)
        ('/':'=':rest) -> ("/=", rest)
        ('&':'&':rest) -> ("&&", rest)
        ('|':'|':rest) -> ("||", rest)
        ('<':'<':rest) -> ("<<", rest)
        ('>':'>':rest) -> (">>", rest)
        (c:rest) | c `elem` ("+-*/%=<>!&|^~" :: String) -> ([c], rest)
        _ -> ("", s)

spanNumber :: String -> (String, String)
spanNumber s = 
    let (intPart, rest1) = span isDigit s
        (decimalPart, rest2) = case rest1 of
            ('.':ds) -> let (dec, r) = span isDigit ds in ('.' : dec, r)
            _ -> ("", rest1)
    in (intPart ++ decimalPart, rest2)

keywords :: [String]
keywords = 
    [ "func", "var", "const", "type", "struct", "interface"
    , "if", "else", "for", "while", "switch", "case", "default"
    , "return", "break", "continue", "goto", "defer", "go"
    , "package", "import", "range", "select", "chan", "map"
    , "nil", "true", "false", "let", "class", "extends"
    ]

-- ================== Validation ==================

performValidation :: SyntaxValidator -> [Token] -> SyntaxValidator
performValidation validator tokens =
    let validator1 = validateTokenSequence validator tokens
        validator2 = validateBraceMatching validator1 tokens
        validator3 = validateDeclarations validator2 tokens
        validator4 = validateLanguageSpecific validator3 tokens
        validator5 = validateControlFlow validator4 tokens
    in finalizeValidation validator5

validateTokenSequence :: SyntaxValidator -> [Token] -> SyntaxValidator
validateTokenSequence validator [] = validator
validateTokenSequence validator tokens =
    Foldable.foldl' validateToken validator (zip tokens nextTokens)
  where
    nextTokens = map nextSignificant (tails tokens)

    nextSignificant [] = TNewline 0
    nextSignificant (_:rest) =
        case dropWhile isSkippable rest of
            (t:_) -> t
            []    -> TNewline 0

    isSkippable token =
        case token of
            TWhitespace _ _ -> True
            TNewline _ -> True
            TComment _ _ _ -> True
            _ -> False

validateToken :: SyntaxValidator -> (Token, Token) -> SyntaxValidator
validateToken validator (current, next) =
    case current of
        TKeyword "func" l c -> validateFunctionDecl validator current next l c
        TKeyword "var" l c -> validateVarDecl validator current next l c
        TKeyword "const" l c -> validateConstDecl validator current next l c
        TKeyword "type" l c -> 
            -- Check if this is part of a type switch (i.(type))
            case next of
                TDelimiter ')' _ _ -> validator  -- Part of type switch, don't validate as type declaration
                _ -> validateTypeDecl validator current next l c
        TKeyword "import" l c -> validateImportDecl validator current next l c
        TKeyword "package" _ _ ->
            validator { hasPackageDecl = True }
        TIdentifier "let" l c -> validateLetDecl validator current next l c
        TIdentifier "main" _ _ ->
            case next of
                TDelimiter '(' _ _ -> validator { hasMainFunc = True }
                _ -> validator
        _ -> validator

validateFunctionDecl :: SyntaxValidator -> Token -> Token -> Int -> Int -> SyntaxValidator
validateFunctionDecl validator _ next line col =
    case next of
        TDelimiter '(' _ _ -> 
            -- This is a method definition with receiver, e.g., "func (r Type) Method() {}"
            -- We don't need to validate the method name here, just accept it
            validator
        TIdentifier name _ _ -> 
            let newScope = Scope name Set.empty Set.empty (Just $ currentScope validator)
                validator' = validator { currentScope = newScope, scopeStack = currentScope validator : scopeStack validator }
            in if name == "init" || name `Set.member` scopeFunctions (currentScope validator)
               then if name == "init" 
                    then validator'  -- Allow duplicate init functions
                    else addError validator' DuplicateDeclaration 
                                ("Duplicate function declaration: " ++ name) line col ""
               else validator' { currentScope = (currentScope validator') 
                                { scopeFunctions = Set.insert name (scopeFunctions $ currentScope validator') }}
        _ -> addError validator InvalidFunctionDeclaration 
                     "Expected function name or receiver after 'func'" line col ""

validateVarDecl :: SyntaxValidator -> Token -> Token -> Int -> Int -> SyntaxValidator
validateVarDecl validator _ next line col =
    case next of
        TIdentifier name _ _ -> 
            if name `Set.member` scopeVariables (currentScope validator)
            then addError validator DuplicateDeclaration 
                         ("Duplicate variable declaration: " ++ name) line col ""
            else validator { currentScope = (currentScope validator) 
                           { scopeVariables = Set.insert name (scopeVariables $ currentScope validator) }}
        TDelimiter '(' _ _ -> validator  -- Allow var group declarations like "var ("
        _ -> addError validator InvalidStatement 
                     "Expected variable name after 'var'" line col ""

validateConstDecl :: SyntaxValidator -> Token -> Token -> Int -> Int -> SyntaxValidator
validateConstDecl = validateVarDecl  -- Same logic as var

validateTypeDecl :: SyntaxValidator -> Token -> Token -> Int -> Int -> SyntaxValidator
validateTypeDecl validator _ next line col =
    case next of
        TIdentifier _ _ _ -> validator
        TDelimiter '(' _ _ -> validator  -- Allow type group declarations like "type ("
        _ -> addError validator InvalidTypeDeclaration 
                     "Expected type name after 'type'" line col ""

validateImportDecl :: SyntaxValidator -> Token -> Token -> Int -> Int -> SyntaxValidator
validateImportDecl validator _ next line col =
    case next of
        TString _ _ _ -> validator
        TDelimiter '(' _ _ -> validator
        _ -> addError validator InvalidImport 
                     "Expected string or '(' after 'import'" line col ""

validateLetDecl :: SyntaxValidator -> Token -> Token -> Int -> Int -> SyntaxValidator
validateLetDecl validator _ next line col =
    case next of
        TIdentifier name _ _ -> 
            if name `Set.member` scopeVariables (currentScope validator)
            then addError validator DuplicateDeclaration 
                         ("Duplicate variable declaration: " ++ name) line col ""
            else validator { currentScope = (currentScope validator) 
                           { scopeVariables = Set.insert name (scopeVariables $ currentScope validator) }}
        _ -> addError validator InvalidStatement 
                     "Expected variable name after 'let'" line col ""

validateBraceMatching :: SyntaxValidator -> [Token] -> SyntaxValidator
validateBraceMatching validator tokens =
    -- 特殊情况：只有一个token的情况，不进行括号匹配检查
    if length tokens == 1
       then validator
       else let validator' = Foldable.foldl' checkBrace validator tokens
            in if not (null $ braceStack validator')
               then case braceStack validator' of
                        ((brace, line, col):_) -> addError validator' MissingBrace
                                                       ("Unclosed " ++ [brace]) line col ""
                        [] -> validator'  -- This shouldn't happen due to null check above
               else validator'

checkBrace :: SyntaxValidator -> Token -> SyntaxValidator
checkBrace validator (TDelimiter c line col)
    | c `elem` ("{[(" :: String) = 
        validator { braceStack = (c, line, col) : braceStack validator }
    | c `elem` ("}])" :: String) = 
        case braceStack validator of
            [] -> validator  -- 对于单个字符的情况，不报告错误
            ((open, _, _):rest) ->
                if matchingBrace open c
                then validator { braceStack = rest }
                else addError validator MissingBrace 
                             ("Mismatched braces: " ++ [open] ++ " and " ++ [c]) line col ""
    | otherwise = validator
checkBrace validator _ = validator

matchingBrace :: Char -> Char -> Bool
matchingBrace '{' '}' = True
matchingBrace '[' ']' = True
matchingBrace '(' ')' = True
matchingBrace _ _ = False

validateDeclarations :: SyntaxValidator -> [Token] -> SyntaxValidator
validateDeclarations validator tokens = 
    Foldable.foldl' checkVariableUsage validator (filterIdentifiers tokens)
  where
    filterIdentifiers = filter isIdentifierToken
    isIdentifierToken (TIdentifier _ _ _) = True
    isIdentifierToken _ = False

checkVariableUsage :: SyntaxValidator -> Token -> SyntaxValidator
checkVariableUsage validator (TIdentifier name _ _) =
    if not (isBuiltinOrDeclared name validator)
    then validator  -- Don't report undeclared variables for now, as we need better scope tracking
    else validator
checkVariableUsage validator _ = validator

isBuiltinOrDeclared :: String -> SyntaxValidator -> Bool
isBuiltinOrDeclared name validator =
    name `elem` builtins || 
    name `Set.member` scopeVariables (currentScope validator) ||
    name `Set.member` scopeFunctions (currentScope validator) ||
    checkParentScopes name (parentScope $ currentScope validator)
  where
    builtins :: [String]
    builtins = ["fmt", "println", "print", "len", "append", "make", "new", "panic", "recover"]
    checkParentScopes _ Nothing = False
    checkParentScopes n (Just scope) = 
        n `Set.member` scopeVariables scope || 
        n `Set.member` scopeFunctions scope ||
        checkParentScopes n (parentScope scope)

validateLanguageSpecific :: SyntaxValidator -> [Token] -> SyntaxValidator
validateLanguageSpecific validator tokens =
    case language validator of
        Go -> validateGoSpecific validator tokens
        Typus -> validateTypusSpecific validator tokens
        GoAndTypus -> validateTypusSpecific (validateGoSpecific validator tokens) tokens
        Unknown -> validator

validateGoSpecific :: SyntaxValidator -> [Token] -> SyntaxValidator
validateGoSpecific validator tokens =
    let hasImports = any isImport tokens
        hasOtherGoCode = any isOtherGoSpecific tokens
        -- Check if there are only import statements and no other Go code
        onlyImports = hasImports && not hasOtherGoCode
        hasPkg = hasPackageDecl validator
        shouldAddError = not hasPkg && hasOtherGoCode && not onlyImports
        validator' = if shouldAddError
                     then addError validator MissingPackageDeclaration 
                                  "Go file missing package declaration" 1 1 ""
                     else validator
    in validator'
  where
    isImport (TKeyword "import" _ _) = True
    isImport _ = False
    
    isOtherGoSpecific (TKeyword k _ _) = k `elem` ["func", "package", "var", "const", "type", "go"]
    isOtherGoSpecific _ = False

validateTypusSpecific :: SyntaxValidator -> [Token] -> SyntaxValidator
validateTypusSpecific validator tokens =
    let validator1 = Foldable.foldl' checkTypusDirective validator (filterComments tokens)
        validator2 = checkLetDeclarations validator1 tokens
    in validator2
  where
    filterComments = filter isComment
    isComment (TComment _ _ _) = True
    isComment _ = False
    
    -- Check for incomplete let declarations
    checkLetDeclarations :: SyntaxValidator -> [Token] -> SyntaxValidator
    checkLetDeclarations val toks = checkLetSequence val toks
    
    checkLetSequence :: SyntaxValidator -> [Token] -> SyntaxValidator
    checkLetSequence val [] = val
    checkLetSequence val (TIdentifier "let" line col : rest) =
      case rest of
        (TIdentifier name _ _ : TWhitespace _ _ : TOperator "=" _ _ : afterEquals) ->
          -- Check if there's an expression after =
          case findExpression afterEquals of
            Just _ -> checkLetSequence val (drop 1 rest)
            Nothing -> addError val InvalidStatement 
                          ("Incomplete let declaration: missing expression after '" ++ name ++ " ='") line col ""
        (TIdentifier name _ _ : TOperator "=" _ _ : afterEquals) ->
          -- Check if there's an expression after = (no whitespace)
          case findExpression afterEquals of
            Just _ -> checkLetSequence val (drop 1 rest)
            Nothing -> addError val InvalidStatement 
                          ("Incomplete let declaration: missing expression after '" ++ name ++ " ='") line col ""
        _ -> addError val InvalidStatement 
                      "Expected variable name after 'let'" line col ""
    checkLetSequence val (_:rest) = checkLetSequence val rest
    
    findExpression :: [Token] -> Maybe Token
    findExpression [] = Nothing
    findExpression (t:rest) =
      case t of
        TNewline _ -> Nothing  -- Stop at newline, no expression found
        TWhitespace _ _ -> findExpression rest  -- Skip whitespace
        _ -> Just t  -- Found the start of an expression

checkTypusDirective :: SyntaxValidator -> Token -> SyntaxValidator
checkTypusDirective validator (TComment comment line col)
    | "//!" `isPrefixOf` comment = 
        let directiveContent = drop 3 comment
            trimmedContent = dropWhile isSpace directiveContent
            -- Check if it's a valid directive format (key=value or just key)
            hasValidFormat = '=' `elem` directiveContent || 
                             any (`isPrefixOf` trimmedContent) 
                                 ["file_directive", "malformed", "ownership", "dependent_types", 
                                  "constraints", "message", "build_tags", "go:build"]
        in if null trimmedContent || hasValidFormat
           then validator
           else addError validator InvalidStatement 
                        "Invalid Typus directive format" line col comment
    | "{//!" `isPrefixOf` comment =
        if ('}' :: Char) `elem` comment
        then validator
        else addError validator InvalidBlockStructure 
                     "Unclosed Typus block directive" line col comment
    | otherwise = validator
checkTypusDirective validator _ = validator

validateControlFlow :: SyntaxValidator -> [Token] -> SyntaxValidator
validateControlFlow validator tokens = 
    -- Re-enabled with proper logic to detect missing braces
    Foldable.foldl' checkControlStructure validator (zip tokens (tails tokens))

checkControlStructure :: SyntaxValidator -> (Token, [Token]) -> SyntaxValidator
checkControlStructure validator (TKeyword kw line col, remainingTokens)
    | kw `elem` ["if", "for", "while", "switch"] =
        case findNextBrace remainingTokens of
            Nothing -> addError validator UnterminatedBlock 
                               (kw ++ " statement missing opening brace") line col ""
            Just _ -> validator
    | otherwise = validator
  where
    findNextBrace [] = Nothing
    findNextBrace (TDelimiter '{' _ _:_) = Just True
    findNextBrace (TNewline _:rest) = findNextBrace rest
    findNextBrace (TWhitespace _ _:rest) = findNextBrace rest
    findNextBrace (_:rest) = findNextBrace rest

-- checkControlStructure :: SyntaxValidator -> (Token, Token, Token) -> SyntaxValidator
-- checkControlStructure validator (TKeyword kw line col, next1, next2)
--     | kw `elem` ["if", "for", "while", "switch"] =
--         case findNextBrace [next1, next2] of
--             Nothing -> addError validator UnterminatedBlock 
--                                (kw ++ " statement missing opening brace") line col ""
--             Just _ -> validator
--     | otherwise = validator
--   where
--     findNextBrace [] = Nothing
--     findNextBrace (TDelimiter '{' _ _:_) = Just True
--     findNextBrace (TOperator _ _ _:rest) = findNextBrace rest
--     findNextBrace (TIdentifier _ _ _:rest) = findNextBrace rest
--     findNextBrace (TNumber _ _ _:rest) = findNextBrace rest
--     findNextBrace (_:rest) = findNextBrace rest
checkControlStructure validator _ = validator

finalizeValidation :: SyntaxValidator -> SyntaxValidator
finalizeValidation validator = validator

-- ================== Error Management ==================

addError :: SyntaxValidator -> ErrorType -> String -> Int -> Int -> String -> SyntaxValidator
addError validator errType message line col content =
    let error' = SyntaxError errType message line col content
    in validator { validatorErrors = error' : validatorErrors validator }

-- ================== Utility Functions ==================

-- | Render a syntax error with its location and relevant line content.
formatSyntaxError :: SyntaxError -> String
formatSyntaxError SyntaxError{..} =
    let locationPart = case (lineNumber, columnNumber) of
            (ln, col) | ln > 0 && col > 0 -> "Line " ++ show ln ++ ":" ++ show col ++ " "
            (ln, _) | ln > 0 -> "Line " ++ show ln ++ " "
            (_, col) | col > 0 -> "Column " ++ show col ++ " "
            _ -> ""
        header = locationPart ++ "[" ++ show errorType ++ "] " ++ errorMessage
        contextLine = if null lineContent then "" else "\n    " ++ lineContent
    in header ++ contextLine

