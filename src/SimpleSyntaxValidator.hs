{-# LANGUAGE OverloadedStrings #-}
module SimpleSyntaxValidator (
    validateSyntaxSimple,
    countBraces,
    SyntaxError(..),
    ErrorType(..)
) where

import Data.List (isInfixOf, isPrefixOf)
import Data.Char (isSpace)

-- Error types
data ErrorType = 
    MissingBrace
  | MissingParenthesis  
  | MissingBracket
  | MissingPackageDeclaration
  | InvalidStatement
  | InvalidImport
  | InvalidFunctionDeclaration
  | InvalidTypeDeclaration
  | UnterminatedForLoop
  | UnterminatedIfStatement
  | UnterminatedSwitch
  | InvalidOperator
  | BracketMismatch
  | UnterminatedString
  | UnterminatedComment
  deriving (Show, Eq)

-- Syntax error data type
data SyntaxError = SyntaxError {
    errorType :: ErrorType,
    message :: String,
    lineNumber :: Int,
    columnNumber :: Int,
    lineContent :: String
} deriving (Show, Eq)

-- Parser state for tracking context
data ParseState = ParseState {
    inString :: Bool,
    inChar :: Bool,
    inRawString :: Bool,
    inSingleComment :: Bool,
    inMultiComment :: Bool,
    stringDelim :: Char,
    bracketStack :: [(Char, Int, Int)], -- (bracket type, line, column)
    currentLine :: Int,
    currentCol :: Int
} deriving (Show)

-- Initial parser state
initialState :: ParseState
initialState = ParseState False False False False False '"' [] 1 1

-- Main validation function
validateSyntaxSimple :: String -> [SyntaxError]
validateSyntaxSimple content = 
    let linesOfCode = lines content
        -- Context-aware bracket validation
        bracketErrors = validateBracketsWithContext content
        -- Go syntax structure validation  
        structureErrors = validateGoStructure linesOfCode
    in bracketErrors ++ structureErrors

-- Context-aware bracket validation
validateBracketsWithContext :: String -> [SyntaxError]
validateBracketsWithContext content = 
    let finalState = foldl processChar initialState (zip content [0..])
        unclosedErrors = map makeUnclosedError (reverse $ bracketStack finalState)
    in unclosedErrors
  where
    processChar :: ParseState -> (Char, Int) -> ParseState
    processChar state (char, idx) = 
        let lineCol = getLineCol content idx
            newLine = fst lineCol
            newCol = snd lineCol
            state' = state { currentLine = newLine, currentCol = newCol }
        in updateState state' char content idx
    
    makeUnclosedError :: (Char, Int, Int) -> SyntaxError
    makeUnclosedError (bracket, line, col) = 
        let errorType' = case bracket of
                           '{' -> MissingBrace
                           '(' -> MissingParenthesis
                           '[' -> MissingBracket
                           _ -> BracketMismatch
            bracketName :: String
            bracketName = case bracket of
                           '{' -> "brace"
                           '(' -> "parenthesis"
                           '[' -> "bracket"
                           _ -> "bracket"
        in SyntaxError errorType' 
            ("Unclosed " ++ bracketName ++ " at line " ++ show line ++ ", column " ++ show col)
            line col ""

-- Update parser state based on current character
updateState :: ParseState -> Char -> String -> Int -> ParseState
updateState state char content idx
    -- Handle string literals
    | inString state && not (inRawString state) = 
        case char of
            '\\' -> state -- Skip next character
            c | c == stringDelim state -> state { inString = False }
            _ -> state
    
    -- Handle raw string literals (backticks)
    | inRawString state = 
        if char == '`' then state { inRawString = False, inString = False }
        else state
    
    -- Handle character literals
    | inChar state = 
        case char of
            '\\' -> state -- Skip next character  
            '\'' -> state { inChar = False }
            _ -> state
    
    -- Handle single-line comments
    | inSingleComment state = 
        if char == '\n' then state { inSingleComment = False }
        else state
    
    -- Handle multi-line comments
    | inMultiComment state = 
        if char == '*' && idx + 1 < length content && content !! (idx + 1) == '/'
        then state { inMultiComment = False }
        else state
    
    -- Not in special context, process normally
    | otherwise = 
        case char of
            -- String/char literal starts
            '"' -> state { inString = True, stringDelim = '"' }
            '\'' -> state { inChar = True }
            '`' -> state { inRawString = True, inString = True }
            
            -- Comment starts
            '/' | idx + 1 < length content && content !! (idx + 1) == '/' -> 
                state { inSingleComment = True }
            '/' | idx + 1 < length content && content !! (idx + 1) == '*' -> 
                state { inMultiComment = True }
            
            -- Opening brackets
            '{' -> pushBracket state '{' 
            '(' -> pushBracket state '('
            '[' -> pushBracket state '['
            
            -- Closing brackets
            '}' -> popBracket state '}' '{'
            ')' -> popBracket state ')' '('
            ']' -> popBracket state ']' '['
            
            _ -> state

-- Push bracket onto stack
pushBracket :: ParseState -> Char -> ParseState
pushBracket state bracket = 
    state { bracketStack = (bracket, currentLine state, currentCol state) : bracketStack state }

-- Pop bracket from stack and check for match
popBracket :: ParseState -> Char -> Char -> ParseState
popBracket state _ expectedOpen =
    case bracketStack state of
        [] -> state -- Error: closing bracket without opening
        ((openBracket, _, _):rest) ->
            if openBracket == expectedOpen
            then state { bracketStack = rest }
            else state -- Error: mismatched brackets

-- Get line and column from character index
getLineCol :: String -> Int -> (Int, Int)
getLineCol content idx = 
    let beforeIdx = take idx content
        lineNum = 1 + length (filter (== '\n') beforeIdx)
        lastNewline = lastIndexOf '\n' beforeIdx
        colNum = if lastNewline == -1 then idx + 1 else idx - lastNewline
    in (lineNum, colNum)

-- Find last index of character in string
lastIndexOf :: Char -> String -> Int
lastIndexOf c str = 
    case reverse $ zip str [0..] of
        [] -> -1
        xs -> case filter ((== c) . fst) xs of
                [] -> -1
                ((_, idx):_) -> idx

-- Validate Go syntax structure
validateGoStructure :: [String] -> [SyntaxError]
validateGoStructure linesOfCode = 
    concatMap validateLine (zip [1..] linesOfCode)
  where
    validateLine :: (Int, String) -> [SyntaxError]
    validateLine (lineNum, line) = 
        let trimmed = trim line
            -- Skip empty lines and comments
            isComment = "//" `isPrefixOf` trimmed || "/*" `isPrefixOf` trimmed
            isEmpty = null trimmed
        in if isEmpty || isComment then []
           else validateGoLine lineNum line trimmed

-- Validate individual Go line
validateGoLine :: Int -> String -> String -> [SyntaxError]
validateGoLine lineNum fullLine trimmed
    | "package " `isPrefixOf` trimmed = validatePackageLine lineNum fullLine trimmed
    | "import " `isPrefixOf` trimmed = validateImportLine lineNum fullLine trimmed  
    | "func " `isPrefixOf` trimmed = validateFunctionLine lineNum fullLine trimmed
    | "var " `isPrefixOf` trimmed = validateVarLine lineNum fullLine trimmed
    | "const " `isPrefixOf` trimmed = validateVarLine lineNum fullLine trimmed
    | "type " `isPrefixOf` trimmed = validateTypeLine lineNum fullLine trimmed
    | "for " `isPrefixOf` trimmed = validateControlLine lineNum fullLine trimmed "for"
    | "if " `isPrefixOf` trimmed = validateControlLine lineNum fullLine trimmed "if"
    | "switch " `isPrefixOf` trimmed = validateControlLine lineNum fullLine trimmed "switch"
    | "return " `isPrefixOf` trimmed = validateReturnLine lineNum fullLine trimmed
    | otherwise = validateGeneralStatement lineNum fullLine trimmed

-- Validation functions for specific constructs
validatePackageLine :: Int -> String -> String -> [SyntaxError]
validatePackageLine lineNum fullLine trimmed = 
    let parts = words trimmed
    in case parts of
        ["package", _] -> []
        ["package"] -> [SyntaxError MissingPackageDeclaration 
                        "Package declaration missing package name" lineNum 1 fullLine]
        _ | "package" `elem` parts -> [SyntaxError InvalidStatement 
                                       "Invalid package declaration" lineNum 1 fullLine]
        _ -> []

validateImportLine :: Int -> String -> String -> [SyntaxError]
validateImportLine lineNum fullLine trimmed = 
    -- Check if it's a single import or import block
    let hasQuotes = "\"" `isInfixOf` trimmed
        hasParens = "(" `isInfixOf` trimmed
    in if not hasQuotes && not hasParens
       then [SyntaxError InvalidImport "Import missing package path" lineNum 1 fullLine]
       else []

validateFunctionLine :: Int -> String -> String -> [SyntaxError]
validateFunctionLine lineNum fullLine trimmed = 
    if not ("(" `isInfixOf` trimmed)
    then [SyntaxError InvalidFunctionDeclaration 
          "Function declaration missing parameters" lineNum 1 fullLine]
    else []

validateVarLine :: Int -> String -> String -> [SyntaxError]  
validateVarLine lineNum fullLine trimmed = 
    let parts = words trimmed
        isBlockDecl = length parts >= 2 && parts !! 1 == "("
    in if not isBlockDecl && length parts < 3
       then [SyntaxError InvalidStatement 
             "Variable declaration incomplete" lineNum 1 fullLine]
       else []

validateTypeLine :: Int -> String -> String -> [SyntaxError]
validateTypeLine lineNum fullLine trimmed = 
    let parts = words trimmed
    in if length parts < 3
       then [SyntaxError InvalidTypeDeclaration 
             "Type declaration incomplete" lineNum 1 fullLine]
       else []

validateControlLine :: Int -> String -> String -> String -> [SyntaxError]
validateControlLine _ _ _ _ = 
    -- Control structures can have opening brace on next line
    []

validateReturnLine :: Int -> String -> String -> [SyntaxError]
validateReturnLine lineNum fullLine trimmed = 
    if "+++" `isInfixOf` trimmed || "---" `isInfixOf` trimmed
    then [SyntaxError InvalidOperator 
          "Invalid operator (use ++ or -- instead)" lineNum 1 fullLine]
    else []

validateGeneralStatement :: Int -> String -> String -> [SyntaxError]
validateGeneralStatement lineNum fullLine trimmed = 
    if "+++" `isInfixOf` trimmed
    then [SyntaxError InvalidOperator "Invalid operator (+++)" lineNum 1 fullLine]
    else if "---" `isInfixOf` trimmed  
    then [SyntaxError InvalidOperator "Invalid operator (---)" lineNum 1 fullLine]
    else []

-- Count braces with context awareness
countBraces :: String -> Int
countBraces content = 
    let _ = foldl processChar initialState (zip content [0..])
        -- Also count braces that were properly closed
        (opens, closes) = countBracesInState content initialState 0 0
    in opens - closes
  where
    processChar state (char, idx) = updateState state char content idx
    
    countBracesInState :: String -> ParseState -> Int -> Int -> (Int, Int)
    countBracesInState [] _ opens closes = (opens, closes)
    countBracesInState (c:cs) state opens closes =
        let state' = updateState state c (c:cs) 0
            (opens', closes') = case c of
                '{' | not (inString state || inChar state || inSingleComment state || inMultiComment state) -> 
                    (opens + 1, closes)
                '}' | not (inString state || inChar state || inSingleComment state || inMultiComment state) -> 
                    (opens, closes + 1)
                _ -> (opens, closes)
        in countBracesInState cs state' opens' closes'

-- Utility: trim whitespace
trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace
