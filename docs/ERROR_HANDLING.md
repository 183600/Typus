# Enhanced Error Handling System

## Overview

The Typus compiler features a comprehensive error handling system that provides:

1. **Structured Error Types** - Rich error information instead of simple strings
2. **Location Tracking** - Precise line and column numbers for all errors
3. **Error Recovery** - Continue analysis after non-fatal errors
4. **Error Classification** - Fatal errors, errors, warnings, and info messages
5. **User-Friendly Messages** - Clear, actionable error descriptions with suggestions

## Architecture

### Core Components

#### 1. ErrorHandler Module (`src/ErrorHandler.hs`)

Provides the foundation for error handling:

```haskell
data TypeError = TypeError
    { errorId :: String
    , severity :: ErrorSeverity
    , category :: ErrorCategory
    , message :: Text
    , location :: ErrorLocation
    , context :: ErrorContext
    , recovery :: ErrorRecovery
    , suggestions :: [Text]
    , relatedErrors :: [TypeError]
    , errorChain :: [TypeError]
    , timestamp :: String
    }
```

**Error Severities:**
- `Fatal` - Stops compilation immediately
- `Error` - Prevents code generation but allows continued analysis
- `Warning` - Does not prevent compilation
- `Info` - Informational messages

**Error Categories:**
- `TypeChecking` - Type system errors
- `Ownership` - Memory ownership violations
- `Parsing` - Syntax errors
- `Semantic` - Semantic analysis errors
- `Runtime` - Runtime errors
- `Constraint` - Constraint violations
- `Inference` - Type inference errors
- `Integration` - Integration errors

#### 2. SourceLocation Module (`src/SourceLocation.hs`)

Tracks precise source code positions:

```haskell
data SourcePos = SourcePos
    { posLine :: Int
    , posColumn :: Int
    , posOffset :: Int
    }

data SourceSpan = SourceSpan
    { spanStart :: SourcePos
    , spanEnd :: SourcePos
    }
```

Features:
- Line and column tracking
- Character offset tracking
- Span merging for multi-line errors
- Tab-aware column calculation

#### 3. EnhancedErrorHandler Module (`src/EnhancedErrorHandler.hs`)

Integrates error handling with the compiler:

```haskell
data CompilerError = CompilerError
    { ceError :: TypeError
    , ceSourceContext :: Maybe String
    , ceStackTrace :: [String]
    , cePhase :: CompilationPhase
    }

data CompilationPhase
    = LexingPhase
    | ParsingPhase
    | TypeCheckingPhase
    | OwnershipAnalysisPhase
    | DependentTypeCheckingPhase
    | CodeGenerationPhase
    | OptimizationPhase
```

## Error Construction

### Creating Errors

Different types of errors can be created with appropriate context:

```haskell
-- Syntax error
syntaxError :: String -> Text -> SourcePos -> CompilerError
syntaxError errId msg pos = ...

-- Type error with suggestions
typeError :: String -> Text -> SourceSpan -> Maybe String -> [Text] -> CompilerError
typeError errId msg span context suggestions = ...

-- Ownership error with code context
ownershipError :: String -> Text -> SourceSpan -> String -> [Text] -> CompilerError
ownershipError errId msg span code suggestions = ...

-- Dependent type error
dependentTypeError :: String -> Text -> SourceSpan -> [Text] -> CompilerError
dependentTypeError errId msg span suggestions = ...
```

### Example Usage

```haskell
-- Create a type mismatch error
let err = typeError "E002" 
            "Type mismatch: expected int, got string"
            (spanBetween (posAt 15 10) (posAt 15 25))
            (Just "var x int = \"hello\"")
            ["Use strconv.Atoi to convert", "Change variable type to string"]
```

## Error Recovery

### Recovery Strategies

The system provides multiple recovery strategies:

```haskell
data ErrorRecovery = RecoveryStrategy
    { canRecover :: Bool
    , shouldContinue :: Bool
    , recoveryAction :: Maybe String
    , recoveryHint :: Maybe String
    , recoveryCost :: Int              -- Cost of recovery (0-100)
    , recoveryConfidence :: Float      -- Confidence (0.0-1.0)
    }
```

**Built-in Recovery Strategies:**

- `fatalRecovery` - Cannot recover (stops compilation)
- `errorRecovery` - Can recover but with effort
- `warningRecovery` - Easy recovery
- `skipRecovery` - Skip the problematic section
- `retryRecovery` - Retry with different approach
- `fallbackRecovery` - Use alternative implementation

### Using Recovery in Compilation

```haskell
compileTypusFile :: TypusFile -> CompilerM (String, [CompilerError])
compileTypusFile typusFile = do
    -- Phase 1: Syntax validation (recoverable)
    validateSyntax typusFile
    
    -- Phase 2: Type checking (continues on error)
    checkTypesWithRecovery typusFile
    
    -- Phase 3: Code generation (if no fatal errors)
    code <- generateGoCodeSafe typusFile
    
    -- Collect warnings
    warnings <- get
    return (code, warnings)
```

## Error Formatting

### Single Error Format

Errors are formatted with full context:

```
[2024-09-30 12:30:45.123] example.typus:15:10-15:25: [ERROR] [TypeChecking] Type mismatch: expected int, got string

Context: function: main, variable: x, type: int

Source Context:
  15 | var x int = "hello"
                  ^^^^^^^

Suggestions:
  💡 Use strconv.Atoi to convert
  💡 Change variable type to string

Phase: TypeCheckingPhase
✓ Recoverable error - compilation can continue
Recovery Confidence: 70%
```

### Multiple Errors Format

Errors are grouped by phase:

```
=== Compilation Errors ===

--- ParsingPhase (2 errors) ---

10:5: [ERROR] [Parsing] Unexpected token '}'
Phase: ParsingPhase
✗ Fatal error - compilation stopped

--- TypeCheckingPhase (3 errors) ---

15:10-15:25: [ERROR] [TypeChecking] Type mismatch
...
```

### Error Report

Comprehensive reports include statistics:

```
=== Error Summary ===
Total Errors: 5
Fatal: 0
Errors: 3
Warnings: 2
Info: 0

By Phase:
  ParsingPhase: 2
  TypeCheckingPhase: 3

By Category:
  Parsing: 2
  TypeChecking: 3

Recoverable: 5 / 5

=== Recommendations ===
• Fix fatal errors first - they prevent compilation
• Many parsing errors detected - check syntax carefully
• Consider adding more type annotations
```

## Location Tracking

### Source Position

Track exact position in source:

```haskell
-- Create position
let pos = posAt 10 5  -- Line 10, column 5

-- Advance position by character
let newPos = posAfter 'x' pos

-- Advance by text
let finalPos = advancePosByText "hello world" pos
```

### Source Spans

Track ranges in source:

```haskell
-- Create span
let span = spanBetween (posAt 5 1) (posAt 5 20)

-- Merge spans
let merged = mergeSpans span1 span2

-- Check if position is in span
let inSpan = isPosInSpan pos span
```

### Integration with Errors

```haskell
-- Convert to error location
let errLoc = toErrorLocationWithSpan span

-- Add location to error
let locatedErr = withSourceLocation err span
```

## Enhanced Parser

### Location-Aware Parsing

```haskell
data LocatedCodeBlock = LocatedCodeBlock
    { lcbBlock :: CodeBlock
    , lcbSpan :: SourceSpan
    , lcbLineStart :: Int
    , lcbLineEnd :: Int
    }

data LocatedTypusFile = LocatedTypusFile
    { ltfFile :: TypusFile
    , ltfBlocks :: [LocatedCodeBlock]
    , ltfSourceLines :: [String]
    }
```

### Parse with Location Tracking

```haskell
parseTypusWithLocations :: String -> ParserResult
parseTypusWithLocations input = ...
```

**Features:**
- Tracks exact line and column for each code block
- Provides source context for errors
- Validates structure during parsing
- Generates warnings for potential issues

## Error Statistics

### Analyzing Errors

```haskell
data ErrorStatistics = ErrorStatistics
    { esTotal :: Int
    , esFatal :: Int
    , esErrors :: Int
    , esWarnings :: Int
    , esInfo :: Int
    , esByPhase :: Map CompilationPhase Int
    , esByCategory :: Map ErrorCategory Int
    , esRecoverable :: Int
    }

-- Analyze errors
let stats = analyzeErrors errorList
```

### Using Statistics

Statistics help understand error patterns:

```haskell
-- Generate report with statistics
let report = generateDetailedReport errors

-- Check if too many errors of specific type
when (esTotal stats > 10) $
    putStrLn "Consider breaking down your code"

-- Identify problem areas
case lookup TypeChecking (esByCategory stats) of
    Just n | n > 5 -> putStrLn "Many type errors - add annotations"
    _ -> return ()
```

## User-Friendly Messages

### Message Simplification

Technical messages are automatically simplified:

```haskell
-- Before: "Malformed syntax in expression"
-- After:  "Syntax error: The code structure is incorrect. 
--          Check for missing brackets or semicolons."

let friendlyErr = makeUserFriendly err
```

### Automatic Suggestions

Context-aware suggestions based on error category:

```haskell
suggestFix :: CompilerError -> [Text]

-- For type errors:
--   ✓ Check the types of your variables
--   ✓ Make sure function return types match

-- For ownership errors:
--   ✓ Consider using references (&) instead of moving
--   ✓ Clone the value if you need multiple owners

-- For parsing errors:
--   ✓ Check for missing or extra brackets
--   ✓ Verify all statements end properly
```

## Integration Examples

### Basic Compilation

```haskell
import CompilerEnhanced

-- Compile with error handling
let result = compileWithErrors typusFile

case crCode result of
    Just code -> do
        putStrLn "Compilation successful!"
        writeFile "output.go" code
    Nothing -> do
        putStrLn "Compilation failed:"
        putStrLn $ formatCompilerErrors (crErrors result)

-- Show warnings even on success
unless (null (crWarnings result)) $
    putStrLn $ "Warnings:\n" ++ formatCompilerErrors (crWarnings result)
```

### Detailed Analysis

```haskell
-- Get detailed statistics
let stats = crStatistics result

-- Generate full report
let report = generateDetailedReport (crErrors result)
writeFile "error_report.txt" report

-- Make decisions based on errors
when (esFatal stats > 0) $
    putStrLn "Cannot proceed - fix fatal errors first"

when (esErrors stats > 10) $
    putStrLn "Too many errors - review code structure"
```

### Custom Error Handling

```haskell
-- Filter specific error types
let typeErrors = filter (\e -> category (ceError e) == TypeChecking) 
                        (crErrors result)

-- Find errors in specific phase
let parseErrors = filter (\e -> cePhase e == ParsingPhase) 
                         (crErrors result)

-- Get recoverable errors only
let recoverableErrors = filter canRecoverFrom (crErrors result)
```

## Best Practices

### 1. Error Construction

- Always provide error IDs for tracking
- Include source context when available
- Add helpful suggestions
- Use appropriate severity levels

### 2. Error Recovery

- Use fatal errors sparingly
- Allow recovery from non-critical errors
- Provide clear recovery hints
- Continue analysis to find multiple errors

### 3. Error Messages

- Be specific about the problem
- Explain why it's an error
- Suggest how to fix it
- Use user-friendly language

### 4. Location Tracking

- Track precise positions
- Include relevant source context
- Show pointer to error location
- Include surrounding code for context

### 5. Testing

- Test error generation
- Test error recovery
- Test error formatting
- Test location tracking
- Verify statistics accuracy

## Testing

### Running Tests

```bash
# Run all error handler tests
stack test --test-arguments "--match EnhancedErrorHandler"

# Run specific test group
stack test --test-arguments "--match 'Error Construction'"
```

### Test Coverage

The test suite covers:
- Error construction
- Error formatting
- Error recovery
- Location tracking
- Error statistics
- User-friendly messages
- Integration scenarios
- Example error cases

## Examples

See [`examples/error_handling_demo.typus`](../examples/error_handling_demo.typus) for comprehensive examples of various error scenarios and how they are handled.

## API Reference

### Core Functions

```haskell
-- Error construction
syntaxError :: String -> Text -> SourcePos -> CompilerError
typeError :: String -> Text -> SourceSpan -> Maybe String -> [Text] -> CompilerError
ownershipError :: String -> Text -> SourceSpan -> String -> [Text] -> CompilerError
dependentTypeError :: String -> Text -> SourceSpan -> [Text] -> CompilerError

-- Error formatting
formatCompilerError :: CompilerError -> String
formatCompilerErrors :: [CompilerError] -> String
generateDetailedReport :: [CompilerError] -> String

-- Error analysis
analyzeErrors :: [CompilerError] -> ErrorStatistics
generateRecommendations :: ErrorStatistics -> String

-- Location tracking
toErrorLocation :: SourcePos -> ErrorLocation
toErrorLocationWithSpan :: SourceSpan -> ErrorLocation
withSourceLocation :: CompilerError -> SourceSpan -> CompilerError

-- User-friendly
makeUserFriendly :: CompilerError -> CompilerError
suggestFix :: CompilerError -> [Text]
```

## Future Enhancements

Planned improvements:

1. **IDE Integration** - LSP support for real-time error checking
2. **Error Caching** - Cache errors for incremental compilation
3. **Machine Learning** - Learn from fixes to improve suggestions
4. **Internationalization** - Support for multiple languages
5. **Interactive Fixes** - Automated fix suggestions
6. **Error Visualization** - Graphical error display
7. **Error Metrics** - Track error patterns over time

## Conclusion

The enhanced error handling system provides comprehensive error management with:

✓ Structured, informative errors
✓ Precise location tracking
✓ Intelligent error recovery
✓ User-friendly messages
✓ Detailed statistics and reports
✓ Extensive test coverage

This system makes debugging easier and improves the developer experience significantly.