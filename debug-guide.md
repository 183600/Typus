# Debug Guide for Typus Compiler

This guide explains how to use the debugging capabilities in the Typus compiler project.

## Debug Module Overview

The `Debug` module (`src/Debug.hs`) provides comprehensive debugging capabilities including:
- Logging with timestamps and location information
- Breakpoints that pause execution
- Trace-level debugging
- Configurable log levels

## Basic Usage

### 1. Import the Debug Module

```haskell
import Debug
import Control.Monad.IO.Class (liftIO)  -- Required for monadic operations
```

### 2. Basic Logging

```haskell
-- Simple logging with location info
debugLog "myFunction" "Starting computation"

-- Log a variable value
debugLog "myFunction" $ "Variable x = " ++ show x
```

### 3. Breakpoints

```haskell
-- Add a breakpoint that pauses execution
debugBreakpoint "myFunction" "About to perform critical operation"

-- After breakpoint, execution pauses and waits for Enter key press
```

### 4. Tracing

```haskell
-- Fine-grained tracing (only shown at debug log level)
debugTrace "myFunction" "Entering loop"
```

## Debug Configuration

### Default Configuration

The `defaultDebugConfig` enables debug mode with these settings:
- Enabled: True
- Log Level: 3 (Info level)
- Show Time: True
- Show Location: True

### Custom Configuration

```haskell
let customConfig = DebugConfig
        { dcEnabled = True
        , dcLogLevel = 4  -- Debug level (shows trace messages)
        , dcShowTime = False
        , dcShowLocation = True
        }

-- Run an action with custom config
withDebugConfig customConfig $ do
    debugLog "myFunction" "This uses custom config"
    debugTrace "myFunction" "This trace will now be visible"
```

### Log Levels

- **0**: Off (no logging)
- **1**: Error level
- **2**: Warning level
- **3**: Info level (default)
- **4**: Debug level (includes trace messages)

## Command Line Breakpoints

To debug the compiler from the command line, you can:

### 1. Compile with Debug Information

```bash
# Build the project
stack build

# Or with cabal
cabal build
```

### 2. Add Debug Statements to Code

Add debugging calls in the source code where you want to investigate:

```haskell
-- In Compiler.hs
compileFile filePath = do
    debugLog "compileFile" $ "Compiling: " ++ filePath
    debugBreakpoint "compileFile" "Starting compilation"

    -- ... rest of compilation logic
    debugTrace "compileFile" "Generated intermediate representation"
```

### 3. Run and Interact with Breakpoints

```bash
# Run the compiler with debug output
stack exec -- typus compile myfile.typus

# The compiler will pause at breakpoints and wait for Enter key
# You can examine the debug output and then continue by pressing Enter
```

## Example Integration

Here's how to integrate debugging into a compiler function:

```haskell
-- In Parser.hs
parseTokens :: [Token] -> ParseResult [Stmt]
parseTokens tokens = do
    debugLog "parseTokens" $ "Parsing " ++ show (length tokens) ++ " tokens"
    debugBreakpoint "parseTokens" "Starting token parsing"

    result <- parseStatements tokens

    debugTrace "parseTokens" $ "Parsed " ++ show (length result) ++ " statements"
    debugLog "parseTokens" "Parsing completed successfully"

    return result
```

## Debugging Workflow

1. **Identify the problem area**: Determine which part of the compiler needs investigation
2. **Add debug statements**: Insert `debugLog`, `debugBreakpoint`, and `debugTrace` calls
3. **Configure debug level**: Set appropriate log level for your needs
4. **Run with debug**: Execute the compiler and observe the output
5. **Interact with breakpoints**: When paused at breakpoints, examine debug output before continuing
6. **Iterate**: Add more debug statements or adjust configuration as needed

## Best Practices

1. **Use descriptive location strings**: Use function names or logical section names
2. **Include relevant data in log messages**: Show variable values and important state
3. **Use appropriate log levels**: Use `debugLog` for important info, `debugTrace` for detailed flow
4. **Place breakpoints strategically**: Put them before critical operations or decision points
5. **Clean up debug statements**: Remove or disable debug calls when debugging is complete

## Troubleshooting

### Debug Output Not Showing

Check that:
- Debug is enabled in configuration (`dcEnabled = True`)
- Log level is appropriate for your debug calls
- You're using the correct import (`import Debug`)

### Breakpoints Not Pausing

Check that:
- Debug is enabled in configuration
- You're running in an interactive terminal (breakpoints need stdin)
- No other process is consuming stdin

## Compilation Issues

If you encounter compilation errors related to the Debug module:

1. Ensure all dependencies are installed: `stack build`
2. Check imports: `import Debug` and `import Control.Monad.IO.Class`
3. Verify the module is in the correct location: `src/Debug.hs`

## Testing the Debug Module

Run the debug example to verify functionality:

```bash
# Compile the debug example
stack ghc -- -isrc -i./src debug-example.hs -o debug-example

# Run it (provide empty input for breakpoints)
echo -e "\n\n" | ./debug-example
```

This will demonstrate all debug features including logging, breakpoints, and configuration options.