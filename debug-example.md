# Debugging Guide for Typus Compiler

This document explains how to use the debugging capabilities we've added to the Typus compiler.

## Overview

We've added two main debugging components:

1. **Debug Module** (`src/Debug.hs`) - A comprehensive debugging library with logging and breakpoint capabilities
2. **Debug CLI Tool** (`debug-cmd.hs`) - A command-line tool for managing debugging operations

## Using the Debug Module

### Basic Logging

```haskell
-- Import the debug module
import Debug

-- Add debug logs to your functions
parseFile :: FilePath -> IO AST
parseFile path = do
    debugLog "Parser.hs:42" "Starting to parse file"
    content <- readFile path
    debugLog "Parser.hs:44" "File read successfully"

    let ast = parseTokens content
    debugLog "Parser.hs:46" "Parsing completed"

    return ast
```

### Breakpoints

```haskell
compile :: AST -> IO ()
compile ast = do
    debugBreakpoint "Compiler.hs:100" "Starting compilation"

    -- Phase 1: Parse analysis
    debugInfo "Compiler.hs:103" "Running parse analysis"
    analyzed <- analyzeAst ast

    -- Phase 2: Type checking
    debugBreakpoint "Compiler.hs:106" "Starting type checking"
    typed <- typeCheck analyzed

    debugInfo "Compiler.hs:108" "Compilation completed successfully"
```

### Different Log Levels

```haskell
processToken :: Token -> IO ()
processToken token = do
    debugError "TokenProcessor:25" "Critical error in token processing"
    debugWarn "TokenProcessor:26" "Warning: suspicious token pattern"
    debugInfo "TokenProcessor:27" "Processing token normally"
    debugTrace "TokenProcessor:28" "Detailed trace information"
```

### Configurable Debugging

```haskell
main :: IO ()
main = do
    let debugConfig = DebugConfig
            { dcEnabled = True
            , dcLogLevel = 3  -- Info level
            , dcShowTime = True
            , dcShowLocation = True
            }

    withDebugConfig debugConfig $ do
        -- Run your compiler with debug config
        result <- compileProgram
        return result
```

## Using the Debug CLI Tool

The `debug-cmd.hs` script provides convenient command-line debugging operations.

### Build with Debug Information

```bash
./debug-cmd.hs build
```

This builds the project with debug flags enabled and includes debugging symbols.

### Run Tests with Debug Output

```bash
./debug-cmd.hs test
```

Runs all tests with verbose debug output, making it easier to see what's happening.

### Set Breakpoints

```bash
# Set a breakpoint in Parser.hs at line 42
./debug-cmd.hs break src/Parser.hs 42
```

This adds a breakpoint comment to the specified location.

### Add Logging

```bash
# Add debug logging to Compiler.hs
./debug-cmd.hs log src/Compiler.hs
```

This automatically adds debug log statements to key functions in the file.

### Enable Tracing

```bash
./debug-cmd.hs trace
```

Shows how to add execution tracing to your code.

### Performance Profiling

```bash
./debug-cmd.hs profile
```

Builds the project with profiling enabled and shows how to generate performance profiles.

## Debugging Workflow

### 1. When You Encounter a Problem

1. **Add logging** to suspect functions:
   ```bash
   ./debug-cmd.hs log src/Parser.hs
   ./debug-cmd.hs log src/Compiler.hs
   ```

2. **Set breakpoints** at critical points:
   ```bash
   ./debug-cmd.hs break src/Parser.hs 156
   ./debug-cmd.hs break src/Compiler.hs 89
   ```

3. **Build and run** with debug output:
   ```bash
   ./debug-cmd.hs build
   stack exec -- typus compile example.typus
   ```

### 2. For Performance Issues

1. **Enable profiling**:
   ```bash
   ./debug-cmd.hs profile
   ```

2. **Run with profiling**:
   ```bash
   stack exec -- typus -- +RTS -p compile example.typus
   ```

3. **Analyze the profile** report generated.

### 3. For Complex Bugs

1. **Add comprehensive tracing**:
   ```bash
   ./debug-cmd.hs trace
   ```

2. **Manually add trace statements** using the Debug module:
   ```haskell
   debugTrace "Module:line" "Entering complex function"
   -- ... complex logic ...
   debugTrace "Module:line" "Exiting complex function"
   ```

3. **Run with maximum verbosity** to see the execution flow.

## Example Debug Session

Let's say you have a bug in the parser where it's not correctly handling a specific token type.

### Step 1: Add Logging
```bash
./debug-cmd.hs log src/Parser.hs
```

### Step 2: Set Breakpoint
```bash
./debug-cmd.hs break src/Parser.hs 234
```

### Step 3: Build and Run
```bash
./debug-cmd.hs build
stack exec -- typus parse problematic.typus
```

### Step 4: Analyze Output
The debug output will show:
- Log messages from each parsing stage
- Breakpoint where execution pauses
- Detailed state information

### Step 5: Fix and Verify
After fixing the issue:
```bash
./debug-cmd.hs test
```

## Debug Configuration

You can customize debugging behavior:

```haskell
-- Minimal debugging (errors only)
minimalConfig = DebugConfig
    { dcEnabled = True
    , dcLogLevel = 1
    , dcShowTime = False
    , dcShowLocation = True
    }

-- Maximum debugging
verboseConfig = DebugConfig
    { dcEnabled = True
    , dcLogLevel = 4
    , dcShowTime = True
    , dcShowLocation = True
    }

-- Performance mode (no debugging)
performanceConfig = DebugConfig
    { dcEnabled = False
    , dcLogLevel = 0
    , dcShowTime = False
    , dcShowLocation = False
    }
```

## Best Practices

1. **Use appropriate log levels**:
   - `debugError` for critical failures
   - `debugWarn` for potential issues
   - `debugInfo` for normal flow
   - `debugTrace` for detailed debugging

2. **Include location information**:
   - Always specify the module and line number
   - This helps locate issues quickly

3. **Remove debug code from production**:
   - Use the debug configuration to disable debugging
   - Consider conditional compilation for performance-critical code

4. **Use breakpoints sparingly**:
   - Too many breakpoints can make debugging tedious
   - Focus on critical decision points

5. **Combine with unit tests**:
   - Use debug output to understand test failures
   - Add debug assertions to catch issues early

## Integration with IDE

You can integrate this debugging system with your IDE:

1. **Set breakpoints** using the CLI tool
2. **Add logging** via keyboard shortcuts
3. **View debug output** in the IDE console
4. **Profile performance** using the profiling commands

This debugging system provides comprehensive tools for understanding and fixing issues in the Typus compiler.