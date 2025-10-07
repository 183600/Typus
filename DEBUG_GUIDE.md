# Typus Compiler Debugging Guide

This guide explains how to use the debugging capabilities added to the Typus compiler.

## Overview

The debugging system includes:
1. **Debug Module**: Basic logging and breakpoint functionality
2. **CommandLineDebug Module**: Interactive command-line debugging
3. **DebugIntegration Module**: Integration hooks for compiler phases
4. **Debug CLI Tool**: Command-line interface for debugging management

## Key Features

### 1. Debug Logging

The `Debug` module provides several logging functions:

```haskell
import Debug

-- Basic logging with location and message
debugLog "parser" "Starting parsing of file"
debugInfo "compiler" "Compilation phase started"
debugWarn "typechecker" "Potential type issue detected"
debugError "generator" "Code generation failed"
debugTrace "analyzer" "Detailed execution trace"
```

### 2. Breakpoints

Interactive breakpoints can be set using the `CommandLineDebug` module:

```haskell
import CommandLineDebug

config <- defaultCLIDebugConfig

-- Set breakpoints at specific locations
setBreakpoint config "parse:main"
setBreakpoint config "compile:example.typus"
setBreakpoint config "ownership:analysis"

-- List all breakpoints
listBreakpoints config

-- Clear all breakpoints
clearBreakpoints config
```

### 3. Command-Line Debug Tool

Use the `debug-cli.hs` script for interactive debugging:

```bash
# Start the debug CLI
runhaskell debug-cli.hs

# Available commands in debug CLI:
help           - Show help
status         - Show debug status
set breakpoint LOC - Set breakpoint at location
break LOC      - Set breakpoint at location
list           - List all breakpoints
clear          - Clear all breakpoints
toggle         - Toggle debug output
level N        - Set debug level (0-4)
enable/disable - Enable/disable debugging
test LOC       - Test breakpoint at location
```

### 4. Compiler Integration

The `DebugIntegration` module provides hooks for compiler phases:

```haskell
import DebugIntegration

config <- setupCompilerDebugging

-- Wrap compiler phases with debugging
withDebugging config "parse" $ do
    -- Parse logic here
    return ()

debugParseStep config "example.typus" $ do
    -- Parse with breakpoint support
    return ()

debugCompileStep config "example.typus" $ do
    -- Compile with breakpoint support
    return ()
```

## Using the Debug System

### Step 1: Enable Debugging

```haskell
import DebugIntegration

-- Initialize debugging with default configuration
config <- setupCompilerDebugging
```

### Step 2: Set Breakpoints

```bash
# In the debug CLI
set breakpoint parse:main
set breakpoint compile:example.typus
set breakpoint ownership:analysis
```

### Step 3: Run Compiler with Debugging

```haskell
-- Compiler execution will stop at breakpoints
main = do
    config <- setupCompilerDebugging

    debugCompilerStart config "example.typus"

    debugParseStep config "example.typus" $ do
        -- Parse logic - will stop at parse:main breakpoint
        parseFile "example.typus"

    debugCompileStep config "example.typus" $ do
        -- Compile logic - will stop at compile:example.typus breakpoint
        compileAST ast

    debugOwnershipStep config "example.typus" $ do
        -- Ownership analysis - will stop at ownership:analysis breakpoint
        analyzeOwnership ast

    debugCompilerEnd config "example.typus"
```

### Step 4: Interactive Debugging

When a breakpoint is hit, the debugger will pause and show:

```
=== BREAKPOINT HIT ===
Location: parse:main
Available commands:
  c, continue - Continue execution
  s, step - Step to next breakpoint
  l, list - List all breakpoints
  d, disable - Disable debugging
  e, enable - Enable debugging
  q, quit - Quit program
  h, help - Show this help
debug>
```

## Debug Levels

The debugging system supports multiple log levels:

- **0**: Off
- **1**: Error only
- **2**: Warning and above
- **3**: Info and above (default)
- **4**: Debug and above (includes trace)

Set the debug level:

```bash
# In debug CLI
level 4  # Show all debug information
level 2  # Show warnings and errors only
level 0  # Disable debugging
```

## Examples

### Example 1: Basic Debugging

```haskell
import Debug
import DebugIntegration

main = do
    config <- setupCompilerDebugging

    debugLog "main" "Starting compiler"
    debugInfo "main" "Loading configuration"

    -- Your compiler logic here

    debugLog "main" "Compiler finished"
```

### Example 2: Breakpoint Debugging

```haskell
import CommandLineDebug
import DebugIntegration

main = do
    config <- setupCompilerDebugging

    -- Set breakpoints for critical sections
    setBreakpoint config "parse:error"
    setBreakpoint config "compile:optimize"

    -- Run with debugging
    runWithCLIDebug config "main" $ do
        -- Compiler logic that may hit breakpoints
        return ()
```

### Example 3: Phase Debugging

```haskell
import DebugIntegration

main = do
    config <- setupCompilerDebugging

    debugParseStep config "input.typus" $ do
        -- Parse with full debugging support
        parseResult <- parseFile "input.typus"
        return parseResult

    debugCompileStep config "input.typus" $ do
        -- Compile with full debugging support
        compileResult <- compileAST parseResult
        return compileResult
```

## Files Added

1. **`src/Debug.hs`**: Core debugging functionality with logging and breakpoints
2. **`src/CommandLineDebug.hs`**: Interactive command-line debugging system
3. **`src/DebugIntegration.hs`**: Integration hooks for compiler phases
4. **`debug-cli.hs`**: Command-line tool for debugging management
5. **`DEBUG_GUIDE.md`**: This guide

## Troubleshooting

### Common Issues

1. **Breakpoints not hitting**: Ensure debugging is enabled with `enable` command
2. **No debug output**: Check debug level with `status` command, increase with `level 4`
3. **Interactive mode not working**: Make sure `cldInteractive` is set to `True`
4. **Import errors**: Ensure all debug modules are properly imported and exported

### Debug Commands Reference

| Command | Description | Example |
|---------|-------------|---------|
| `help` | Show help | `help` |
| `status` | Show debug status | `status` |
| `set breakpoint LOC` | Set breakpoint | `set breakpoint parse:main` |
| `break LOC` | Set breakpoint (short) | `break compile:main` |
| `list` | List breakpoints | `list` |
| `clear` | Clear all breakpoints | `clear` |
| `toggle` | Toggle debug output | `toggle` |
| `level N` | Set debug level | `level 4` |
| `enable` | Enable debugging | `enable` |
| `disable` | Disable debugging | `disable` |
| `test LOC` | Test breakpoint | `test parse:main` |

This debugging system provides comprehensive support for debugging the Typus compiler with interactive breakpoints, detailed logging, and phase-specific debugging hooks.