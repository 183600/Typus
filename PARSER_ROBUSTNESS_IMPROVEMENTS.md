# Parser Robustness and AST Reuse Improvements

## Overview

This document describes the improvements made to move from string-based pattern matching to AST-based analysis in the Typus compiler, addressing the robustness issues identified in the ticket.

## Problems Addressed

### 1. String-Based Symbol Collection (Analyzer/SymbolTable.hs)
**Problem**: `collectSymbolsAndTypes` relied heavily on `isPrefixOf`, `words`, and manual string slicing which was prone to errors on legitimate but stylistically different code.

**Solution**: 
- Added `collectSymbolsFromAST` function that works directly with the parsed `GoModule` AST
- Modified `collectSymbolsAndTypes` to first attempt AST parsing and fall back to line-by-line processing only on parse failure
- Processes structured AST nodes (`GoFunc`, `GoVar`, `GoConst`, `GoType`) instead of raw strings

### 2. String-Based Value Copy Detection (Compiler/OwnershipChecker.hs)
**Problem**: `extractValueCopyVars` used hardcoded character checks like searching for `"`, `true`, `false`, and digits, which fails on complex expressions.

**Solution**:
- Created new module `Compiler.ValueAnalysis` with structured analysis
- Implemented `ValueKind` ADT distinguishing between `ValueCopy`, `Reference`, and `Unknown`
- Added proper expression analysis functions:
  - `isStringLiteral`: Checks for quoted strings  
  - `isBooleanLiteral`: Checks for true/false
  - `isNumericLiteral`: Checks for numeric constants
  - `isValueCompositeLiteral`: Checks for value type composite literals
  - `isReferenceInit`: Checks for `&`, `make()`, `new()`, arrays, maps
- Integrated value analysis into the IR pipeline via `SemanticIR.semanticValueInfo`

### 3. IR Pipeline Enhancement (Compiler/IR.hs)
**Solution**:
- Extended `SemanticIR` to include `semanticValueInfo :: [ValueInfo]`
- Value analysis is performed once during `buildSemanticIR` and reused
- Ownership checker now receives pre-analyzed value information
- Moved value/reference determination from runtime string checking to compile-time IR analysis

### 4. Type Checker Improvements (Compiler/TypeChecker.hs)
**Existing**: The type checker already used AST parsing via `parseGoModule` but then fell back to string matching on extracted lines.

**Enhancement**: Extended `extractDeclarationsFromModule` to also gather type declarations, ensuring comprehensive AST-based extraction.

## Key Benefits

1. **Robustness**: AST-based analysis handles whitespace variations, comments, and formatting differences
2. **Correctness**: Proper expression parsing instead of substring matching
3. **Performance**: Value analysis done once in IR, not repeatedly on raw strings  
4. **Maintainability**: Centralized analysis logic in dedicated modules
5. **Extensibility**: Easy to add new value kinds or expression patterns

## Files Modified

- `src/Compiler/ValueAnalysis.hs` - New module for value semantic analysis
- `src/Compiler/IR.hs` - Extended SemanticIR with value information
- `src/Compiler/OwnershipChecker.hs` - Uses IR-level value info, legacy fallback
- `src/Compiler.hs` - Updated compilation pipeline to use value-aware checking
- `src/Analyzer/SymbolTable.hs` - Added AST-based symbol collection
- `typus.cabal` - Registered new module

## Backward Compatibility

- Old `checkOwnership` function preserved for existing code
- New `checkOwnershipWithValueInfo` function for IR-based checking
- Legacy `extractValueCopyVarsLegacy` retained but not used in main pipeline
- Symbol collection falls back to line-by-line if AST parsing fails

## Testing Recommendations

1. Test with various code formatting styles (different whitespace, line breaks)
2. Test complex expressions: function calls, method chains, nested structures
3. Test edge cases: multi-line declarations, comments interspersed with code
4. Verify ownership analysis correctly identifies value vs reference types
5. Ensure symbol table correctly extracts declarations from AST

## Future Enhancements

1. Extend `ValueAnalysis` to handle function return types
2. Add flow-sensitive analysis to track value/reference transformations
3. Integrate with dependent type checker for richer semantic analysis
4. Consider full expression AST parsing for complex initializers
