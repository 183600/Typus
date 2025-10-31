# Parser History

This document records the evolution of the Typus parser implementation. It used to carry a large, commented-out tokenizer-based prototype inside `src/Parser.hs`. The code has been removed from the module itself to keep the production parser focused and readable.

## Legacy Tokenizer Prototype

The initial parser experiments implemented a traditional tokenizer/recursive-descent pipeline. The tokenizer produced tokens such as `TIdentifier`, `TStringLiteral`, `TSymbol`, and `TDirective`, and the parser state tracked positions through `ParserState`. While this approach provided fine-grained control, it duplicated a substantial amount of logic that our downstream Go toolchain already handles.

The prototype also tried to interpret Typus directives (`//!`) inline and relied on manual brace tracking, but it predated the current directive semantics. Maintaining the legacy scaffolding inside `Parser.hs` became distracting once the line-oriented parser stabilized.

## Current Line-Based Parser

The active implementation (still in `src/Parser.hs`) operates directly on lines of source text. It:

- Collects file-level directives and Go build tags at the top of each file.
- Parses directive blocks marked with `{//! ...}` while balancing braces within the block body.
- Produces `TypusFile` structures comprised of per-block `BlockDirectives` and raw code segments.

This approach favors clarity and matches the directive-oriented structure of Typus sources without re-implementing a full lexer.

## Where to Find the Old Code

If you need to reference the legacy tokenizer code—for example, when revisiting more granular syntax handling—it remains available in the git history of this branch and the mainline prior to this cleanup. You can recover it from version control history whenever deeper inspection is required.

Keeping the historical context here allows `Parser.hs` to remain concise while preserving knowledge about why the tokenizer approach was abandoned.
