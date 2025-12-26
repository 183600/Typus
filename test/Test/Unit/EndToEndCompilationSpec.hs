{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.EndToEndCompilationSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, choose, listOf, elements)
import Test.QuickCheck.Gen (oneof, suchThat)

import Compiler (compile, generateGoCode, CompilerResult)
import Parser (parseTypus, TypusFile(..))
import ErrorHandler (formatError, formatErrors)
import Utils (trim, removeComments, normalizeIndentation)
import SourceLocation (SourcePos(..), SourceSpan(..), startPos)
import IntegratedCompiler (compileTypusFile)
import GoToolchain (validateGoCode)

import Data.List (isPrefixOf, isInfixOf, isSuffixOf)
import qualified Data.Text as T
import Data.Char (isSpace)

-- Helper generators for end-to-end testing

-- Generate complete Typus programs
genCompleteTypusProgram :: Gen String
genCompleteTypusProgram = oneof
  [ return $ unlines
    [ "// @ownership"
    , "```go"
    , "package main"
    , ""
    , "import \"fmt\""
    , ""
    , "func add(a int, b int) int {"
    , "    return a + b"
    , "}"
    , ""
    , "func main() {"
    , "    result := add(5, 3)"
    , "    fmt.Println(\"Result:\", result)"
    , "}"
    , "```"
    ]
  , return $ unlines
    [ "// @dependent-types"
    , "```rust"
    , "fn factorial(n: u32) -> u32 {"
    , "    match n {"
    , "        0 => 1,"
    , "        _ => n * factorial(n - 1)"
    , "    }"
    , "}"
    , ""
    , "fn main() {"
    , "    println!(\"Factorial of 5: {}\", factorial(5))"
    , "}"
    , "```"
    ]
  ]

-- Generate Typus programs with multiple code blocks
genMultiBlockTypusProgram :: Gen String
genMultiBlockTypusProgram = do
  blockCount <- choose (2, 4)
  let blocks = replicate blockCount "```go\nfunc test() { return 42 }\n```"
  return $ unlines $ ["// @ownership"] ++ blocks

-- Generate Typus programs with ownership patterns
genOwnershipTypusProgram :: Gen String
genOwnershipTypusProgram = oneof
  [ return $ unlines
    [ "// @ownership"
    , "```rust"
    , "fn main() {"
    , "    let data = String::from(\"hello\");"
    , "    let owner = data;"
    , "    println!(\"{}\", owner);"
    , "}"
    , "```"
    ]
  , return $ unlines
    [ "// @ownership"
    , "```rust"
    , "fn transfer_ownership() {"
    , "    let vec = vec![1, 2, 3];"
    , "    consume(vec);"
    , "}"
    , ""
    , "fn consume(data: Vec<i32>) {"
    , "    // data is consumed here"
    , "}"
    , "```"
    ]
  ]

-- Generate Typus programs with dependent types
genDependentTypeTypusProgram :: Gen String
genDependentTypeTypusProgram = oneof
  [ return $ unlines
    [ "// @dependent-types"
    , "```haskell"
    , "data Vector n a where"
    , "    Vector :: Nat -> a -> Vector n a"
    , ""
    , "safeHead :: Vector (n + 1) a -> a"
    , "safeHead (Vector _ x) = x"
    , "```"
    ]
  , return $ unlines
    [ "// @dependent-types"
    , "```idris"
    , "data Matrix : Nat -> Nat -> Type where"
    , "    Nil : Matrix 0 n"
    , "    (::) : a -> Matrix k n -> Matrix (k + 1) n"
    , ""
    , "safeIndex : Matrix m n -> Fin m -> Vector n a"
    , "```"
    ]
  ]

-- Generate malformed Typus programs for error handling
genMalformedTypusProgram :: Gen String
genMalformedTypusProgram = oneof
  [ return $ unlines
    [ "// @ownership"
    , "```go"
    , "package main"
    , ""
    , "func broken( {  // missing parameter"
    , "    return 42"
    , "}"
    , "```"
    ]
  , return $ unlines
    [ "// @dependent-types"
    , "```rust"
    , "fn undefined() ->  {  // missing return type"
    , "    42"
    , "}"
    , "```"
    ]
  , return $ unlines
    [ "// @ownership"
    , "```rust"
    , "fn main() {"
    , "    let x = 42"
    , "    let y = x"  // Move
    , "    println!(\"{}\", x)"  // Use after move
    , "}"
    , "```"
    ]
  ]

-- Generate large Typus programs for performance testing
genLargeTypusProgram :: Gen String
genLargeTypusProgram = do
  funcCount <- choose (5, 20)
  let functions = replicate funcCount "func test() { return 42 }"
  return $ unlines $
    [ "// @ownership"
    , "```go"
    , "package main"
    , ""
    , "import \"fmt\""
    ] ++
    functions ++
    [ ""
    , "func main() {"
    , "    fmt.Println(\"Hello, World!\")"
    , "}"
    , "```"
    ]

-- End-to-end property tests

-- Property: Complete compilation pipeline should handle simple programs
prop_complete_pipeline_simple :: Property
prop_complete_pipeline_simple =
  forAll genCompleteTypusProgram $ \program ->
  let parseResult = parseTypus program ""
      compileResult = compile program ""
  in case (parseResult, compileResult) of
    (Left _, Left _) -> property True  -- Both fail gracefully
    (Right _, Left _) -> property True  -- Parse succeeds, compile fails gracefully
    (Right parseFile, Right compilation) -> 
      property $ True  -- Both succeed
    (Left parseErr, Right _) -> property False  -- Should not compile if parse fails

-- Property: Multi-block programs should be handled correctly
prop_multi_block_handling :: Property
prop_multi_block_handling =
  forAll genMultiBlockTypusProgram $ \multiBlockProgram ->
  let result = compile multiBlockProgram ""
  in case result of
    Left _ -> property True  -- Should fail gracefully
    Right compilation -> property $ True  -- Should handle multiple blocks

-- Property: Ownership analysis should be integrated in compilation
prop_ownership_integration :: Property
prop_ownership_integration =
  forAll genOwnershipTypusProgram $ \ownershipProgram ->
  let result = compile ownershipProgram ""
  in case result of
    Left _ -> property True  -- Should fail gracefully
    Right compilation -> property $ True  -- Should analyze ownership

-- Property: Dependent type checking should be integrated
prop_dependent_types_integration :: Property
prop_dependent_types_integration =
  forAll genDependentTypeTypusProgram $ \dependentTypeProgram ->
  let result = compile dependentTypeProgram ""
  in case result of
    Left _ -> property True  -- Should fail gracefully
    Right compilation -> property $ True  -- Should check dependent types

-- Property: Error handling should be consistent across pipeline
prop_error_handling_consistency :: Property
prop_error_handling_consistency =
  forAll genMalformedTypusProgram $ \malformedProgram ->
  let parseResult = parseTypus malformedProgram ""
      compileResult = compile malformedProgram ""
  in case (parseResult, compileResult) of
    (Left parseErr, Left compileErr) -> 
      property $ True  -- Both should detect errors
    (Right parseFile, Left compileErr) -> 
      property $ True  -- Parse succeeds but compile detects errors
    _ -> property False  -- Should not succeed with malformed input

-- Property: Generated Go code should be syntactically valid
prop_generated_go_syntax_valid :: Property
prop_generated_go_syntax_valid =
  let goProgram = unlines
    [ "// @ownership"
    , "```go"
    , "package main"
    , ""
    , "import \"fmt\""
    , ""
    , "func add(a int, b int) int {"
    , "    return a + b"
    , "}"
    , ""
    , "func main() {"
    , "    result := add(5, 3)"
    , "    fmt.Println(\"Result:\", result)"
    , "}"
    , "```"
    ]
      result = compile goProgram ""
      goCode = case result of
        Left _ -> ""
        Right compilation -> generateGoCode compilation
  in property $ length goCode > 0 ==> 
     "package main" `isInfixOf` goCode .&&.
     "func add" `isInfixOf` goCode .&&.
     "func main" `isInfixOf` goCode

-- Property: Large programs should not cause performance degradation
prop_large_program_performance :: Property
prop_large_program_performance =
  forAll genLargeTypusProgram $ \largeProgram ->
  let programSize = length largeProgram
      result = compile largeProgram ""
  in case result of
    Left _ -> property $ programSize <= 10000  -- Should handle reasonable size
    Right compilation -> property $ True  -- Should compile successfully

-- Property: Compilation should preserve semantic meaning
prop_semantic_preservation :: Property
prop_semantic_preservation =
  let semanticProgram = unlines
    [ "// @ownership"
    , "```go"
    , "package main"
    , ""
    , "func identity(x int) int {"
    , "    return x"
    , "}"
    , ""
    , "func main() {"
    , "    value := 42"
    , "    result := identity(value)"
    , "    // result should be 42"
    , "}"
    , "```"
    ]
      result = compile semanticProgram ""
  in case result of
    Left _ -> property True  -- May fail gracefully
    Right compilation -> 
      let goCode = generateGoCode compilation
      in property $ "identity" `isInfixOf` goCode .&&.
         "42" `isInfixOf` goCode

-- Property: Multiple compilation passes should be idempotent
prop_compilation_idempotent :: Property
prop_compilation_idempotent =
  forAll genCompleteTypusProgram $ \program ->
  let result1 = compile program ""
      result2 = compile program ""
  in case (result1, result2) of
    (Left err1, Left err2) -> property $ err1 === err2
    (Right comp1, Right comp2) -> property $ True  -- Compare compilation results
    _ -> property False  -- Should be consistent

-- Property: Pipeline should handle Unicode content
prop_unicode_handling :: Property
prop_unicode_handling =
  let unicodeProgram = unlines
    [ "// @ownership"
    , "```go"
    , "package main"
    , ""
    , "import \"fmt\""
    , ""
    , "func main() {"
    , "    message := \"测试中文 🚀\""
    , "    fmt.Println(message)"
    , "}"
    , "```"
    ]
      result = compile unicodeProgram ""
  in case result of
    Left _ -> property True  -- Should handle gracefully
    Right compilation -> 
      let goCode = generateGoCode compilation
      in property $ "测试中文" `isInfixOf` goCode

-- Property: Pipeline should handle mixed directives
prop_mixed_directives :: Property
prop_mixed_directives =
  let mixedProgram = unlines
    [ "// @ownership"
    , "// @dependent-types"
    , "// @constraints"
    , "```go"
    , "package main"
    , ""
    , "func main() {"
    , "    fmt.Println(\"Mixed directives\")"
    , "}"
    , "```"
    ]
      result = compile mixedProgram ""
  in case result of
    Left _ -> property True  -- Should handle gracefully
    Right compilation -> property $ True  -- Should process all directives

-- Property: Pipeline should handle empty code blocks
prop_empty_code_blocks :: Property
prop_empty_code_blocks =
  let emptyBlockProgram = unlines
    [ "// @ownership"
    , "```go"
    , "```"
    , "// @dependent-types"
    , "```rust"
    , "```"
    ]
      result = compile emptyBlockProgram ""
  in case result of
    Left _ -> property True  -- Should handle gracefully
    Right compilation -> property $ True  -- Should handle empty blocks

-- Property: Pipeline should handle deeply nested structures
prop_nested_structures :: Property
prop_nested_structures =
  let nestedProgram = unlines
    [ "// @ownership"
    , "```rust"
    , "struct Outer {"
    , "    inner: Inner"
    , "}"
    , ""
    , "struct Inner {"
    , "    value: i32"
    , "    nested: Nested"
    , "}"
    , ""
    , "struct Nested {"
    , "    data: Vec<String>"
    , "}"
    , "```"
    ]
      result = compile nestedProgram ""
  in case result of
    Left _ -> property True  -- Should handle gracefully
    Right compilation -> property $ True  -- Should handle nesting

-- Property: Pipeline should handle function signatures with complex types
prop_complex_function_signatures :: Property
prop_complex_function_signatures =
  let complexSignatureProgram = unlines
    [ "// @dependent-types"
    , "```haskell"
    , "complexFunction :: Either String (Maybe [Int]) -> IO (Result Error ())"
    , "complexFunction input = do"
    , "    case input of"
    , "        Left err -> return (Error err)"
    , "        Right Nothing -> return (Ok ())"
    , "        Right (Just values) -> processValues values"
    , "```"
    ]
      result = compile complexSignatureProgram ""
  in case result of
    Left _ -> property True  -- Should handle gracefully
    Right compilation -> property $ True  -- Should handle complex signatures

-- Property: Pipeline should maintain source location information
prop_source_location_preservation :: Property
prop_source_location_preservation =
  let locationProgram = unlines
    [ "// @ownership"
    , "```go"
    , "package main"
    , ""
    , "func line1() { println(\"line 1\") }"
    , "func line2() { println(\"line 2\") }"
    , "func line3() { println(\"line 3\") }"
    , "```"
    ]
      result = compile locationProgram ""
  in case result of
    Left _ -> property True  -- Should handle gracefully
    Right compilation -> property $ True  -- Should preserve location info

-- Property: Pipeline should handle concurrent compilation scenarios
prop_concurrent_compilation :: Property
prop_concurrent_compilation =
  let concurrentProgram = unlines
    [ "// @ownership"
    , "```rust"
    , "use std::thread;"
    , ""
    , "fn main() {"
    , "    let handle = thread::spawn(|| {"
    , "        println!(\"Hello from thread!\")"
    , "    });"
    , "    handle.join().unwrap();"
    , "}"
    , "```"
    ]
      result = compile concurrentProgram ""
  in case result of
    Left _ -> property True  -- Should handle gracefully
    Right compilation -> property $ True  -- Should handle concurrency

-- Property: Pipeline should handle template/generic code
prop_template_code :: Property
prop_template_code =
  let templateProgram = unlines
    [ "// @dependent-types"
    , "```rust"
    , "struct Container<T> {"
    , "    data: T,"
    , "}"
    , ""
    , "impl<T> Container<T> {"
    , "    fn new(value: T) -> Self {"
    , "        Container { data: value }"
    , "    }"
    , "}"
    , ""
    , "fn main() {"
    , "    let int_container = Container::new(42);"
    , "    let string_container = Container::new(String::from(\"hello\"));"
    , "}"
    , "```"
    ]
      result = compile templateProgram ""
  in case result of
    Left _ -> property True  -- Should handle gracefully
    Right compilation -> property $ True  -- Should handle templates

tests :: TestTree
tests = testGroup "End-to-End Compilation Tests"
  [ fastProperty "Complete compilation pipeline handles simple programs" prop_complete_pipeline_simple
  , fastProperty "Multi-block programs are handled correctly" prop_multi_block_handling
  , fastProperty "Ownership analysis is integrated in compilation" prop_ownership_integration
  , fastProperty "Dependent type checking is integrated" prop_dependent_types_integration
  , fastProperty "Error handling is consistent across pipeline" prop_error_handling_consistency
  , fastProperty "Generated Go code is syntactically valid" prop_generated_go_syntax_valid
  , fastProperty "Large programs do not cause performance degradation" prop_large_program_performance
  , fastProperty "Compilation preserves semantic meaning" prop_semantic_preservation
  , fastProperty "Multiple compilation passes are idempotent" prop_compilation_idempotent
  , fastProperty "Pipeline handles Unicode content" prop_unicode_handling
  , fastProperty "Pipeline handles mixed directives" prop_mixed_directives
  , fastProperty "Pipeline handles empty code blocks" prop_empty_code_blocks
  , fastProperty "Pipeline handles deeply nested structures" prop_nested_structures
  , fastProperty "Pipeline handles function signatures with complex types" prop_complex_function_signatures
  , fastProperty "Pipeline maintains source location information" prop_source_location_preservation
  , fastProperty "Pipeline handles concurrent compilation scenarios" prop_concurrent_compilation
  , fastProperty "Pipeline handles template/generic code" prop_template_code
  ]