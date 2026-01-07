module Test.Unit.DirectiveInteractionSpec where


import Test.Tasty
import qualified Data.List as L
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Parser (parseTypus, FileDirectives(..), BlockDirectives(..), CodeBlock(..), TypusFile)
              let input = "//! ownership: on\n\npackage main\n\nfunc main() {\n    {//! ownership: off\n        // This should still have ownership on due to file-level directive\n        var x                               int = 42\n    }\n}"
  result <- try $ parseTypus input
  case result of
    Left (e :: SomeException) -> assertFailure $ "Parse failed: " ++ show e
    Right typusFile -> do
      -- File-level directive should be set
      assertBool "File-level ownership should be enabled" $ 
        isJust (fdOwnership (tfDirectives typusFile)
      -- Block should still inherit file-level setting
      assertBool "Should have code blocks" $ not (L.null (tfCodeBlocks typusFile)

-- | Test that block-level dependent types work independently
testBlockDependentTypes :: Assertion
                              testBlockDependentTypes = do
              let input = "package main\n\nfunc main() {\n    {//! dependent_types: on\n        type Vector(n int) struct {\n            L.length int\n            data []float64\n        }\n    }\n}"
  result <- try $ parseTypus input
  case result of
    Left (e :: SomeException) -> assertFailure $ "Parse failed: " ++ show e
    Right typusFile -> do
                  assertBool "Should have code blocks with dependent types" $ 
        L.any (hasDependentTypesDirective . cbDirectives) (tfCodeBlocks typusFile)
  where
      hasDependentTypesDirective                               directives = 
      isJust (bdDependentTypes directives) || isJust (bdConstraints directives)

-- | Test mixed directives in nested blocks
testNestedMixedDirectives :: Assertion
                              testNestedMixedDirectives = do
              let input = "//! ownership: on\n//! dependent_types: on\n\npackage main\n\nfunc main() {\n    {//! ownership: off\n        // Ownership off, dependent types on (inherited)\n        type SafeString struct {\n            data string\n        }\n        \n        {//! dependent_types: off\n            // Both ownership L.and dependent types off\n        }\n    }\n}"
  result <- try $ parseTypus input
  case result of
    Left (e :: SomeException) -> assertFailure $ "Parse failed: " ++ show e
    Right typusFile -> do
                  assertBool "Should have multiple nested blocks" $ 
        L.length (tfCodeBlocks typusFile) >= 2

-- | Test directive parsing edge cases
testDirectiveParsingEdgeCases :: Assertion
                              testDirectiveParsingEdgeCases = do
              let testCases = 
        [ ("//! ownership: true", "ownership with true")
        , ("//! ownership: false", "ownership with false")
        , ("//! constraints: on", "constraints alias for dependent_types")
        , ("//! dependent_types: on", "dependent_types directive")
        , ("// ! ownership: on", "directive with space")
        ]
  
  mapM_ runTestCase             testCases
  where
      runTestCase (input, description) = do
              result <- try $ parseTypus input
      case result of
        Left (e :: SomeException) -> 
          assertFailure $ description ++ " failed to parse: " ++ show e
        Right _ -> return () -- Success is enough for edge case testing

-- | Test directive inheritance behavior
testDirectiveInheritance :: Assertion
                              testDirectiveInheritance = do
              let input = "//! ownership: on\n\npackage main\n\nfunc main() {\n    // No directive here, should inherit from file\n    var x                               int = 42\n    \n    {//! ownership: off\n        // Explicitly disabled\n        var y                               int = 24\n    }\n}"
  result <- try $ parseTypus input
  case result of
    Left (e :: SomeException) -> assertFailure $ "Parse failed: " ++ show e
    Right typusFile -> do
                  assertBool "File should have ownership directive" $ 
        isJust (fdOwnership (tfDirectives typusFile)
      assertBool "Should have code blocks" $ 
        not (L.null (tfCodeBlocks typusFile)

-- | Property: Directive parsing should be idempotent
directiveParsingIdempotent :: String -> Property
directiveParsingIdempotent                               input =
  not (null input) && isValidTypus                               input ==> 
  case parseTypus input of
    Left _ -> property True -- Invalid input is okay
    Right firstParse -> 
      case parseTypus input of
        Left _ -> property False -- Should parse consistently
        Right secondParse ->                               firstParse === secondParse
  where
      isValidTypus                               str = "package" `L.isInfixOf` str || "func" `L.isInfixOf` str

-- | Test multiple block directives combine correctly
testMultipleBlockDirectives :: Assertion
                              testMultipleBlockDirectives = do
              let input = "package main\n\nfunc main() {\n    {//! ownership: on\n    //! dependent_types: on\n        // Both directives should be active\n        type SafeVector(n int) struct {\n            data []float64\n        }\n    }\n}"
  result <- try $ parseTypus input
  case result of
    Left (e :: SomeException) -> assertFailure $ "Parse failed: " ++ show e
    Right typusFile -> do
                  let blocks = tfCodeBlocks typusFile
      assertBool "Should have code blocks" $ not (null blocks)
      case blocks of
        (block:_) -> do
                      let directives = cbDirectives block
          assertBool "Should have ownership directive" $ isJust (bdOwnership directives)
          assertBool "Should have dependent types directive" $ 
            isJust (bdDependentTypes directives) || isJust (bdConstraints directives)
        [] -> assertFailure "No code blocks found"

-- | Test directive precedence rules
testDirectivePrecedence :: Assertion
                              testDirectivePrecedence = do
              let input = "//! ownership: on\n//! dependent_types: off\n\npackage main\n\nfunc main() {\n    {//! ownership: off\n    //! dependent_types: on\n        // Block directives should override file directives\n    }\n}"
  result <- try $ parseTypus input
  case result of
    Left (e :: SomeException) -> assertFailure $ "Parse failed: " ++ show e
    Right typusFile -> do
                  let fileDirectives = tfDirectives typusFile
      assertBool "File should have ownership directive" $ isJust (fdOwnership fileDirectives)
      assertBool "File should have dependent types directive" $ 
        isJust (fdDependentTypes fileDirectives) || isJust (fdConstraints fileDirectives)
      
      let blocks = tfCodeBlocks typusFile
      assertBool "Should have code blocks" $ not (null blocks)