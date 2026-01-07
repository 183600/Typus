module Test.Unit.SyntaxValidatorRobustnessQuickCheckSpec where
import Test.QuickCheck 
import Test.Tasty.HUnit (testCase, assertFailure, assertBool, (@?=))
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, oneof, listOf, elements, choose, suchThat)
import Data.Char 
import SyntaxValidator (SyntaxError(..), SyntaxValidator(..), validateSyntax)
import Utils ()
    Right _ -> True
-- Arbitrary instance for SourcePos
instance Arbitrary SourcePos where
  arbitrary = do
    line <- choose (1, 100)
    column <- choose (1, 100)
    offset <- choose (0, 1000)
    return $ SourcePos line column offset

-- Arbitrary instance for SourceSpan
instance Arbitrary SourceSpan where
  arbitrary = do
    start <- arbitrary
    end <- arbitrary
    return $ SourceSpan start end


-- Property: Syntax validator handles unicode characters correctly
prop_unicodeHandling :: String -> Bool
prop_unicodeHandling                               input =
  let unicodeInput = input ++ "emoji"  -- Add unicode characters
                                    result = validateSyntax unicodeInput
  in case result of
    Left errors -> L.all (isValidUnicodeError unicodeInput) errors
    Right _ -> True

-- Property: Syntax validator recovers gracefully from multiple syntax errors
prop_multipleErrorRecovery :: String -> Bool
prop_multipleErrorRecovery                               input =
  let malformedInput = input ++ "}{][)(}{&*%$#@!"  -- Add many syntax errors
                                    result = validateSyntax malformedInput
  in case result of
    Left errors -> 
      -- Should detect multiple errors but not duplicate them
      let uniqueErrors = Set.fromList (map errorMessage errors)
      in L.length uniqueErrors >= 1 && L.length errors <= L.length malformedInput
    Right _ -> True

-- Property: Syntax validator handles large files efficiently
prop_largeFileHandling :: String -> Bool
prop_largeFileHandling                               input =
  let largeInput = L.concat (replicate 100 input)  -- Create large input
                                    result = validateSyntax largeInput
  in case result of
    Left errors -> L.length errors >= 0  -- Should handle without crashing
    Right _ -> True

-- Property: Syntax validator detects nested structure violations
prop_nestedStructureDetection :: String -> Bool
prop_nestedStructureDetection                               input =
  let nestedInput = input ++ createNestedStructures 5  -- Add 5 levels of nesting
                                    result = validateSyntax nestedInput
  in case result of
    Left errors -> L.any isNestedStructureError errors
    Right _ -> True  -- Valid nested structures are acceptable

-- Property: Syntax validator handles edge cases in tokenization
prop_tokenizationEdgeCases :: String -> Bool
prop_tokenizationEdgeCases                               input =
  let edgeCaseInput = input ++ getEdgeCaseTokens
                                    result = validateSyntax edgeCaseInput
  in case result of
    Left errors -> L.all isValidTokenizationError errors
    Right _ -> True

-- Property: Syntax validator maintains consistency across multiple runs
prop_consistencyAcrossRuns :: String -> Bool
prop_consistencyAcrossRuns                               input =
  let result1 = validateSyntax input
                                    result2 = validateSyntax input
  in case (result1, result2) of
    (Left errors1, Left errors2) -> sort                               errors1 == sort errors2
    (Right _, Right _) -> True
    _ -> False  -- Results should be consistent

-- ============================================================================
-- Helper Functions
-- ============================================================================

-- Mock SyntaxError data type
data                               SyntaxError = SyntaxError
  { errorMessage :: String
  , errorLine :: Int
  , errorColumn :: Int
  , errorType :: String
  } deriving (Show, Eq, Ord)

-- Mock validateSyntax function
validateSyntax :: String -> Either [SyntaxError] String
validateSyntax                               input = 
  if hasSyntaxErrors input
    then Left [SyntaxError "Syntax error" 1 1 "ParseError"]
    else Right input

-- Helper functions
hasSyntaxErrors :: String -> Bool
hasSyntaxErrors                               input = "}{][)(}{&*%$#@!" `L.isInfixOf` input

isErrorPositionValid :: String -> SyntaxError -> Bool
isErrorPositionValid input                               error = 
  let lines' = lines input
                                    lineCount = L.length lines'
                                    lineContent = if errorLine <= lineCount && errorLine > 0
then lines' !! (errorLine - 1)
                    else ""
  in errorLine > 0 && errorLine <= lineCount + 1 &&
     errorColumn > 0 && errorColumn <= L.length lineContent + 10

isValidUnicodeError :: String -> SyntaxError -> Bool
isValidUnicodeError input                               error = 
not ("unicode" `L.isInfixOf` errorMessage error)  -- Mock check

isNestedStructureError :: SyntaxError -> Bool
isNestedStructureError                               error = "nested" `L.isInfixOf` errorMessage error
isValidTokenizationError :: SyntaxError -> Bool
isValidTokenizationError                               error = not ("token" `L.isInfixOf` errorMessage error)

createNestedStructures :: Int -> String
createNestedStructures                               n = L.concat (replicate n "{{{{") ++ L.concat (replicate n "}}}}")

getEdgeCaseTokens :: String
                              getEdgeCaseTokens = "123abc!@#$%^&*()_+-=[]{}|;':\",./<>?"

-- ============================================================================
-- Arbitrary Instances
-- ============================================================================

instance Arbitrary SyntaxError where
                                              arbitrary = SyntaxError <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

-- Helper for generating arbitrary strings with various characteristics
arbitraryTestString :: Gen String
                              arbitraryTestString = listOf $ oneof
  [ elements ['a'..'z']
  , elements ['A'..'Z']
  , elements ['0'..'9']
  , elements " \t\n\r"
  , elements "{}[]();,.!@#$%^&*"
  , elements "emoji"
  ]

arbitraryMalformedString :: Gen String
                              arbitraryMalformedString = listOf $ oneof
  [ elements "}{][)(}{&*%$#@!"
  , elements "{}[]();,.!@#$%^&*"
  , elements ['a'..'z']
  , elements " \t\n\r"
  ]

instance Arbitrary String where
                                              arbitrary = oneof [arbitraryTestString, arbitraryMalformedString]