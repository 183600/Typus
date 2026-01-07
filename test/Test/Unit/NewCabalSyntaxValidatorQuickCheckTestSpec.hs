module Test.Unit.NewCabalSyntaxValidatorQuickCheckTestSpec where
import Test.QuickCheck 
import Test.Tasty.HUnit (testCase, assertFailure, assertBool, (@?=))
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property, (===), counterexample, forAll, oneof, elements, listOf, suchThat)
import qualified Data.List as L
import Data.List 
                        let code = "package main\n\nfunc main() {\n}\n"
                                              errors = validateSyntax code
            null errors @?= True
          ,             testCase "validateSyntax detects missing brace" $ do
                        let code = "package main\n\nfunc main() {\n"
                                              errors = validateSyntax code
            not (null errors) @?= True
          ,             testCase "SyntaxError construction works" $ do
                        let error = SyntaxError "test error" 10 5
                SyntaxError message line                               col = error
            message @?= "test error"
            line @?= 10
            col @?= 5
          ,             testCase "SyntaxError Show instance contains error info" $ do
                        let error = SyntaxError "test error" 10 5
                                              showOutput = show error
            "test error" `L.isInfixOf` showOutput @?= True
            "10" `L.isInfixOf` showOutput @?= True
            "5" `L.isInfixOf` showOutput @?= True
        ]
    ]
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


-- | Property: validateSyntax handles empty input
prop_validateSyntaxEmpty :: Property
                              prop_validateSyntaxEmpty = 
  let errors = validateSyntax ""
  in null errors

-- | Property: validateSyntax handles valid Go syntax
prop_validateSyntaxValid :: String -> Property
prop_validateSyntaxValid                               code = 
  let validCode = "package main\n\nfunc main() {\n}\n"
                                    errors = validateSyntax validCode
  in null errors

-- | Property: validateSyntax detects syntax errors
prop_validateSyntaxErrors :: String -> Property
prop_validateSyntaxErrors                               code = 
  let invalidCode = "package main\n\nfunc main() {\n"  -- Missing closing brace
                                    errors = validateSyntax invalidCode
  in not (null errors)

-- | Property: SyntaxError equality works correctly
prop_syntaxErrorEquality :: String -> Int -> Int -> Property
prop_syntaxErrorEquality message1 line1 col1 message2 line2                               col2 = 
  let error1 = SyntaxError message1 line1 col1
                                    error2 = SyntaxError message2 line2 col2
  in (error1 == error2) === (message1 == message2 &&                               line1 == line2 &&                               col1 == col2)

-- | Property: SyntaxError ordering is consistent
prop_syntaxErrorOrdering :: String -> Int -> Int -> Property
prop_syntaxErrorOrdering message line                               col = 
  let error1 = SyntaxError message line col
                                    error2 = SyntaxError message line col
                                    comparison = compare error1 error2
  in                               comparison === EQ

-- | Property: SyntaxError Show instance contains error message
prop_syntaxErrorShowContainsMessage :: String -> Int -> Int -> Property
prop_syntaxErrorShowContainsMessage message line                               col = 
  let error = SyntaxError message line col
                                    showOutput = show error
  in message `L.isInfixOf` showOutput

-- | Property: validateSyntax returns same errors for same input
prop_validateSyntaxDeterministic :: String -> Property
prop_validateSyntaxDeterministic                               code = 
  let errors1 = validateSyntax code
                                    errors2 = validateSyntax code
  in L.length                               errors1 == L.length errors2

-- | Property: validateSyntax line numbers are positive
prop_validateSyntaxLineNumbersPositive :: String -> Property
prop_validateSyntaxLineNumbersPositive                               code = 
  let errors = validateSyntax code
                                    lineNumbers = [line | SyntaxError _ line _ <- errors]
  in L.all (> 0) lineNumbers

-- | Property: validateSyntax column numbers are positive
prop_validateSyntaxColumnNumbersPositive :: String -> Property
prop_validateSyntaxColumnNumbersPositive                               code = 
  let errors = validateSyntax code
                                    columnNumbers =  [col | SyntaxError _ _ col <- errors]
  in property $ L.all (> 0) columnNumbers

-- Helper operator for composing properties
(.&&.) :: Property -> Property -> Property
(.&&.) = (&&)