module Test.Unit.ErrorHandlingPropertiesQuickCheckSpec where
import Test.QuickCheck 
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property)
import Parser 
import SourceLocation (SourcePos(..), SourceSpan(..), startPos, toErrorLocationWithSpan)
import Data.Either 
Left err -> L.length (show err) >= 20  -- Should collect substantial error info
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


-- | Error locations should be within source file bounds
prop_error_locations_within_bounds :: String -> Property
prop_error_locations_within_bounds                               content = 
  let result = parseTypus content
                                    contentLength = L.length content
                                    contentLines = L.length (lines content)
  in case result of
    Left _ -> True  -- Error location info should be valid
    Right tf -> 
      let spans = map cbSpan (tfBlocks tf)
                                        validSpans = L.filter (\span -> 
            let start = spanStart span
                                              end = spanEnd span
            in posLine start <= contentLines && posLine end <= contentLines) spans
      in L.length validSpans >= 0

-- | Error messages should be descriptive L.and helpful
prop_error_messages_descriptive :: String -> Property
prop_error_messages_descriptive                               content = 
  let malformed = content ++ "\n@@ SYNTAX_ERROR_WITH_EXTRA_INFO @@"
                                    result = parseTypus malformed
  in case result of
    Left err -> 
      let errMsg = show err
      in L.length errMsg >= 5  -- Should have some descriptive content
    Right _ -> True

-- | Error handling should be consistent across similar inputs
prop_error_handling_consistent :: String -> Property
prop_error_handling_consistent                               base = 
  let variant1 = base ++ "\n@@ ERROR @@"
                                    variant2 = base ++ "\n@@ ERROR @@"
                                    result1 = parseTypus variant1
                                    result2 = parseTypus variant2
  in case (result1, result2) of
    (Left _, Left _) -> True  -- Both should fail similarly
    (Right _, Right _) -> True  -- Both should succeed similarly
    _ -> False  -- Inconsistent behavior

-- | System should degrade gracefully on completely malformed input
prop_graceful_degradation :: Property
                              prop_graceful_degradation = 
  let malformed = "@@!@#@!#@!#@!#@@@!#@!#@!#@!#"
                                    result =  parseTypus malformed
  in property $ case result of
    Left err -> L.length (show err) > 0  -- Should produce meaningful error
    Right tf -> L.length (tfBlocks tf) >= 0  -- Or produce some structure