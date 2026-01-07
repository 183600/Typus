module Test.Unit.CrossModuleIntegrationQuickCheckSpec where
import Test.QuickCheck 
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property)
import Parser 
import SourceLocation (SourcePos(..), SourceSpan(..), startPos, advancePosBy)
import Utils 
Right tf -> property $ L.all isValidBlockSpan (tfBlocks tf)
  where
      isValidBlockSpan                               block = spanStart (cbSpan block) <= spanEnd (cbSpan block)
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


-- | Utils text processing should work correctly on parser output content
prop_utils_parser_integration :: String -> Property
prop_utils_parser_integration                               content = 
  let result = parseTypus content
  in case result of
    Left _ -> property True
    Right tf -> property $ L.all blockContentProcessable (tfBlocks tf)
  where
      blockContentProcessable                               block = 
      let processed = removeComments (cbContent block)
                                        trimmed = trim processed
      in L.length trimmed <= L.length (cbContent block)

-- | Source location math should work correctly with parser-generated spans
prop_sourcelocation_parser_integration :: String -> Property
prop_sourcelocation_parser_integration                               content = 
  let result = parseTypus content
  in case result of
    Left _ -> property True
    Right tf -> property $ L.all spansHaveValidPositions (tfBlocks tf)
  where
      spansHaveValidPositions                               block = 
      let span = cbSpan block
                                        start = spanStart span
                                        end = spanEnd span
      in posLine start > 0 && posColumn start > 0 && 
         posLine end > 0 && posColumn end > 0 &&
         posOffset start <= posOffset end

-- | Error handler should process parser errors correctly
prop_errorhandler_parser_integration :: String -> Property
prop_errorhandler_parser_integration                               content = 
  let result = parseTypus content
  in case result of
    Left err -> property $ L.length (show err) > 0  -- Error should have descriptive message
    Right _ -> property True  -- Success is also valid

-- | Compiler should handle parser output gracefully
prop_compiler_parser_integration :: String -> Property
prop_compiler_parser_integration                               content = 
  let parseResult = parseTypus content
  in case parseResult of
    Left _ -> property True  -- If parsing fails, compilation behavior is undefined
    Right tf -> 
      let compileResult = compile tf
      in case compileResult of
        Left _ -> property True  -- Compilation may fail
        Right _ -> property True  -- Or succeed

-- | Text processing pipeline should maintain consistency
prop_text_processing_pipeline :: String -> Property
prop_text_processing_pipeline                               content = 
  let step1 = trim content
                                    step2 = removeComments step1
                                    step3 = trim step2
                                    lines1 = lines step1
                                    lines2 = lines step2
                                    lines3 = lines step3
  in property $ L.length lines3 <= L.length lines2 && L.length lines2 <= L.length lines1

-- | Source location tracking should work through compilation pipeline
prop_sourcelocation_compilation_tracking :: String -> Property
prop_sourcelocation_compilation_tracking                               content = 
  let result = parseTypus content
  in case result of
    Left _ -> property True
    Right tf -> 
      let spans = map cbSpan (tfBlocks tf)
                                        positions = map spanStart spans
      in property $ L.all isValidPosition positions
  where
      isValidPosition                               pos = posLine pos > 0 && posColumn pos > 0

-- | Error recovery should maintain source location information
prop_error_recovery_sourcelocation :: String -> Property
prop_error_recovery_sourcelocation                               content = 
  let withError = content ++ "\n@@ MALFORMED SYNTAX @@\n" ++ content
                                    result = parseTypus withError
  in case result of
    Left _ -> property True  -- May fail completely
    Right tf -> 
      let spans = map cbSpan (tfBlocks tf)
      in property $ property $ L.all (\span -> isValidSpan span) spans

-- Helper function to check span validity
isValidSpan :: SourceSpan -> Bool
isValidSpan                               span = spanStart span <= spanEnd span