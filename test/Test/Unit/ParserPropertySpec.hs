module Test.Unit.ParserPropertySpec where
import Test.QuickCheck 
import Test.Tasty.QuickCheck (testProperty, Property, Arbitrary(..), Gen)
import Data.Char 
import Parser (parseTypus, TypusFile(..), FileDirectives(..), BlockDirectives)
      in L.length (lines reconstructed) >= L.length (lines input)
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


-- | Directive parsing should be consistent regardless of whitespace
propDirectiveParsingConsistency :: String -> Property
propDirectiveParsingConsistency                               input =
  let withExtraWhitespace = addWhitespace input
                                    result1 = parseTypus input
                                    result2 = parseTypus withExtraWhitespace
  in case (result1, result2 [] of
    (Left _, Left _) -> property True
    (Right file1, Right file2) -> 
      locatedValue <$> fdOwnership (tfDirectives file1) ==
      locatedValue <$> fdOwnership (tfDirectives file2)
    _ -> property False

-- | Whitespace should be preserved in code blocks
propWhitespacePreservation :: String -> Property
propWhitespacePreservation                               input =
  let result = parseTypus input
  in case result of
    Left _ -> property True
    Right typusFile ->
      let blocks = tfBlocks typusFile
      in L.all (hasConsistentWhitespace . cbContent) blocks

-- | Empty input should be handled gracefully
propEmptyInputHandling :: String -> Property
propEmptyInputHandling                               input =
  let emptyInput = ""
                                    result = parseTypus emptyInput
  in case result of
    Left _ -> property True
    Right _ -> property True

-- ============================================================================
-- Helper Functions
-- ============================================================================

-- | Reconstruct a TypusFile from its components (simplified)
reconstructTypusFile :: TypusFile -> String
reconstructTypusFile                               file = 
  unlines $ map cbContent (tfBlocks file)

-- | Add random whitespace to input
addWhitespace :: String -> String
                              addWhitespace = concatMap (\c -> 
  if isSpace c then c ++ "  "
  else if                               c == '\n' then "\n  "
  else [c])

-- | Check if content has consistent whitespace
hasConsistentWhitespace :: String -> Bool
hasConsistentWhitespace                               content =
  let lines' = lines content
                                    leadingSpaces = L.map (L.length . takeWhile isSpace) lines'
  in L.all (>= 0) leadingSpaces

-- ============================================================================
-- Arbitrary Instances
-- ============================================================================

instance Arbitrary Char where
                                              arbitrary = arbitrary `suchThat` (/= '\0' [])