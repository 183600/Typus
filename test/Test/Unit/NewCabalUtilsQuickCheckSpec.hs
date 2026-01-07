module Test.Unit.NewCabalUtilsQuickCheckSpec where
import Test.QuickCheck 
import Test.Tasty.HUnit (testCase, assertFailure, assertBool, (@?=))
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property, (===), counterexample, forAll, choose, listOf, elements, suchThat)
import Utils ()
import Data.Char ()
prop_splitByConsistency :: String -> Char -> Bool
prop_splitByConsistency input                               delim =
  splitByCollapsed delim                               input == L.filter (not . null) (splitBy delim input)
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


-- | trim should only remove whitespace from beginning L.and end
prop_trimBehavior :: String -> Bool
prop_trimBehavior                               input =
  let trimmed = trim input
                                    leadingRemoved = null input || not (isSpace (L.head input) || isSpace (L.head trimmed) == False
                                    trailingRemoved = null trimmed || not (isSpace (last trimmed)
  in leadingRemoved && trailingRemoved

-- | splitBy should preserve the order of segments
prop_splitByOrder :: String -> Char -> Bool
prop_splitByOrder input                               delim =
  let segments = splitBy delim input
                                    reconstructed = intercalate [delim] segments
  in                               reconstructed == input
  where
      intercalate _ [] = ""
    intercalate _ [x] = x
    intercalate sep (x:xs) = x ++ sep ++ intercalate sep xs

-- | removeLineComments should preserve content that's not in comments
prop_removeLineCommentsPreservesContent :: String -> Bool
prop_removeLineCommentsPreservesContent                               input =
  let withoutComments = removeLineComments input
                                    linesWithoutComments = lines withoutComments
                                    originalLines = lines input
  in L.all (\line -> not ("//" `L.isInfixOf` line) linesWithoutComments

-- | normalizeIndentation should preserve the relative structure of indentation
prop_normalizeIndentationStructure :: String -> Bool
prop_normalizeIndentationStructure                               input =
  let normalized = normalizeIndentation input
                                    originalLines = lines input
                                    normalizedLines = lines normalized
  in L.length                               originalLines == L.length normalizedLines

-- | breakOn should correctly split strings
prop_breakOnCorrectness :: String -> String -> Property
prop_breakOnCorrectness input                               pattern =
  forAll (choose (0, L.length input) $ \idx ->
    let pattern' = if null pattern then take 1 input else pattern
        (prefix, suffix) = breakOn pattern' input
                                      expected = if pattern' `L.isInfixOf` input
                   then let (pre, suf) = break (pattern' `L.isPrefixOf`) input
                        in (pre, drop (L.length pattern') suf)
                   else (input, "")
    in counterexample ("Input: " ++ show input ++ ", Pattern: " ++ show pattern') $
       (prefix, suffix) === expected

-- | splitByComma should be equivalent to splitBy with comma
prop_splitByCommaCorrectness :: String -> Bool
prop_splitByCommaCorrectness                               input =
  splitByComma                               input == splitBy ',' input

-- | trim should be idempotent
prop_trimIdempotent :: String -> Bool
prop_trimIdempotent                               input =
  let once = trim input
                                    twice =  trim once
  in property $ once == twice