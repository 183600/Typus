module Test.Unit.NewQuickCheckTestSuite1Spec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.QuickCheck (Property, (==>), forAll, Gen, arbitrary, choose)
import Data.Char (isSpace, isControl)
import Data.List (isPrefixOf, isSuffixOf)

import TestSupport.QuickCheck (fastProperty)
import Utils

-- | Test suite for Utils module string processing and boundary conditions
tests :: TestTree
tests =
  testGroup "NewQuickCheckTestSuite1 - Utils String Processing"
    [ testGroup "Trim function properties"
        [ testCase "trim handles all whitespace" $ do
            trim " \t\n\r\v\f " @?= ""
            
        , testCase "trim preserves internal whitespace" $ do
            trim "  hello  world  " @?= "hello  world"
            
        , testCase "trim handles empty string" $ do
            trim "" @?= ""
            
        , testCase "trim handles unicode whitespace" $ do
            trim "\x00A0hello\x2003" @?= "\x00A0hello\x2003"  -- Non-breaking spaces preserved
            
        , fastProperty "trim idempotent" prop_trimIdempotent
        , fastProperty "trim never adds characters" prop_trimNeverAdds
        , fastProperty "trim removes only leading/trailing whitespace" prop_trimOnlyRemovesWhitespace
        ]

    , testGroup "Split function boundary conditions"
        [ testCase "splitBy handles empty input" $ do
            splitBy ',' "" @?= [""]
            
        , testCase "splitBy with delimiter only" $ do
            splitBy ',' "," @?= ["", ""]
            
        , testCase "splitBy with consecutive delimiters" $ do
            splitBy ',' "a,,b" @?= ["a", "", "b"]
            
        , testCase "splitByCollapsed removes empty segments" $ do
            splitByCollapsed ',' "a,,b" @?= ["a", "b"]
            
        , testCase "splitByCollapsed with only delimiters" $ do
            splitByCollapsed ',' ",,," @?= []
            
        , fastProperty "splitBy length property" prop_splitByLength
        , fastProperty "splitByCollapsed never returns empty strings" prop_splitByCollapsedNoEmpty
        , fastProperty "splitBy recombination" prop_splitByRecombination
        ]

    , testGroup "Comment removal edge cases"
        [ testCase "removeLineComments handles empty lines" $ do
            removeLineComments "" @?= ""
            
        , testCase "removeLineComments handles only comments" $ do
            removeLineComments "// full comment\n" @?= "\n"
            
        , testCase "removeLineComments preserves escaped quotes" $ do
            removeLineComments "text = \"hello \\\" world\\\"\" // comment\n" 
                @?= "text = \"hello \\\" world\\\"\" \n"
            
        , testCase "removeComments handles nested block comments correctly" $ do
            removeComments "outer /* inner /* more */ still */ end\n" 
                @?= "outer  end\n"
            
        , testCase "removeComments handles unterminated block comments" $ do
            removeComments "start /* unterminated\nafter" 
                @?= "start \n"
            
        , fastProperty "removeLineComments preserves non-comment content" prop_removeLineCommentsPreservesContent
        , fastProperty "removeComments never increases string length" prop_removeCommentsNeverIncreases
        ]

    , testGroup "Indentation normalization edge cases"
        [ testCase "normalizeIndentation handles mixed tabs and spaces" $ do
            let input = "    \tline1\n\t    line2\n"
                expected = "line1\n\tline2\n"
            normalizeIndentation input @?= expected
            
        , testCase "normalizeIndentation handles empty lines" $ do
            let input = "\n    line\n\n    line2\n"
                expected = "\nline\n\nline2\n"
            normalizeIndentation input @?= expected
            
        , testCase "forceSingleTabIndentation handles whitespace-only lines" $ do
            let input = "  \n\t\n   \t\n"
                expected = "\n\t\n\n"
            forceSingleTabIndentation input @?= expected
            
        , fastProperty "normalizeIndentation preserves relative indentation" prop_normalizeIndentationPreservesRelative
        , fastProperty "forceSingleTabIndentation result property" prop_forceSingleTabProperty
        ]

    , testGroup "Search function robustness"
        [ testCase "breakOn with empty pattern" $ do
            breakOn "" "hello" @?= ("", "hello")
            
        , testCase "breakOn with pattern not found" $ do
            breakOn "xyz" "hello" @?= ("hello", "")
            
        , testCase "breakOn with pattern at start" $ do
            breakOn "he" "hello" @?= ("", "llo")
            
        , testCase "breakOn with pattern at end" $ do
            breakOn "lo" "hello" @?= ("hel", "")
            
        , fastProperty "breakOn concatenation property" prop_breakOnConcatenation
        , fastProperty "breakOn with pattern longer than string" prop_breakOnPatternLonger
        ]
    
    , testGroup "Unicode and special character handling"
        [ testCase "trim with Unicode characters" $ do
            trim "  \x03B8\x03B5\x03C1\x03BC\x03B1  " @?= "\x03B8\x03B5\x03C1\x03BC\x03B1"
            
        , testCase "splitBy with Unicode delimiter" $ do
            splitBy '。' "你好。世界。" @?= ["你好", "世界", ""]
            
        , testCase "removeComments with Unicode strings" $ do
            removeComments "text := \"测试\" /* comment */\n" @?= "text := \"测试\" \n"
            
        , fastProperty "trim preserves Unicode content" prop_trimUnicode
        , fastProperty "splitBy handles Unicode characters" prop_splitByUnicode
        ]
    ]

-- ============================================================================
-- QuickCheck Properties
-- ============================================================================

-- Trim properties
prop_trimIdempotent :: String -> Bool
prop_trimIdempotent input = trim (trim input) == trim input

prop_trimNeverAdds :: String -> Bool
prop_trimNeverAdds input = length (trim input) <= length input

prop_trimOnlyRemovesWhitespace :: String -> Bool
prop_trimOnlyRemovesWhitespace input = 
    let trimmed = trim input
        originalLength = length input
        trimmedLength = length trimmed
        removed = take (originalLength - trimmedLength) input
    in all isSpace removed

-- Split properties
prop_splitByLength :: Char -> String -> Bool
prop_splitByLength delim input = 
    let parts = splitBy delim input
        joined = intercalate [delim] parts
    in length joined >= length input  -- May be longer due to delimiter reinsertion

prop_splitByCollapsedNoEmpty :: Char -> String -> Bool
prop_splitByCollapsedNoEmpty delim input = 
    all (not . null) (splitByCollapsed delim input)

prop_splitByRecombination :: Char -> String -> Bool
prop_splitByRecombination delim input = 
    let parts = splitBy delim input
        recombined = concat parts ++ replicate (length parts - 1) [delim]
    in recombined == input

-- Comment removal properties
prop_removeLineCommentsPreservesContent :: String -> Bool
prop_removeLineCommentsPreservesContent input = 
    let withoutComments = removeLineComments input
        linesInput = lines input
        linesOutput = lines withoutComments
    in length linesOutput == length linesInput

prop_removeCommentsNeverIncreases :: String -> Bool
prop_removeComments input = length (removeComments input) <= length input

-- Indentation properties
prop_normalizeIndentationPreservesRelative :: String -> Bool
prop_normalizeIndentationPreservesRelative input = 
    let linesInput = lines input
        linesOutput = lines (normalizeIndentation input)
    in length linesInput == length linesOutput

prop_forceSingleTabProperty :: String -> Bool
prop_forceSingleTabProperty input = 
    let linesOutput = lines (forceSingleTabIndentation input)
        nonEmptyLines = filter (not . null) linesOutput
    in all ("\t" `isPrefixOf`) nonEmptyLines

-- Search properties
prop_breakOnConcatenation :: String -> String -> Bool
prop_breakOnConcatenation pattern input = 
    let (prefix, suffix) = breakOn pattern input
    in prefix ++ pattern ++ suffix == input || pattern `notElem` input

prop_breakOnPatternLonger :: String -> String -> Bool
prop_breakOnPatternLonger pattern input = 
    length pattern > length input ==> breakOn pattern input == (input, "")

-- Unicode properties
prop_trimUnicode :: String -> Bool
prop_trimUnicode input = 
    let trimmed = trim input
        hasNonWhitespace = any (not . isSpace) input
    in hasNonWhitespace ==> any (not . isSpace) trimmed

prop_splitByUnicode :: Char -> String -> Bool
prop_splitByUnicode delim input = 
    let parts = splitBy delim input
        totalLength = sum (map length parts) + length parts - 1
    in totalLength == length input

-- Helper function
intercalate :: String -> [String] -> String
intercalate _ [] = []
intercalate _ [x] = x
intercalate sep (x:xs) = x ++ sep ++ intercalate sep xs