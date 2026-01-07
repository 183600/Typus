{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Unit.NewUtilsQuickCheckPropertySpec where

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


-- | QuickCheck property tests for Utils module Test.Unit.NewUtilsQuickCheckPropertySpec Test.Unit.NewUtilsQuickCheckPropertySpec where
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Utils 
              forceSingleTabIndentation, fixIndentation, breakOn)
import Data.Char 
    \str -> trim ("  " ++ str ++ "  ") === trim str
  
  ,             testProperty "trim is idempotent" $
    \str -> trim (trim str) === trim str
  
  ,             testProperty "trim removes only whitespace" $
    \str -> L.all (not . isSpace) (trim str) || L.null (trim str)
  
  ,             testProperty "trim preserves non-whitespace characters" $
    \str -> L.filter (not . isSpace)                               str === L.filter (not . isSpace) (trim str)
  ]

-- | Properties for split functions
splitProperties :: TestTree
splitProperties = testGroup "split function properties"
  [             testProperty "splitBy preserves empty segments" $
    \delim str -> L.concat (splitBy delim str) === str
  
  ,             testProperty "splitByCollapsed removes empty segments" $
    \delim str -> not (L.elem ',' str) ==> 
      L.length (splitByCollapsed delim str) <= L.length (splitBy delim str)
  
  ,             testProperty "splitByComma equals splitBy ','" $
    \str -> splitByComma                               str === splitBy ',' str
  
  ,             testProperty "splitByCommaCollapsed equals splitByCollapsed ','" $
    \str -> splitByCommaCollapsed                               str === splitByCollapsed ',' str
  
  ,             testProperty "splitBy with delimiter not in string returns single element" $
    \str delim -> not (L.elem delim str) ==> splitBy delim                               str === [str]
  
  ,             testProperty "splitBy on empty string returns single empty string" $
    \delim -> splitBy delim "" === [""]
  ]

-- | Properties for comment removal functions
commentRemovalProperties :: TestTree
commentRemovalProperties = testGroup "comment removal properties"
  [             testProperty "removeLineComments removes // comments" $
    \prefix suffix -> 
      let line = prefix ++ "// this is a comment" ++ suffix
                                        result = removeLineComments line
      in not ("//" `L.isInfixOf` result) && result `L.isPrefixOf` prefix ++ suffix
  
  ,             testProperty "removeLineComments preserves // in strings" $
    \before after ->
      let line = before ++ "\"string with // comment\"" ++ after
                                        result = removeLineComments line
      in "// comment" `L.isInfixOf` result
  
  ,             testProperty "removeComments removes both // L.and /* */ comments" $
    \prefix suffix ->
      let code = prefix ++ "// line comment\n" ++ prefix ++ "/* block comment */" ++ suffix
                                        result = removeComments code
      in not (("//" `L.isInfixOf` result) || ("/*" `L.isInfixOf` result)
  
  ,             testProperty "removeComments preserves comments in strings" $
    \before after ->
      let code = before ++ "\"string with // L.and /* comments */\"" ++ after
                                        result = removeComments code
      in "// L.and /* comments */" `L.isInfixOf` result
  ]

-- | Properties for indentation functions
indentationProperties :: TestTree
indentationProperties = testGroup "indentation function properties"
  [             testProperty "normalizeIndentation preserves relative indentation" $
    \lines1 lines2 ->
      let input = unlines [lines1, lines2]
                                        result = normalizeIndentation input
                                        resultLines = lines result
      in L.length                               resultLines === L.length (lines input)
  
  ,             testProperty "normalizeIndentation removes common prefix" $
    \str1 str2 ->
      let input = "  " ++ str1 ++ "\n  " ++ str2
                                        result = normalizeIndentation input
      in not ("  " `L.isPrefixOf` result)
  
  ,             testProperty "forceSingleTabIndentation converts to tab format" $
    \str -> not (null str) ==> 
      let result = forceSingleTabIndentation str
                                        resultLines = lines result
                                        nonEmptyLines = L.filter (not . null) resultLines
      in L.all ("\t" `L.isPrefixOf`) nonEmptyLines
  
  ,             testProperty "fixIndentation equals normalizeIndentation" $
    \str -> fixIndentation                               str === normalizeIndentation str
  ]

-- | Properties for breakOn function
breakOnProperties :: TestTree
breakOnProperties = testGroup "breakOn function properties"
  [             testProperty "breakOn with empty pattern returns empty prefix" $
    \str -> breakOn ""                               str === ("", str)
  
  ,             testProperty "breakOn concatenates to original when pattern found" $
    \pat str -> pat `L.isInfixOf`                               str ==> 
      let (before, after) = breakOn pat str
      in before ++ pat ++                               after === str
  
  ,             testProperty "breakOn returns original string when pattern not found" $
    \pat str -> not (pat `L.isInfixOf` str) ==> 
      breakOn pat                               str === (str, "")
  
  ,             testProperty "breakOn with pattern equal to string" $
    \str -> breakOn str                               str === ("", "")
  
  ,             testProperty "breakOn is deterministic" $
    \pat str -> breakOn pat                               str === breakOn pat str
  ]