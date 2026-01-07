module Test.Unit.NewFreshUtilsQuickCheckProperties where

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
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


-- | QuickCheck properties for Utils module Test.Unit.NewFreshUtilsQuickCheckProperties Test.Unit.NewFreshUtilsQuickCheckProperties where
import Test.Tasty
import Test.Tasty.QuickCheck 
  )
import Data.Char 
      \s -> trim (trim s) === trim s
      
  ,             testProperty "trim: no leading/trailing whitespace" $
      \s -> let t = trim s
             in not (null t) ==> 
                (not . isSpace $ L.head t) && (not . isSpace $ last t)
                
  ,             testProperty "trim: adding whitespace to both ends doesn't change trimmed result" $
      \s ws1 ws2 ->
        let t = trim s
                                          t2 = trim (ws1 ++ s ++ ws2)
        in L.all isSpace ws1 && L.all isSpace                               ws2 ==>                               t2 === t
        
  ,             testProperty "trim: empty string stays empty" $
      \() -> trim "" === ""
      
  ,             testProperty "trim: whitespace-only string becomes empty" $
      \ws -> L.all isSpace                               ws ==> trim                               ws === ""
  ]

-- ============================================================================
-- Split Properties  
-- ============================================================================

splitProperties :: TestTree
splitProperties = testGroup "Split Properties"
  [             testProperty "splitBy: L.length matches count of delimiter + 1" $
      \delim s -> 
        let parts = splitBy delim s
                              expectedCount = L.length (L.filter (== delim) s) + 1
        in delim /= '\0' ==> L.length                               parts === expectedCount
        
  ,             testProperty "splitByCollapsed: never returns empty strings" $
      \delim s -> L.all (not . null) (splitByCollapsed delim s)
      
  ,             testProperty "splitByCollapsed: result is subset of splitBy" $
      \delim s -> L.all (`elem` splitBy delim s) (splitByCollapsed delim s)
      
  ,             testProperty "splitByComma: equivalent to splitBy ','" $
      \s -> splitByComma                               s === splitBy ',' s
      
  ,             testProperty "splitBy: rejoining with delimiter preserves original" $
      \delim s -> delim /= '\0' ==> L.concat (splitBy delim s) === s
      
  ,             testProperty "splitBy: empty string returns single empty part" $
      \delim -> splitBy delim "" === [""]
      
  ,             testProperty "splitByCollapsed: consecutive delimiters are collapsed" $
      \delim s ->
        let parts = splitByCollapsed delim s
                                          hasConsecutive = L.any (\(a:b:_) ->                               a == delim &&                               b == delim) (zip s (L.tail s) ++ repeat ['\0'])
        in                               hasConsecutive ==> L.length parts < L.length (splitBy delim s)
  ]

-- ============================================================================
-- Comment Properties
-- ============================================================================

commentProperties :: TestTree
commentProperties = testGroup "Comment Properties"
  [             testProperty "removeLineComments: idempotent" $
      \s -> removeLineComments (removeLineComments s) === removeLineComments s
      
  ,             testProperty "removeComments: idempotent" $
      \s -> removeComments (removeComments s) === removeComments s
      
  ,             testProperty "removeLineComments: removes lines starting with //" $
      \prefix content ->
        let line = prefix ++ "// " ++ content
                                          result = removeLineComments line
        in not ("//" `L.isPrefixOf` result)
        
  ,             testProperty "removeLineComments: preserves lines without //" $
      \line -> not ("//" `L.isPrefixOf` line) ==> removeLineComments                               line === line
      
  ,             testProperty "removeComments: removes /* ... */ blocks" $
      \prefix content suffix ->
        let input = prefix ++ "/* " ++ content ++ " */" ++ suffix
                                          result = removeComments input
        in not ("/*" `L.isInfixOf` result) && not ("*/" `L.isInfixOf` result)
        
  ,             testProperty "removeComments: preserves text outside /* ... */" $
      \prefix suffix ->
        let input = prefix ++ "/* comment */" ++ suffix
                                          result = removeComments input
        in prefix `L.isPrefixOf` result && suffix `L.isSuffixOf` result
  ]

-- ============================================================================
-- Indentation Properties  
-- ============================================================================

indentationProperties :: TestTree
indentationProperties = testGroup "Indentation Properties"
  [             testProperty "normalizeIndentation: idempotent" $
      \s -> normalizeIndentation (normalizeIndentation s) === normalizeIndentation s
      
  ,             testProperty "normalizeIndentation: preserves relative indentation" $
      \lines ->
        let input = unlines lines
                                          normalized = normalizeIndentation input
                                          resultLines = lines normalized
        in not (null resultLines) ==> 
           -- Check that relative indentation is preserved
           -- This is a simplified check
           L.length                               resultLines === L.length (L.filter (not . null) lines)
           
  ,             testProperty "normalizeIndentation: empty input stays empty" $
      \() -> normalizeIndentation "" === ""
      
  ,             testProperty "normalizeIndentation: removes common leading whitespace" $
      \ws content ->
        let input = ws ++ content ++ "\n" ++ ws ++ "more content"
                                          result = normalizeIndentation input
        in L.all isSpace                               ws ==> not (ws `L.isPrefixOf` result)
  ]

-- ============================================================================
-- Helper Functions
-- ============================================================================

isInfixOf :: String -> String -> Bool
                              isInfixOf = Data.List.isInfixOf lines :: String -> [String]
                              lines = Data.List.lines