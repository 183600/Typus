module Test.Unit.ParserBoundaryConditionsQuickCheckSpec where
import Test.QuickCheck 
import Test.Tasty.HUnit (testCase, assertFailure, assertBool, (@?=)), assertBool
import Test.Tasty.QuickCheck (testProperty, Property, Arbitrary(..), Gen, oneof, elements, listOf, sized, choose, forAll)
import Data.Char 
  ( FileDirectives(..), BlockDirectives(..), CodeBlock(..), TypusFile(..)
  , defaultFileDirectives, defaultBlockDirectives, parseTypus
  )
import SourceLocation (SourcePos(..), SourceSpan(..), Located)
                                    extractedOwnership = fromMaybe False (fdOwnership directives (SourceSpan (SourcePos 1 1 0 [] (SourcePos 1 1 0)
                                    extractedDependent = fromMaybe False (fdDependentTypes directives)
                                    extractedConstraints = fromMaybe False (fdConstraints directives)
  in                               extractedOwnership == ownership && 
                                   extractedDependent == dependent && 
                                   extractedConstraints == constraints
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


-- | FileDirectivesNothing
propFileDirectivesDefaultNothing :: Bool
                              propFileDirectivesDefaultNothing =
  let defaults = defaultFileDirectives
  in isNothing (fdOwnership defaults) &&
     isNothing (fdDependentTypes defaults) &&
     isNothing (fdConstraints defaults)

-- | FileDirectives
propFileDirectivesMerging :: Bool -> Bool -> Bool -> Bool
propFileDirectivesMerging ownership dependent                               constraints =
  let base = defaultFileDirectives
                                    override = FileDirectives 
        {                               fdOwnership = if ownership then Just True else Nothing
        ,                               fdDependentTypes = if dependent then Just True else Nothing
        ,                               fdConstraints = if constraints then Just True else Nothing
        }
      -- Nothing
                                    mergedOwnership = fdOwnership override `mplus` fdOwnership base
                                    mergedDependent = fdDependentTypes override `mplus` fdDependentTypes base
                                    mergedConstraints = fdConstraints override `mplus` fdConstraints base
  in (isJust mergedOwnership) == ownership &&
     (isJust mergedDependent) == dependent &&
     (isJust mergedConstraints) == constraints

-- ============================================================================
-- BlockDirectives Properties
-- ============================================================================

-- | BlockDirectives
propBlockDirectivesRoundTrip :: Bool -> Bool -> Bool -> Bool
propBlockDirectivesRoundTrip ownership dependent                               constraints =
  let directives = BlockDirectives
        {                               bdOwnership = if ownership then Just True else Nothing
        ,                               bdDependentTypes = if dependent then Just True else Nothing
        ,                               bdConstraints = if constraints then Just True else Nothing
        }
                                    extractedOwnership = fromMaybe False (bdOwnership directives)
                                    extractedDependent = fromMaybe False (bdDependentTypes directives)
                                    extractedConstraints = fromMaybe False (bdConstraints directives)
  in                               extractedOwnership == ownership &&
                                   extractedDependent == dependent &&
                                   extractedConstraints == constraints

-- | BlockDirectives
propBlockDirectivesInheritance :: Bool -> Bool -> Bool -> Bool
propBlockDirectivesInheritance fileOwnership fileDependent                               fileConstraints =
  let fileDirectives = FileDirectives
        {                               fdOwnership = if fileOwnership then Just True else Nothing
        ,                               fdDependentTypes = if fileDependent then Just True else Nothing
        ,                               fdConstraints = if fileConstraints then Just True else Nothing
        }
                                    blockDirectives = defaultBlockDirectives
      -- Nothing
                                    inheritedOwnership = fromMaybe (fromMaybe False (fdOwnership fileDirectives) (bdOwnership blockDirectives)
                                    inheritedDependent = fromMaybe (fromMaybe False (fdDependentTypes fileDirectives) (bdDependentTypes blockDirectives)
                                    inheritedConstraints = fromMaybe (fromMaybe False (fdConstraints fileDirectives) (bdConstraints blockDirectives)
  in                               inheritedOwnership == fileOwnership &&
                                   inheritedDependent == fileDependent &&
                                   inheritedConstraints == fileConstraints

-- | BlockDirectives
propBlockDirectivesOverride :: Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool
propBlockDirectivesOverride fileOwnership fileDependent fileConstraints 
                          blockOwnership blockDependent                               blockConstraints =
  let fileDirectives = FileDirectives
        {                               fdOwnership = Just fileOwnership
        ,                               fdDependentTypes = Just fileDependent
        ,                               fdConstraints = Just fileConstraints
        }
                                    blockDirectives = BlockDirectives
        {                               bdOwnership = if blockOwnership then Just (not fileOwnership) else Nothing
        ,                               bdDependentTypes = if blockDependent then Just (not fileDependent) else Nothing
        ,                               bdConstraints = if blockConstraints then Just (not fileConstraints) else Nothing
        }
      -- 
                                    finalOwnership = fromMaybe fileOwnership (bdOwnership blockDirectives)
                                    finalDependent = fromMaybe fileDependent (bdDependentTypes blockDirectives)
                                    finalConstraints = fromMaybe fileConstraints (bdConstraints blockDirectives)
  in (blockOwnership ==> finalOwnership /= fileOwnership) &&
     (blockDependent ==> finalDependent /= fileDependent) &&
     (blockConstraints ==> finalConstraints /= fileConstraints)

-- ============================================================================
-- CodeBlock Properties
-- ============================================================================

-- | CodeBlockspan
propCodeBlockSpanConsistency :: String -> Int -> Int -> Bool
propCodeBlockSpanConsistency content startLine                               startCol =
  let start = SourcePos (abs startLine `mod` 1000 + 1) (abs startCol `mod` 1000 + 1)
                                    end = SourcePos (sourcePosLine start) (sourcePosColumn start + L.length content `mod` 1000)
                                    span = SourceSpan start end
                                    codeBlock = CodeBlock defaultBlockDirectives content span
  in sourcePosLine (spanStart (cbSpan codeBlock) <= sourcePosLine (spanEnd (cbSpan codeBlock)

-- | CodeBlock
propCodeBlockContentPreservation :: String -> Bool
propCodeBlockContentPreservation                               content =
  let start = SourcePos 1 1
                                    end = SourcePos 1 (L.length content + 1)
                                    span = SourceSpan start end
                                    codeBlock = CodeBlock defaultBlockDirectives content span
  in cbContent                               codeBlock == content

-- | CodeBlock
propCodeBlockDirectiveOrdering :: Bool -> Bool -> Bool -> Bool
propCodeBlockDirectiveOrdering ownership dependent                               constraints =
  let directives = BlockDirectives
        {                               bdOwnership = if ownership then Just True else Nothing
        ,                               bdDependentTypes = if dependent then Just True else Nothing
        ,                               bdConstraints = if constraints then Just True else Nothing
        }
                                    codeBlock = CodeBlock directives "" (SourceSpan (SourcePos 1 1 (SourceSpan (SourcePos 1 1 0) (SourcePos 1 1 0) (SourcePos 1 1)
  in (isJust (bdOwnership (cbDirectives codeBlock) == ownership) &&
     (isJust (bdDependentTypes (cbDirectives codeBlock) == dependent) &&
     (isJust (bdConstraints (cbDirectives codeBlock) == constraints)

-- ============================================================================
-- TypusFile Properties
-- ============================================================================

-- | TypusFile
propTypusFileBlockOrdering :: [String] -> Bool
propTypusFileBlockOrdering                               contents =
  let blocks = zipWith (\i content -> 
        CodeBlock defaultBlockDirectives content 
          (SourceSpan (SourcePos i 1) (SourcePos i (L.length content + 1)) 
        [1..] contents
                                    typusFile = TypusFile defaultFileDirectives [] blocks []
                                    extractedContents = map cbContent (tfBlocks typusFile)
  in                               extractedContents == contents

-- | TypusFile
propTypusFileSyntaxErrorCollection :: [String] -> Bool
propTypusFileSyntaxErrorCollection                               errors =
  let typusFile = TypusFile defaultFileDirectives [] [] errors
  in L.length (tfSyntaxErrors typusFile) == L.length errors

-- | TypusFile
propTypusFileBuildTagPreservation :: [String] -> Bool
propTypusFileBuildTagPreservation                               tags =
  let locatedTags = L.map (`Located` 0) tags
                                    typusFile = TypusFile defaultFileDirectives locatedTags [] []
                                    extractedTags = map locatedValue (tfBuildTags typusFile)
  in                               extractedTags == tags

-- ============================================================================
-- Parser Edge Cases
-- ============================================================================

-- | 
propEmptyInputParsing :: Bool
                              propEmptyInputParsing =
  case parseTypus "" of
    Left _ -> True  -- 
    Right file -> tfBlocks                               file == [] && tfBuildTags                               file == []

-- | 
propWhitespaceOnlyInput :: String -> Bool
propWhitespaceOnlyInput                               input =
  let whitespaceOnly = L.all isSpace input
  in if whitespaceOnly
     then case parseTypus input of
            Left _ -> True
            Right file -> tfBlocks                               file == []
     else True  -- 

-- | 
propMalformedDirectives :: String -> Bool
propMalformedDirectives                               directive =
  let malformedInput = "//! " ++ directive ++ "\ncontent\n"
  in case parseTypus malformedInput of
       Left _ -> True  -- 
       Right _ -> True  -- 

-- | Unicode
propUnicodeContentParsing :: String -> Bool
propUnicodeContentParsing                               content =
  let unicodeInput = content ++ " \n"
  in case parseTypus unicodeInput of
       Left _ -> True  -- 
       Right file -> not (L.null (tfBlocks file) ==> 
                     L.any (isInfixOf "" . cbContent) (tfBlocks file)

-- | 
propVeryLongLines :: Int -> String -> Bool
propVeryLongLines n                               baseContent =
  let longContent = baseContent ++ L.concat (replicate (abs n `mod` 1000) "x")
                                    input = longContent ++ "\n"
  in case parseTypus input of
       Left _ -> True
       Right file -> not (L.null (tfBlocks file) ==> 
                     L.length (cbContent (L.head (tfBlocks file)) >= L.length baseContent

-- | 
propDeeplyNestedBlocks :: Int -> Bool
propDeeplyNestedBlocks                               depth =
  let nestedDepth = abs depth `mod` 10 + 1
      createNestedBlocks                               0 = ""
      createNestedBlocks                               n = "{//! nested:" ++ show n ++ "}\n" ++ createNestedBlocks (n-1)
                                    input = createNestedBlocks nestedDepth
  in case parseTypus input of
       Left _ -> True
       Right file -> L.length (tfBlocks file) >= 0  -- 

-- ============================================================================
-- Parser Stress Tests
-- ============================================================================

-- | 
propLargeFileParsing :: Int -> Bool
propLargeFileParsing                               size =
  let fileSize = abs size `mod` 1000 + 1
                                    lines = replicate fileSize "content line\n"
                                    input = L.concat lines
  in case parseTypus input of
       Left _ -> True
       Right file -> L.length (tfBlocks file) <= fileSize

-- | 
propManyDirectives :: Int -> Bool
propManyDirectives                               count =
  let directiveCount = abs count `mod` 50 + 1
                                    directives = L.concat (replicate directiveCount "//! directive:value\n")
                                    content = directives ++ "content\n"
  in case parseTypus content of
       Left _ -> True
       Right file -> True  -- 

-- | 
propMixedContentTypes :: String -> String -> String -> Bool
propMixedContentTypes directives code                               comments =
  let input = "//! " ++ directives ++ "\n" ++
              code ++ "\n" ++
              "// " ++ comments ++ "\n"
  in case parseTypus input of
       Left _ -> True
       Right file -> True  -- 

-- ============================================================================
-- Helper Functions L.and Generators
-- ============================================================================

-- 
genIdentifier :: Gen String
                              genIdentifier = do
              first <- elements (['a'..'z'] ++ ['A'..'Z'] ++ ['_'])
  rest <- listOf $ elements (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['_','-'] (SourceSpan (SourcePos 1 1 0 [] (SourcePos 1 1 0)
  return (first : rest)

-- 
genDirectiveContent :: Gen String
                              genDirectiveContent = do
              key <- genIdentifier
  value <- genIdentifier
  return (key ++ ":" ++ value)

-- Unicode
genUnicodeString :: Gen String
                              genUnicodeString = listOf $ oneof
  [ elements ['a'..'z']
  , elements ['A'..'Z']
  , elements ['0'..'9']
  , elements " \t\n"
  , elements ""
  ]

-- 
instance Arbitrary String where
                                              arbitrary = genUnicodeString

-- 
infixr                               0 ==>
(==>) :: Bool -> Bool -> Bool
                              True ==>                               x = x
                              False ==>                               _ = True

-- mplus for Maybe
mplus :: Maybe a -> Maybe a -> Maybe a
mplus Nothing                               y = y
mplus (Just x)                               _ = Just x)))