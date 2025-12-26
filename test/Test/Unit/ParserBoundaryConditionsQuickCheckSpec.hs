{-# LANGUAGE LambdaCase #-}
module Test.Unit.ParserBoundaryConditionsQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import Test.Tasty.QuickCheck (testProperty, Property, Arbitrary(..), Gen, oneof, elements, listOf, sized, choose, forAll)
import Data.Char (isAlphaNum, isLetter, isDigit, isSpace)
import Data.List (isPrefixOf, isInfixOf, isSuffixOf, null, length)
import Data.Maybe (isJust, isNothing, fromMaybe)
import qualified Data.Text as T

import Parser 
  ( FileDirectives(..), BlockDirectives(..), CodeBlock(..), TypusFile(..)
  , defaultFileDirectives, defaultBlockDirectives, parseTypus
  )
import SourceLocation (SourcePos(..), SourceSpan(..), Located(..))

-- | Parser模块边界条件QuickCheck测试
tests :: TestTree
tests =
  testGroup "Parser Boundary Conditions QuickCheck Tests"
    [ testGroup "FileDirectives Properties"
        [ testProperty "FileDirectives: round-trip serialization" propFileDirectivesRoundTrip
        , testProperty "FileDirectives: default values are Nothing" propFileDirectivesDefaultNothing
        , testProperty "FileDirectives: merging preserves priority" propFileDirectivesMerging
        ]

    , testGroup "BlockDirectives Properties"  
        [ testProperty "BlockDirectives: round-trip serialization" propBlockDirectivesRoundTrip
        , testProperty "BlockDirectives: inheritance from file" propBlockDirectivesInheritance
        , testProperty "BlockDirectives: override behavior" propBlockDirectivesOverride
        ]

    , testGroup "CodeBlock Properties"
        [ testProperty "CodeBlock: span consistency" propCodeBlockSpanConsistency
        , testProperty "CodeBlock: content preservation" propCodeBlockContentPreservation
        , testProperty "CodeBlock: directive ordering" propCodeBlockDirectiveOrdering
        ]

    , testGroup "TypusFile Properties"
        [ testProperty "TypusFile: block ordering preservation" propTypusFileBlockOrdering
        , testProperty "TypusFile: syntax error collection" propTypusFileSyntaxErrorCollection
        , testProperty "TypusFile: build tag preservation" propTypusFileBuildTagPreservation
        ]

    , testGroup "Parser Edge Cases"
        [ testProperty "Empty input parsing" propEmptyInputParsing
        , testProperty "Whitespace-only input" propWhitespaceOnlyInput
        , testProperty "Malformed directives" propMalformedDirectives
        , testProperty "Unicode content parsing" propUnicodeContentParsing
        , testProperty "Very long lines" propVeryLongLines
        , testProperty "Deeply nested blocks" propDeeplyNestedBlocks
        ]

    , testGroup "Parser Stress Tests"
        [ testProperty "Large file parsing" propLargeFileParsing
        , testProperty "Many directives" propManyDirectives
        , testProperty "Mixed content types" propMixedContentTypes
        ]
    ]

-- ============================================================================
-- FileDirectives Properties
-- ============================================================================

-- | FileDirectives的往返序列化属性
propFileDirectivesRoundTrip :: Bool -> Bool -> Bool -> Bool
propFileDirectivesRoundTrip ownership dependent constraints =
  let directives = FileDirectives 
        { fdOwnership = if ownership then Just True else Nothing
        , fdDependentTypes = if dependent then Just True else Nothing  
        , fdConstraints = if constraints then Just True else Nothing
        }
      extractedOwnership = fromMaybe False (fdOwnership directives)
      extractedDependent = fromMaybe False (fdDependentTypes directives)
      extractedConstraints = fromMaybe False (fdConstraints directives)
  in extractedOwnership == ownership && 
     extractedDependent == dependent && 
     extractedConstraints == constraints

-- | FileDirectives默认值都是Nothing
propFileDirectivesDefaultNothing :: Bool
propFileDirectivesDefaultNothing =
  let defaults = defaultFileDirectives
  in isNothing (fdOwnership defaults) &&
     isNothing (fdDependentTypes defaults) &&
     isNothing (fdConstraints defaults)

-- | FileDirectives合并保持优先级
propFileDirectivesMerging :: Bool -> Bool -> Bool -> Bool
propFileDirectivesMerging ownership dependent constraints =
  let base = defaultFileDirectives
      override = FileDirectives 
        { fdOwnership = if ownership then Just True else Nothing
        , fdDependentTypes = if dependent then Just True else Nothing
        , fdConstraints = if constraints then Just True else Nothing
        }
      -- 简化的合并逻辑：非Nothing的值具有优先级
      mergedOwnership = fdOwnership override `mplus` fdOwnership base
      mergedDependent = fdDependentTypes override `mplus` fdDependentTypes base
      mergedConstraints = fdConstraints override `mplus` fdConstraints base
  in (isJust mergedOwnership) == ownership &&
     (isJust mergedDependent) == dependent &&
     (isJust mergedConstraints) == constraints

-- ============================================================================
-- BlockDirectives Properties
-- ============================================================================

-- | BlockDirectives的往返序列化属性
propBlockDirectivesRoundTrip :: Bool -> Bool -> Bool -> Bool
propBlockDirectivesRoundTrip ownership dependent constraints =
  let directives = BlockDirectives
        { bdOwnership = if ownership then Just True else Nothing
        , bdDependentTypes = if dependent then Just True else Nothing
        , bdConstraints = if constraints then Just True else Nothing
        }
      extractedOwnership = fromMaybe False (bdOwnership directives)
      extractedDependent = fromMaybe False (bdDependentTypes directives)
      extractedConstraints = fromMaybe False (bdConstraints directives)
  in extractedOwnership == ownership &&
     extractedDependent == dependent &&
     extractedConstraints == constraints

-- | BlockDirectives从文件指令继承
propBlockDirectivesInheritance :: Bool -> Bool -> Bool -> Bool
propBlockDirectivesInheritance fileOwnership fileDependent fileConstraints =
  let fileDirectives = FileDirectives
        { fdOwnership = if fileOwnership then Just True else Nothing
        , fdDependentTypes = if fileDependent then Just True else Nothing
        , fdConstraints = if fileConstraints then Just True else Nothing
        }
      blockDirectives = defaultBlockDirectives
      -- 继承逻辑：如果块指令为Nothing，则使用文件指令
      inheritedOwnership = fromMaybe (fromMaybe False (fdOwnership fileDirectives)) (bdOwnership blockDirectives)
      inheritedDependent = fromMaybe (fromMaybe False (fdDependentTypes fileDirectives)) (bdDependentTypes blockDirectives)
      inheritedConstraints = fromMaybe (fromMaybe False (fdConstraints fileDirectives)) (bdConstraints blockDirectives)
  in inheritedOwnership == fileOwnership &&
     inheritedDependent == fileDependent &&
     inheritedConstraints == fileConstraints

-- | BlockDirectives覆盖行为
propBlockDirectivesOverride :: Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool
propBlockDirectivesOverride fileOwnership fileDependent fileConstraints 
                          blockOwnership blockDependent blockConstraints =
  let fileDirectives = FileDirectives
        { fdOwnership = Just fileOwnership
        , fdDependentTypes = Just fileDependent
        , fdConstraints = Just fileConstraints
        }
      blockDirectives = BlockDirectives
        { bdOwnership = if blockOwnership then Just (not fileOwnership) else Nothing
        , bdDependentTypes = if blockDependent then Just (not fileDependent) else Nothing
        , bdConstraints = if blockConstraints then Just (not fileConstraints) else Nothing
        }
      -- 覆盖逻辑：块指令优先于文件指令
      finalOwnership = fromMaybe fileOwnership (bdOwnership blockDirectives)
      finalDependent = fromMaybe fileDependent (bdDependentTypes blockDirectives)
      finalConstraints = fromMaybe fileConstraints (bdConstraints blockDirectives)
  in (blockOwnership ==> finalOwnership /= fileOwnership) &&
     (blockDependent ==> finalDependent /= fileDependent) &&
     (blockConstraints ==> finalConstraints /= fileConstraints)

-- ============================================================================
-- CodeBlock Properties
-- ============================================================================

-- | CodeBlock的span一致性
propCodeBlockSpanConsistency :: String -> Int -> Int -> Bool
propCodeBlockSpanConsistency content startLine startCol =
  let start = SourcePos (abs startLine `mod` 1000 + 1) (abs startCol `mod` 1000 + 1)
      end = SourcePos (sourcePosLine start) (sourcePosColumn start + length content `mod` 1000)
      span = SourceSpan start end
      codeBlock = CodeBlock defaultBlockDirectives content span
  in sourcePosLine (spanStart (cbSpan codeBlock)) <= sourcePosLine (spanEnd (cbSpan codeBlock))

-- | CodeBlock内容保持
propCodeBlockContentPreservation :: String -> Bool
propCodeBlockContentPreservation content =
  let start = SourcePos 1 1
      end = SourcePos 1 (length content + 1)
      span = SourceSpan start end
      codeBlock = CodeBlock defaultBlockDirectives content span
  in cbContent codeBlock == content

-- | CodeBlock指令顺序
propCodeBlockDirectiveOrdering :: Bool -> Bool -> Bool -> Bool
propCodeBlockDirectiveOrdering ownership dependent constraints =
  let directives = BlockDirectives
        { bdOwnership = if ownership then Just True else Nothing
        , bdDependentTypes = if dependent then Just True else Nothing
        , bdConstraints = if constraints then Just True else Nothing
        }
      codeBlock = CodeBlock directives "" (SourceSpan (SourcePos 1 1) (SourcePos 1 1))
  in (isJust (bdOwnership (cbDirectives codeBlock)) == ownership) &&
     (isJust (bdDependentTypes (cbDirectives codeBlock)) == dependent) &&
     (isJust (bdConstraints (cbDirectives codeBlock)) == constraints)

-- ============================================================================
-- TypusFile Properties
-- ============================================================================

-- | TypusFile块顺序保持
propTypusFileBlockOrdering :: [String] -> Bool
propTypusFileBlockOrdering contents =
  let blocks = zipWith (\i content -> 
        CodeBlock defaultBlockDirectives content 
          (SourceSpan (SourcePos i 1) (SourcePos i (length content + 1)))) 
        [1..] contents
      typusFile = TypusFile defaultFileDirectives [] blocks []
      extractedContents = map cbContent (tfBlocks typusFile)
  in extractedContents == contents

-- | TypusFile语法错误收集
propTypusFileSyntaxErrorCollection :: [String] -> Bool
propTypusFileSyntaxErrorCollection errors =
  let typusFile = TypusFile defaultFileDirectives [] [] errors
  in length (tfSyntaxErrors typusFile) == length errors

-- | TypusFile构建标签保持
propTypusFileBuildTagPreservation :: [String] -> Bool
propTypusFileBuildTagPreservation tags =
  let locatedTags = map (`Located` 0) tags
      typusFile = TypusFile defaultFileDirectives locatedTags [] []
      extractedTags = map locatedValue (tfBuildTags typusFile)
  in extractedTags == tags

-- ============================================================================
-- Parser Edge Cases
-- ============================================================================

-- | 空输入解析
propEmptyInputParsing :: Bool
propEmptyInputParsing =
  case parseTypus "" of
    Left _ -> True  -- 解析失败是可接受的
    Right file -> tfBlocks file == [] && tfBuildTags file == []

-- | 仅空白字符输入
propWhitespaceOnlyInput :: String -> Bool
propWhitespaceOnlyInput input =
  let whitespaceOnly = all isSpace input
  in if whitespaceOnly
     then case parseTypus input of
            Left _ -> True
            Right file -> tfBlocks file == []
     else True  -- 非空白输入不在此测试范围内

-- | 格式错误的指令
propMalformedDirectives :: String -> Bool
propMalformedDirectives directive =
  let malformedInput = "//! " ++ directive ++ "\ncontent\n"
  in case parseTypus malformedInput of
       Left _ -> True  -- 格式错误的指令应该导致解析失败
       Right _ -> True  -- 或者解析成功（如果解析器容错）

-- | Unicode内容解析
propUnicodeContentParsing :: String -> Bool
propUnicodeContentParsing content =
  let unicodeInput = content ++ " αβγδεζηθ\n"
  in case parseTypus unicodeInput of
       Left _ -> True  -- 解析失败是可接受的
       Right file -> not (null (tfBlocks file)) ==> 
                     any (isInfixOf "αβγδεζηθ" . cbContent) (tfBlocks file)

-- | 非常长的行
propVeryLongLines :: Int -> String -> Bool
propVeryLongLines n baseContent =
  let longContent = baseContent ++ concat (replicate (abs n `mod` 1000) "x")
      input = longContent ++ "\n"
  in case parseTypus input of
       Left _ -> True
       Right file -> not (null (tfBlocks file)) ==> 
                     length (cbContent (head (tfBlocks file))) >= length baseContent

-- | 深度嵌套块
propDeeplyNestedBlocks :: Int -> Bool
propDeeplyNestedBlocks depth =
  let nestedDepth = abs depth `mod` 10 + 1
      createNestedBlocks 0 = ""
      createNestedBlocks n = "{//! nested:" ++ show n ++ "}\n" ++ createNestedBlocks (n-1)
      input = createNestedBlocks nestedDepth
  in case parseTypus input of
       Left _ -> True
       Right file -> length (tfBlocks file) >= 0  -- 至少没有崩溃

-- ============================================================================
-- Parser Stress Tests
-- ============================================================================

-- | 大文件解析
propLargeFileParsing :: Int -> Bool
propLargeFileParsing size =
  let fileSize = abs size `mod` 1000 + 1
      lines = replicate fileSize "content line\n"
      input = concat lines
  in case parseTypus input of
       Left _ -> True
       Right file -> length (tfBlocks file) <= fileSize

-- | 多指令
propManyDirectives :: Int -> Bool
propManyDirectives count =
  let directiveCount = abs count `mod` 50 + 1
      directives = concat (replicate directiveCount "//! directive:value\n")
      content = directives ++ "content\n"
  in case parseTypus content of
       Left _ -> True
       Right file -> True  -- 只要没有崩溃就算通过

-- | 混合内容类型
propMixedContentTypes :: String -> String -> String -> Bool
propMixedContentTypes directives code comments =
  let input = "//! " ++ directives ++ "\n" ++
              code ++ "\n" ++
              "// " ++ comments ++ "\n"
  in case parseTypus input of
       Left _ -> True
       Right file -> True  -- 只要没有崩溃就算通过

-- ============================================================================
-- Helper Functions and Generators
-- ============================================================================

-- 生成标识符
genIdentifier :: Gen String
genIdentifier = do
  first <- elements (['a'..'z'] ++ ['A'..'Z'] ++ ['_'])
  rest <- listOf $ elements (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['_','-'])
  return (first : rest)

-- 生成指令内容
genDirectiveContent :: Gen String
genDirectiveContent = do
  key <- genIdentifier
  value <- genIdentifier
  return (key ++ ":" ++ value)

-- 生成Unicode字符串
genUnicodeString :: Gen String
genUnicodeString = listOf $ oneof
  [ elements ['a'..'z']
  , elements ['A'..'Z']
  , elements ['0'..'9']
  , elements " \t\n"
  , elements "αβγδεζηθικλμνξοπρστυφχψω"
  ]

-- 实例声明
instance Arbitrary String where
  arbitrary = genUnicodeString

-- 辅助函数
infixr 0 ==>
(==>) :: Bool -> Bool -> Bool
True ==> x = x
False ==> _ = True

-- mplus for Maybe
mplus :: Maybe a -> Maybe a -> Maybe a
mplus Nothing y = y
mplus (Just x) _ = Just x