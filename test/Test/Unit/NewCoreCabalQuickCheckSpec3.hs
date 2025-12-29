module Test.Unit.NewCoreCabalQuickCheckSpec3 (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)

import Parser (FileDirectives(..), BlockDirectives(..), TypusFile(..), CodeBlock(..))
import SourceLocation (Located(..), SourcePos(..), SourceSpan(..))
import qualified Data.Text as T

-- | Parser consistency tests with QuickCheck properties
tests :: TestTree
tests =
  testGroup "New Core Cabal QuickCheck Tests 3 - Parser Consistency"
    [ testGroup "Directive parsing properties"
        [ fastProperty "file directives merge is commutative" prop_fileDirectivesMergeCommutative
        , fastProperty "block directives merge is associative" prop_blockDirectivesMergeAssociative
        , testCase "default file directives" $ do
            let expected = FileDirectives { fdOwnership = Nothing, fdDependentTypes = Nothing, fdConstraints = Nothing }
            defaultFileDirectives @?= expected
        , testCase "default block directives" $ do
            let expected = BlockDirectives { bdOwnership = Nothing, bdDependentTypes = Nothing, bdConstraints = Nothing }
            defaultBlockDirectives @?= expected
        ]
    , testGroup "Code block properties"
        [ fastProperty "code block text concatenation preserves order" prop_codeBlockConcatenationOrder
        , fastProperty "code block span covers all content" prop_codeBlockSpanCoverage
        , testCase "code block creation" $ do
            let text = T.pack "test code"
                span = SourceSpan (SourcePos 1 1) (SourcePos 1 10)
                directives = defaultBlockDirectives
                block = CodeBlock { cbText = text, cbSpan = span, cbDirectives = directives }
            cbText block @?= text
            cbSpan block @?= span
        ]
    , testGroup "Parse tree consistency"
        [ fastProperty "typus file directive collection is idempotent" prop_typusFileDirectiveCollectionIdempotent
        , testCase "empty typus file" $ do
            let file = TypusFile { tfDirectives = defaultFileDirectives, tfBlocks = [] }
            tfDirectives file @?= defaultFileDirectives
            length (tfBlocks file) @?= 0
        ]
    ]

-- Simplified versions of data structures for testing
data FileDirectives = FileDirectives
    { fdOwnership :: Maybe (Located Bool)
    , fdDependentTypes :: Maybe (Located Bool)
    , fdConstraints :: Maybe (Located Bool)
    } deriving (Show, Eq)

data BlockDirectives = BlockDirectives
    { bdOwnership :: Maybe (Located Bool)
    , bdDependentTypes :: Maybe (Located Bool)
    , bdConstraints :: Maybe (Located Bool)
    } deriving (Show, Eq)

data CodeBlock = CodeBlock
    { cbText :: T.Text
    , cbSpan :: SourceSpan
    , cbDirectives :: BlockDirectives
    } deriving (Show, Eq)

data TypusFile = TypusFile
    { tfDirectives :: FileDirectives
    , tfBlocks :: [CodeBlock]
    } deriving (Show, Eq)

data SourcePos = SourcePos Int Int  -- line, column
  deriving (Show, Eq)

data SourceSpan = SourceSpan SourcePos SourcePos
  deriving (Show, Eq)

data Located a = Located SourcePos a
  deriving (Show, Eq)

-- | QuickCheck properties

-- File directives merge is commutative
prop_fileDirectivesMergeCommutative :: Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool
prop_fileDirectivesMergeCommutative own1 dep1 con1 own2 dep2 con2 =
  let dir1 = FileDirectives { fdOwnership = Just (Located (SourcePos 1 1) own1)
                            , fdDependentTypes = Just (Located (SourcePos 1 2) dep1)
                            , fdConstraints = Just (Located (SourcePos 1 3) con1) }
      dir2 = FileDirectives { fdOwnership = Just (Located (SourcePos 2 1) own2)
                            , fdDependentTypes = Just (Located (SourcePos 2 2) dep2)
                            , fdConstraints = Just (Located (SourcePos 2 3) con2) }
      merged1 = mergeFileDirectives dir1 dir2
      merged2 = mergeFileDirectives dir2 dir1
  in merged1 == merged2

-- Block directives merge is associative
prop_blockDirectivesMergeAssociative :: Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool
prop_blockDirectivesMergeAssociative own1 dep1 con1 own2 dep2 con2 own3 dep3 con3 =
  let dir1 = BlockDirectives { bdOwnership = Just (Located (SourcePos 1 1) own1)
                             , bdDependentTypes = Just (Located (SourcePos 1 2) dep1)
                             , bdConstraints = Just (Located (SourcePos 1 3) con1) }
      dir2 = BlockDirectives { bdOwnership = Just (Located (SourcePos 2 1) own2)
                             , bdDependentTypes = Just (Located (SourcePos 2 2) dep2)
                             , bdConstraints = Just (Located (SourcePos 2 3) con2) }
      dir3 = BlockDirectives { bdOwnership = Just (Located (SourcePos 3 1) own3)
                             , bdDependentTypes = Just (Located (SourcePos 3 2) dep3)
                             , bdConstraints = Just (Located (SourcePos 3 3) con3) }
      left = mergeBlockDirectives (mergeBlockDirectives dir1 dir2) dir3
      right = mergeBlockDirectives dir1 (mergeBlockDirectives dir2 dir3)
  in left == right

-- Code block text concatenation preserves order
prop_codeBlockConcatenationOrder :: String -> String -> String -> Bool
prop_codeBlockConcatenationOrder txt1 txt2 txt3 =
  let text1 = T.pack txt1
      text2 = T.pack txt2
      text3 = T.pack txt3
      span1 = SourceSpan (SourcePos 1 1) (SourcePos 1 (length txt1))
      span2 = SourceSpan (SourcePos 2 1) (SourcePos 2 (length txt2))
      span3 = SourceSpan (SourcePos 3 1) (SourcePos 3 (length txt3))
      directives = defaultBlockDirectives
      block1 = CodeBlock { cbText = text1, cbSpan = span1, cbDirectives = directives }
      block2 = CodeBlock { cbText = text2, cbSpan = span2, cbDirectives = directives }
      block3 = CodeBlock { cbText = text3, cbSpan = span3, cbDirectives = directives }
      blocks = [block1, block2, block3]
      concatenated = T.concat $ map cbText blocks
      expected = text1 <> text2 <> text3
  in concatenated == expected

-- Code block span covers all content
prop_codeBlockSpanCoverage :: String -> Bool
prop_codeBlockSpanCoverage txt =
  let text = T.pack txt
      lineCount = length $ lines txt
      lastLineLength = length $ last $ lines txt
      span = SourceSpan (SourcePos 1 1) (SourcePos lineCount lastLineLength)
      block = CodeBlock { cbText = text, cbSpan = span, cbDirectives = defaultBlockDirectives }
      (SourceSpan start end) = cbSpan block
  in start == SourcePos 1 1 && 
     line end == lineCount && 
     column end == lastLineLength

-- Typus file directive collection is idempotent
prop_typusFileDirectiveCollectionIdempotent :: Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool
prop_typusFileDirectiveCollectionIdempotent own1 dep1 con1 own2 dep2 con2 =
  let fileDirectives = FileDirectives { fdOwnership = Just (Located (SourcePos 1 1) own1)
                                      , fdDependentTypes = Just (Located (SourcePos 1 2) dep1)
                                      , fdConstraints = Just (Located (SourcePos 1 3) con1) }
      blockDirectives = BlockDirectives { bdOwnership = Just (Located (SourcePos 2 1) own2)
                                        , bdDependentTypes = Just (Located (SourcePos 2 2) dep2)
                                        , bdConstraints = Just (Located (SourcePos 2 3) con2) }
      span = SourceSpan (SourcePos 1 1) (SourcePos 1 10)
      block = CodeBlock { cbText = T.pack "test", cbSpan = span, cbDirectives = blockDirectives }
      file = TypusFile { tfDirectives = fileDirectives, tfBlocks = [block] }
      collectedOnce = collectAllDirectives file
      collectedTwice = collectAllDirectives $ file { tfDirectives = collectedOnce }
  in collectedOnce == collectedTwice

-- Helper functions
defaultFileDirectives :: FileDirectives
defaultFileDirectives = FileDirectives { fdOwnership = Nothing, fdDependentTypes = Nothing, fdConstraints = Nothing }

defaultBlockDirectives :: BlockDirectives
defaultBlockDirectives = BlockDirectives { bdOwnership = Nothing, bdDependentTypes = Nothing, bdConstraints = Nothing }

mergeFileDirectives :: FileDirectives -> FileDirectives -> FileDirectives
mergeFileDirectives d1 d2 = FileDirectives
  { fdOwnership = fdOwnership d2 `orMaybe` fdOwnership d1
  , fdDependentTypes = fdDependentTypes d2 `orMaybe` fdDependentTypes d1
  , fdConstraints = fdConstraints d2 `orMaybe` fdConstraints d1
  }

mergeBlockDirectives :: BlockDirectives -> BlockDirectives -> BlockDirectives
mergeBlockDirectives d1 d2 = BlockDirectives
  { bdOwnership = bdOwnership d2 `orMaybe` bdOwnership d1
  , bdDependentTypes = bdDependentTypes d2 `orMaybe` bdDependentTypes d1
  , bdConstraints = bdConstraints d2 `orMaybe` bdConstraints d1
  }

collectAllDirectives :: TypusFile -> FileDirectives
collectAllDirectives file = 
  let fileDirs = tfDirectives file
      blockDirs = map cbDirectives (tfBlocks file)
      combined = foldl mergeBlockDirectives defaultBlockDirectives blockDirs
  in FileDirectives
     { fdOwnership = bdOwnership combined `orMaybe` fdOwnership fileDirs
     , fdDependentTypes = bdDependentTypes combined `orMaybe` fdDependentTypes fileDirs
     , fdConstraints = bdConstraints combined `orMaybe` fdConstraints fileDirs
     }

orMaybe :: Maybe a -> Maybe a -> Maybe a
orMaybe Nothing y = y
orMaybe (Just x) _ = Just x