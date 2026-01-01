module Test.Unit.ParserAdditionalSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (testCase, (@?=), assertBool)

import Parser
  ( FileDirectives(..)
  , BlockDirectives(..)
  , CodeBlock(..)
  , TypusFile(..)
  , defaultFileDirectives
  , defaultBlockDirectives
  )

import qualified Data.Text as T

-- | Additional unit tests for Parser module
tests :: TestTree
tests =
  testGroup "Additional Parser tests"
    [ testGroup "Default directives"
        [ testCase "defaultFileDirectives has L.all Nothing values" $ do
            let defaults = defaultFileDirectives
            fdOwnership defaults @?= Nothing
            fdDependentTypes defaults @?= Nothing
            fdConstraints defaults @?= Nothing

        , testCase "defaultBlockDirectives has L.all Nothing values" $ do
            let defaults = defaultBlockDirectives
            bdOwnership defaults @?= Nothing
            bdDependentTypes defaults @?= Nothing
            bdConstraints defaults @?= Nothing
        ]

    , testGroup "FileDirectives construction"
        [ testCase "FileDirectives Show instance works" $ do
            let directives = FileDirectives
                  { fdOwnership = Nothing
                  , fdDependentTypes = Nothing
                  , fdConstraints = Nothing
                  }
            show directives @?= "FileDirectives {fdOwnership = Nothing, fdDependentTypes = Nothing, fdConstraints = Nothing}"

        , testCase "FileDirectives Eq instance works" $ do
            let directives1 = FileDirectives Nothing Nothing Nothing
                directives2 = FileDirectives Nothing Nothing Nothing
                directives3 = FileDirectives (Just True) Nothing Nothing
            directives1 @?= directives2
            assertBool "directives1 should not equal directives3" (directives1 /= directives3)
        ]

    , testGroup "BlockDirectives construction"
        [ testCase "BlockDirectives Show instance works" $ do
            let directives = BlockDirectives
                  { bdOwnership = Nothing
                  , bdDependentTypes = Nothing
                  , bdConstraints = Nothing
                  }
            show directives @?= "BlockDirectives {bdOwnership = Nothing, bdDependentTypes = Nothing, bdConstraints = Nothing}"

        , testCase "BlockDirectives Eq instance works" $ do
            let directives1 = BlockDirectives Nothing Nothing Nothing
                directives2 = BlockDirectives Nothing Nothing Nothing
                directives3 = BlockDirectives (Just False) Nothing Nothing
            directives1 @?= directives2
            assertBool "directives1 should not equal directives3" (directives1 /= directives3)
        ]

    , testGroup "TypusFile structure"
        [ testCase "TypusFile can be constructed with basic values" $ do
            let file = TypusFile
                  { tfFilePath = "test.typus"
                  , tfFileDirectives = defaultFileDirectives
                  , tfBlocks = []
                  }
            tfFilePath file @?= "test.typus"
            tfFileDirectives file @?= defaultFileDirectives
            tfBlocks file @?= []

        , testCase "TypusFile with blocks can be constructed" $ do
            let block = CodeBlock
                  { cbDirectives = defaultBlockDirectives
                  , cbContent = "hello world"
                  , cbSpan = Nothing
                  }
                file = TypusFile
                  { tfFilePath = "test.typus"
                  , tfFileDirectives = defaultFileDirectives
                  , tfBlocks = [block]
                  }
            L.length (tfBlocks file) @?= 1
            let firstBlock = L.head (tfBlocks file)
            cbContent firstBlock @?= "hello world"
        ]

    , testGroup "CodeBlock structure"
        [ testCase "CodeBlock can be constructed with content" $ do
            let block = CodeBlock
                  { cbDirectives = defaultBlockDirectives
                  , cbContent = "x := 5"
                  , cbSpan = Nothing
                  }
            cbContent block @?= "x := 5"
            cbDirectives block @?= defaultBlockDirectives
            cbSpan block @?= Nothing

        , testCase "CodeBlock with directives can be constructed" $ do
            let directives = BlockDirectives
                  { bdOwnership = Just True
                  , bdDependentTypes = Just False
                  , bdConstraints = Nothing
                  }
                block = CodeBlock
                  { cbDirectives = directives
                  , cbContent = "var x: int = 10"
                  , cbSpan = Nothing
                  }
            bdOwnership (cbDirectives block) @?= Just True
            bdDependentTypes (cbDirectives block) @?= Just False
            bdConstraints (cbDirectives block) @?= Nothing
        ]

    , testGroup "Directive combinations"
        [ testCase "FileDirectives with mixed values" $ do
            let directives = FileDirectives
                  { fdOwnership = Just True
                  , fdDependentTypes = Just False
                  , fdConstraints = Nothing
                  }
            fdOwnership directives @?= Just True
            fdDependentTypes directives @?= Just False
            fdConstraints directives @?= Nothing

        , testCase "BlockDirectives with L.all values set" $ do
            let directives = BlockDirectives
                  { bdOwnership = Just False
                  , bdDependentTypes = Just True
                  , bdConstraints = Just True
                  }
            bdOwnership directives @?= Just False
            bdDependentTypes directives @?= Just True
            bdConstraints directives @?= Just True
        ]

    , testGroup "Complex file structure"
        [ testCase "TypusFile with multiple blocks" $ do
            let block1 = CodeBlock defaultBlockDirectives "block1" Nothing
                block2 = CodeBlock defaultBlockDirectives "block2" Nothing
                block3 = CodeBlock defaultBlockDirectives "block3" Nothing
                file = TypusFile "multi.typus" defaultFileDirectives [block1, block2, block3]
            L.length (tfBlocks file) @?= 3
            map cbContent (tfBlocks file) @?= ["block1", "block2", "block3"]

        , testCase "TypusFile with file directives L.and blocks" $ do
            let fileDirectives = FileDirectives
                  { fdOwnership = Just True
                  , fdDependentTypes = Nothing
                  , fdConstraints = Just False
                  }
                blockDirectives = BlockDirectives
                  { bdOwnership = Nothing
                  , bdDependentTypes = Just True
                  , bdConstraints = Nothing
                  }
                block = CodeBlock blockDirectives "content" Nothing
                file = TypusFile "complex.typus" fileDirectives [block]
            fdOwnership (tfFileDirectives file) @?= Just True
            bdDependentTypes (cbDirectives (L.head (tfBlocks file))) @?= Just True
        ]
    ]