{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Unit.TestParserDirectivesSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import qualified Parser as Parser
import SourceLocation
import ErrorHandler
import Utils
import qualified Data.Text as T
import TestSupport.Arbitrary ()
import Prelude hiding (spanBetween)

-- | Test suite for Parser Directives
testParserDirectives :: TestTree
testParserDirectives = testGroup "Parser Directives Tests"
  [ testCase "Parser: default file directives" $
      Parser.defaultFileDirectives @?= Parser.FileDirectives Nothing Nothing Nothing
           
  , testCase "Parser: default block directives" $
      Parser.defaultBlockDirectives @?= Parser.BlockDirectives Nothing Nothing Nothing
      
  , testCase "Parser: FileDirectives structure" $
      let directives = Parser.FileDirectives Nothing Nothing Nothing
      in do
        Parser.fdOwnership directives @?= Nothing
        Parser.fdDependentTypes directives @?= Nothing
        Parser.fdConstraints directives @?= Nothing
        
  , testCase "Parser: BlockDirectives structure" $
      let directives = Parser.BlockDirectives Nothing Nothing Nothing
      in do
        Parser.bdOwnership directives @?= Nothing
        Parser.bdDependentTypes directives @?= Nothing
        Parser.bdConstraints directives @?= Nothing
        
  , testCase "Parser: CodeBlock structure" $
      let codeBlock = Parser.CodeBlock 
            { Parser.cbDirectives = Parser.defaultBlockDirectives
            , Parser.cbContent = "package main\n\nfunc main() {\n    fmt.Println(\"hello\")\n}\n"
            , Parser.cbSpan = SourceLocation.SourceSpan (SourceLocation.SourcePos 1 1 0) (SourceLocation.SourcePos 8 1 0)
            }
      in do
        Parser.bdOwnership (Parser.cbDirectives codeBlock) @?= Nothing
        "package main" `isInfixOf` (Parser.cbContent codeBlock) @?= True
        
  , testCase "Parser: TypusFile structure" $
      let typusFile = Parser.TypusFile 
            { Parser.tfDirectives = Parser.defaultFileDirectives
            , Parser.tfBuildTags = []
            , Parser.tfBlocks = [
                Parser.CodeBlock 
                  { Parser.cbDirectives = Parser.defaultBlockDirectives
                  , Parser.cbContent = "package main\n\nfunc main() {\n    fmt.Println(\"hello\")\n}\n"
                  , Parser.cbSpan = SourceLocation.SourceSpan (SourceLocation.SourcePos 3 1 0) (SourceLocation.SourcePos 9 1 0)
                  }
              ]
            , Parser.tfSyntaxErrors = []
            }
      in do
        Parser.fdOwnership (Parser.tfDirectives typusFile) @?= Nothing
        Parser.fdDependentTypes (Parser.tfDirectives typusFile) @?= Nothing
        length (Parser.tfBlocks typusFile) @?= 1
        let block1 = Parser.tfBlocks typusFile !! 0
        Parser.bdOwnership (Parser.cbDirectives block1) @?= Nothing
  ]

-- Helper functions
isInfixOf :: String -> String -> Bool
isInfixOf needle haystack = needle `elem` (substrings haystack)
  where
    substrings s = [take i s | i <- [1..length s]]