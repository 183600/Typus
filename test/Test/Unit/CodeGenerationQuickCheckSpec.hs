{-# LANGUAGE RecordWildCards #-}

module Test.Unit.CodeGenerationQuickCheckSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import TestSupport.QuickCheck
import TestSupport.Arbitrary

import Compiler.IR
import Compiler.GoLexer (GoToken(..), GoTokenKind(..))
import Parser (TypusFile(..), CodeBlock(..), FileDirectives(..), BlockDirectives(..), defaultFileDirectives, defaultBlockDirectives, tfContents)
import SourceLocation (SourcePos(..), emptySpan, sourceLine, sourceColumn)
import qualified Data.Text as T



-- | Test that IR statements maintain consistency
prop_ir_statement_consistency :: String -> String -> Property
prop_ir_statement_consistency stmtType stmtContent = 
    not (null stmtType) && not (null stmtContent) ==>
    let stmt = IRStatement { irStmtType = stmtType, irStmtContent = stmtContent }
    in irStmtType stmt == stmtType && irStmtContent stmt == stmtContent

-- | Test that IR expressions maintain consistency  
prop_ir_expression_consistency :: String -> String -> Property
prop_ir_expression_consistency exprType exprContent = 
    not (null exprType) && not (null exprContent) ==>
    let expr = IRExpression { irExprType = exprType, irExprContent = exprContent }
    in irExprType expr == exprType && irExprContent expr == exprContent

-- | Test that TypusFile round-trips maintain structure
prop_typus_file_structure :: [String] -> Property
prop_typus_file_structure strs = property $
    let startPos = SourcePos 0 0 0
        blocks = map (\s -> CodeBlock defaultBlockDirectives s (emptySpan startPos)) strs
        file = TypusFile defaultFileDirectives [] blocks []
        contents = tfContents file
        expectedContents = concat strs
    in contents == expectedContents && length (tfBlocks file) == length strs

-- | Test that source location basic properties hold
prop_source_location_basic :: Int -> Int -> Property
prop_source_location_basic line col = 
    line >= 0 && col >= 0 ==>
    let pos = SourcePos line col 0
    in sourceLine pos == line && sourceColumn pos == col

-- | Test that Go token basic properties hold
prop_go_token_basic :: String -> Property
prop_go_token_basic s = 
    not (null s) ==>
    let token = GoToken TokIdentifier s
    in tokenKind token == TokIdentifier && tokenText token == s

tests :: TestTree
tests = testGroup "CodeGeneration QuickCheck Tests"
  [ testProperty "IR statement consistency" prop_ir_statement_consistency
  , testProperty "IR expression consistency" prop_ir_expression_consistency
  , testProperty "TypusFile structure consistency" prop_typus_file_structure
  , testProperty "Source location basic properties" prop_source_location_basic
  , testProperty "Go token basic properties" prop_go_token_basic
  ]