{-# LANGUAGE RecordWildCards #-}
module Test.Unit.CodeGenerationQuickCheckSpec where



import Test.Tasty
import Test.Tasty.QuickCheck

import Compiler.IR
import Compiler.GoLexer (GoToken(..), GoTokenKind(..))
import Parser (TypusFile(..), CodeBlock(..), defaultFileDirectives, defaultBlockDirectives, tfContents)
import SourceLocation (SourcePos(..), emptySpan, sourceLine, sourceColumn)



-- | Test that IR statements maintain consistency
prop_ir_statement_consistency :: String -> String -> Property
prop_ir_statement_consistency stmtType stmtContent = 
    let nonEmptyType = if null stmtType then "stmt" else stmtType
        nonEmptyContent = if null stmtContent then "content" else stmtContent
        stmt = IRStatement { irStmtType = nonEmptyType, irStmtContent = nonEmptyContent }
    in property $ irStmtType stmt == nonEmptyType && irStmtContent stmt == nonEmptyContent

-- | Test that IR expressions maintain consistency  
prop_ir_expression_consistency :: String -> String -> Property
prop_ir_expression_consistency exprType exprContent = 
    let nonEmptyType = if null exprType then "expr" else exprType
        nonEmptyContent = if null exprContent then "content" else exprContent
        expr = IRExpression { irExprType = nonEmptyType, irExprContent = nonEmptyContent }
    in property $ irExprType expr == nonEmptyType && irExprContent expr == nonEmptyContent

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
    -- Use a non-empty string for identifier tokens, but handle empty strings gracefully
    let testStr = if null s then "default" else s
        token = GoToken TokIdentifier testStr
    in property $ tokenKind token == TokIdentifier && tokenText token == testStr

tests :: TestTree
tests = testGroup "CodeGeneration QuickCheck Tests"
  [ testProperty "IR statement consistency" prop_ir_statement_consistency
  , testProperty "IR expression consistency" prop_ir_expression_consistency
  , testProperty "TypusFile structure consistency" prop_typus_file_structure
  , testProperty "Source location basic properties" prop_source_location_basic
  , testProperty "Go token basic properties" prop_go_token_basic
  ]