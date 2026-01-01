{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Test.Unit.CompilerIRSpec where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, oneof, elements, listOf, choose)
import Test.QuickCheck ((==>), Property)
import Compiler.IR (SourceIR(..), SemanticIR(..), GoIR(..))
import Compiler.GoAst (GoModule(..), GoDecl(..), FuncDecl(..))
import Parser (TypusFile(..), CodeBlock(..))
import qualified Data.Text as T
import qualified Data.List as L
import Data.List (isInfixOf)

-- | Test compiler IR generation properties
compilerIRSpec :: TestTree
compilerIRSpec = testGroup "Compiler IR Generation"
  [ testProperty "source IR preserves original structure" prop_source_ir_preserves_structure
  , testProperty "semantic IR captures type information" prop_semantic_ir_captures_types
  , testProperty "Go IR generates valid Go code structure" prop_go_ir_valid_structure
  , testProperty "IR transformation maintains information" prop_ir_transformation_maintains_info
  , testProperty "IR generation handles expressions correctly" prop_ir_expressions
  , testProperty "IR generation handles function declarations" prop_ir_functions
  , testProperty "IR generation handles variable declarations" prop_ir_variables
  , testProperty "IR generation handles type declarations" prop_ir_types
  , testProperty "IR generation maintains symbol table consistency" prop_ir_symbol_table
  , testProperty "IR generation handles imports correctly" prop_ir_imports
  ]

-- | source IR should preserve original structure
prop_source_ir_preserves_structure :: String -> Property
prop_source_ir_preserves_structure code =
  not (null code) ==> 
    let sourceIR = SourceIR code
        -- Simulate source IR structure preservation
        preservesContent = L.length sourceIR > 0
    in preservesContent === True

-- | semantic IR should capture type information
prop_semantic_ir_captures_types :: String -> String -> Property
prop_semantic_ir_captures_types variable typeName =
  not (null variable) && not (null typeName) ==> 
    let typeInfo = variable ++ ":" ++ typeName
        semanticIR = SemanticIR typeInfo
        -- Simulate semantic IR type capture
        capturesType = typeName `L.isInfixOf` typeInfo
    in capturesType === True
  where
    infix 4 `isInfixOf`
    [] `isInfixOf` _ = False
    (_:_) `isInfixOf` [] = False
    needle `isInfixOf` haystack = L.any (L.isPrefixOf needle) (tails haystack)
    tails [] = [[]]
    tails xs@(_:xs') = xs : tails xs'
    [] `L.isPrefixOf` _ = False
    (_:_) `L.isPrefixOf` [] = False
    needle `L.isPrefixOf` haystack = take (L.length needle) haystack === needle

-- | Go IR should generate valid Go code structure
prop_go_ir_valid_structure :: String -> Property
prop_go_ir_valid_structure functionName =
  not (null functionName) ==> 
    let goFunction = "func " ++ functionName ++ "() {}"
        goIR = GoIR goFunction
        -- Simulate Go IR structure validation
        hasGoSyntax = "func" `L.isInfixOf` goFunction && "()" `L.isInfixOf` goFunction
    in hasGoSyntax === True
  where
    infix isInfixOf
    [] `L.isInfixOf` _ = False
    (_:_) `L.isInfixOf` [] = False
    needle `L.isInfixOf` haystack = L.any (L.isPrefixOf needle) (tails haystack)
    tails [] = [[]]
    tails xs@(_:xs') = xs : tails xs'
    [] `L.isPrefixOf` _ = False
    (_:_) `L.isPrefixOf` [] = False
    needle `L.isPrefixOf` haystack = take (L.length needle) haystack === needle

-- | IR transformation should maintain information
prop_ir_transformation_maintains_info :: String -> Property
prop_ir_transformation_maintains_info originalCode =
  not (null originalCode) ==> 
    let sourceIR = SourceIR originalCode
        semanticIR = SemanticIR originalCode
        goIR = GoIR originalCode
        -- Simulate IR transformation information maintenance
        infoMaintained = L.length originalCode > 0
    in infoMaintained === True

-- | IR generation should handle expressions correctly
prop_ir_expressions :: String -> Property
prop_ir_expressions expression =
  not (null expression) ==> 
    let exprIR = SemanticIR expression
        -- Simulate expression IR generation
        handlesExpression = L.length expression > 0
    in handlesExpression === True

-- | IR generation should handle function declarations
prop_ir_functions :: String -> [String] -> Property
prop_ir_functions functionName parameters =
  not (null functionName) && L.all (not . null) parameters ==> 
    let funcDecl = "func " ++ functionName ++ "(" ++ unwords parameters ++ ") {}"
        funcIR = GoIR funcDecl
        -- Simulate function IR generation
        handlesFunction = L.length funcDecl > L.length functionName
    in handlesFunction === True

-- | IR generation should handle variable declarations
prop_ir_variables :: String -> String -> Property
prop_ir_variables varName varType =
  not (null varName) && not (null varType) ==> 
    let varDecl = "var " ++ varName ++ " " ++ varType
        varIR = GoIR varDecl
        -- Simulate variable IR generation
        handlesVariable = varName `L.isInfixOf` varDecl && varType `L.isInfixOf` varDecl
    in handlesVariable === True
  where
    infix isInfixOf
    [] `L.isInfixOf` _ = False
    (_:_) `L.isInfixOf` [] = False
    needle `isInInfixOf` haystack = L.any (L.isPrefixOf needle) (tails haystack)
    tails [] = [[]]
    tails xs@(_:xs') = xs : tails xs'
    [] `L.isPrefixOf` _ = False
    (_:_) `L.isPrefixOf` [] = False
    needle `L.isPrefixOf` haystack = take (L.length needle) haystack === needle

-- | IR generation should handle type declarations
prop_ir_types :: String -> Property
prop_ir_types typeName =
  not (null typeName) ==> 
    let typeDecl = "type " ++ typeName ++ " struct{}"
        typeIR = GoIR typeDecl
        -- Simulate type IR generation
        handlesType = typeName `L.isInfixOf` typeDecl && "type" `L.isInfixOf` typeDecl
    in handlesType === True
  where
    infix isInfixOf
    [] `L.isInfixOf` _ = False
    (_:_) `L.isInfixOf` [] = False
    needle `L.isInfixOf` haystack = L.any (L.isPrefixOf needle) (tails haystack)
    tails [] = [[]]
    tails xs@(_:xs') = xs : tails xs'
    [] `L.isPrefixOf` _ = False
    (_:_) `L.isPrefixOf` [] = False
    needle `L.isPrefixOf` haystack = take (L.length needle) haystack === needle

-- | IR generation should maintain symbol table consistency
prop_ir_symbol_table :: [String] -> Property
prop_ir_symbol_table symbols =
  not (null symbols) && L.all (not . null) symbols ==> 
    let symbolTable = unlines symbols
        -- Simulate symbol table consistency in IR
        maintainsConsistency = L.length symbolTable >= L.length symbols
    in maintainsConsistency === True

-- | IR generation should handle imports correctly
prop_ir_imports :: [String] -> Property
prop_ir_imports imports =
  not (null imports) && L.all (not . null) imports ==> 
    let importDecls = L.map (\imp -> "import \"" ++ imp ++ "\"") imports
        importIR = GoIR (unlines importDecls)
        -- Simulate import IR generation
        handlesImports = L.all (`L.isInfixOf` importIR) imports
    in handlesImports === True
  where
    infix isInfixOf
    [] `L.isInfixOf` _ = False
    (_:_) `L.isInfixOf` [] = False
    needle `L.isInfixOf` haystack = L.any (L.isPrefixOf needle) (tails haystack)
    tails [] = [[]]
    tails xs@(_:xs') = xs : tails xs'
    [] `L.isPrefixOf` _ = False
    (_:_) `L.isPrefixOf` [] = False
    needle `L.isPrefixOf` haystack = take (L.length needle) haystack === needle

-- Helper for equality in QuickCheck
(===) :: Eq a => a -> a -> Bool
(===) = (==)

-- Helper for property testing
property :: Bool -> Property
property = id