module Test.Unit.NewCoreFunctionalitySpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)

import SourceLocation (SourcePos(..), SourceSpan(..), Located(..), startPos, emptySpan, spanFrom, isValidSpan)
import Compiler.Errors.Core (ErrorSeverity(..), ErrorCategory(..), ErrorCollector, newErrorCollector, addError, getErrors, hasErrors)
import Dependencies (AST(..), Statement(..), TypeExpr(..), Constraint(..), TypeVar(..))

-- | Core functionality tests for fundamental Typus components
tests :: TestTree
tests =
  testGroup "New Core Functionality Tests"
    [ testGroup "SourceLocation tests"
        [ testCase "startPos creates position at line 1, column 1" $ do
            let pos = startPos
            posLine pos @?= 1
            posColumn pos @?= 1

        , testCase "emptySpan has zero length" $ do
            let span = emptySpan
            isValidSpan span @?= False

        , testCase "spanFrom creates valid span from position" $ do
            let pos = SourcePos 5 10
            let span = spanFrom pos
            isValidSpan span @?= True

        , testCase "Located values preserve their location" $ do
            let locatedValue = Located (SourcePos 2 3) "test"
            locatedValue locatedValue @?= "test"
            posLine (locatedPos locatedValue) @?= 2
            posColumn (locatedPos locatedValue) @?= 3
        ]

    , testGroup "ErrorHandler tests"
        [ testCase "newErrorCollector starts with no errors" $ do
            collector <- newErrorCollector
            hasErrors collector @?= False

        , testCase "addError adds error to collector" $ do
            collector <- newErrorCollector
            let errorMsg = "Test error message"
            updatedCollector <- addError collector errorMsg
            hasErrors updatedCollector @?= True
            errors <- getErrors updatedCollector
            assertBool "Error list should contain the added error" (not (null errors))
        ]

    , testGroup "Dependencies AST tests"
        [ testCase "AST constructor creates empty program" $ do
            let ast = AST []
            case ast of
                AST statements -> null statements @?= True

        , testCase "Statement types can be constructed" $ do
            let typeVar = TypeVar "T"
            let typeExpr = TypeVarExpr typeVar
            let statement = TypeDecl "x" typeExpr
            case statement of
                TypeDecl name expr -> do
                    name @?= "x"
                    case expr of
                        TypeVarExpr tv -> TypeVarName tv @?= "T"
                        _ -> assertBool "Should be TypeVarExpr" False

        , testCase "Constraint can be created with type variables" $ do
            let typeVar1 = TypeVar "A"
            let typeVar2 = TypeVar "B"
            let constraint = EqualityConstraint typeVar1 typeVar2
            case constraint of
                EqualityConstraint tv1 tv2 -> do
                    TypeVarName tv1 @?= "A"
                    TypeVarName tv2 @?= "B"
        ]
    ]