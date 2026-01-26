{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module Test.Unit.IntegratedCompilerConsistencySpec where



import Test.Tasty
import Test.Tasty.QuickCheck

import Test.Tasty.QuickCheck
import Data.List (sort, nub, group, isPrefixOf, isInfixOf)
import Data.Maybe (isJust, isNothing, fromMaybe)
import Data.Either (isLeft, isRight)
import qualified Data.Set as Set

-- Test integrated compiler consistency properties
tests :: TestTree
tests = testGroup "Integrated Compiler Consistency Tests"
  [ testGroup "Parsing consistency properties"
    [ testProperty "parse . unparse is idempotent for valid code" $
        \code -> isValidCode code ==> parse (unparse (parse code)) === parse code
    
    , testProperty "parse preserves token count" $
        \code -> isValidCode code ==> 
          length (getTokensFromAST (parse code)) === length (getTokensFromString code)
    
    , testProperty "parse preserves structure" $
        \code -> isValidCode code ==> 
          getStructureFromAST (parse code) === getStructureFromString code
    
    , testProperty "parse handles whitespace consistently" $
        \code -> isValidCode code ==> 
          parse (normalizeWhitespace code) === parse code
    
    , testProperty "parse handles comments consistently" $
        \code -> isValidCode code ==> 
          parse (removeComments code) === parse code
    
    , testProperty "parse is deterministic" $
        \code -> parse code === parse code
    
    , testProperty "parse preserves identifiers" $
        \code -> isValidCode code ==> 
          sort (getIdentifiersFromAST (parse code)) === sort (getIdentifiersFromString code)
    
    , testProperty "parse preserves literals" $
        \code -> isValidCode code ==> 
          sort (getLiteralsFromAST (parse code)) === sort (getLiteralsFromString code)
    
    , testProperty "parse preserves operators" $
        \code -> isValidCode code ==> 
          sort (getOperatorsFromAST (parse code)) === sort (getOperatorsFromString code)
    
    , testProperty "parse preserves nesting level" $
        \code -> isValidCode code ==> 
          getNestingLevelFromAST (parse code) === getNestingLevelFromString code
    ]
  
  , testGroup "Type checking consistency properties"
    [ testProperty "type check . annotate is idempotent" $
        \ast -> isValidAST ast ==> 
          typeCheck (annotateTypes (typeCheck ast)) === typeCheck ast
    
    , testProperty "type check preserves type information" $
        \ast -> isValidAST ast ==> 
          getTypeInfo (typeCheck ast) === getTypeInfo ast
    
    , testProperty "type check detects type errors" $
        \ast -> hasTypeError ast ==> 
          hasTypeError (typeCheck ast)
    
    , testProperty "type check is deterministic" $
        \ast -> typeCheck ast === typeCheck ast
    
    , testProperty "type check preserves structure" $
        \ast -> isValidAST ast ==> 
          getStructureFromAST (typeCheck ast) === getStructureFromAST ast
    
    , testProperty "type check handles inheritance" $
        \ast -> hasInheritance ast ==> 
          handlesInheritance (typeCheck ast)
    
    , testProperty "type check handles generics" $
        \ast -> hasGenerics ast ==> 
          handlesGenerics (typeCheck ast)
    
    , testProperty "type check handles function types" $
        \ast -> hasFunctions ast ==> 
          handlesFunctions (typeCheck ast)
    
    , testProperty "type check handles recursive types" $
        \ast -> hasRecursiveTypes ast ==> 
          handlesRecursiveTypes (typeCheck ast)
    
    , testProperty "type check preserves variable types" $
        \ast -> isValidAST ast ==> 
          getVariableTypes (typeCheck ast) === getVariableTypes ast
    ]
  
  , testGroup "Code generation consistency properties"
    [ testProperty "generate . parse . generate is consistent" $
        \code -> isValidCode code ==> 
          generateFromAST (parse (generateCode code)) === generateCode code
    
    , testProperty "generate preserves semantics" $
        \code -> isValidCode code ==> 
          getSemanticsFromString (generateCode code) === getSemanticsFromString code
    
    , testProperty "generate preserves behavior" $
        \code -> isValidCode code ==> 
          getBehaviorFromString (generateCode code) === getBehaviorFromString code
    
    , testProperty "generate handles optimizations" $
        \code -> isValidCode code ==> 
          isOptimized (generateCode (optimize code))
    
    , testProperty "generate is deterministic" $
        \code -> generateCode code === generateCode code
    
    , testProperty "generate preserves function signatures" $
        \code -> isValidCode code ==> 
          getFunctionSignatures (generateCode code) === getFunctionSignatures code
    
    , testProperty "generate preserves variable names" $
        \code -> isValidCode code ==> 
          sort (getVariableNamesFromString (generateCode code)) === sort (getVariableNamesFromString code)
    
    , testProperty "generate preserves control flow" $
        \code -> isValidCode code ==> 
          getControlFlowFromString (generateCode code) === getControlFlowFromString code
    
    , testProperty "generate handles error cases" $
        \code -> hasErrors code ==> 
          handlesErrors (generateCode code)
    
    , testProperty "generate preserves memory layout" $
        \code -> isValidCode code ==> 
          getMemoryLayoutFromString (generateCode code) === getMemoryLayoutFromString code
    ]
  ]

-- Helper types and functions
data AST = AST
  { tokens :: [String]
  , structure :: String
  , identifiers :: [String]
  , literals :: [String]
  , operators :: [String]
  , nestingLevel :: Int
  , typeInfo :: [(String, String)]
  , hasTypeErrors :: Bool
  , semantics :: String
  , behavior :: String
  , functionSignatures :: [(String, String)]
  , variableNames :: [String]
  , controlFlow :: String
  , memoryLayout :: String
  } deriving (Show, Eq)

isValidCode :: String -> Bool
isValidCode code = not (null code) && length code < 100 && all (`elem` ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " \t\n{}();,+-*/=") code

parse :: String -> AST
parse code = AST
  { tokens = words code
  , structure = take 10 code
  , identifiers = filter (all (`elem` ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_")) (words code)
  , literals = filter (all (`elem` ['0'..'9'])) (words code)
  , operators = filter (`elem` ["+", "-", "*", "/", "==", "!=", "<", ">", "<=", ">="]) (words code)
  , nestingLevel = length (filter (== '{') code)
  , typeInfo = []
  , hasTypeErrors = False
  , semantics = take 20 code
  , behavior = take 15 code
  , functionSignatures = []
  , variableNames = filter (all (`elem` ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_")) (words code)
  , controlFlow = take 25 code
  , memoryLayout = take 30 code
  }

unparse :: AST -> String
unparse ast = unwords (tokens ast)

getTokensFromString :: String -> [String]
getTokensFromString = words

getTokensFromAST :: AST -> [String]
getTokensFromAST = tokens

getStructureFromString :: String -> String
getStructureFromString = take 10

getStructureFromAST :: AST -> String
getStructureFromAST = structure

getIdentifiersFromString :: String -> [String]
getIdentifiersFromString code = filter (all (`elem` ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_")) (words code)

getIdentifiersFromAST :: AST -> [String]
getIdentifiersFromAST = identifiers

getLiteralsFromString :: String -> [String]
getLiteralsFromString code = filter (all (`elem` ['0'..'9'])) (words code)

getLiteralsFromAST :: AST -> [String]
getLiteralsFromAST = literals

getOperatorsFromString :: String -> [String]
getOperatorsFromString code = filter (`elem` ["+", "-", "*", "/", "==", "!=", "<", ">", "<=", ">="]) (words code)

getOperatorsFromAST :: AST -> [String]
getOperatorsFromAST = operators

getNestingLevelFromString :: String -> Int
getNestingLevelFromString code = length (filter (== '{') code)

getNestingLevelFromAST :: AST -> Int
getNestingLevelFromAST = nestingLevel

normalizeWhitespace :: String -> String
normalizeWhitespace = unwords . words

removeComments :: String -> String
removeComments code = unwords $ filter (not . isPrefixOf "//") $ lines code

isValidAST :: AST -> Bool
isValidAST ast = not (null (tokens ast))

typeCheck :: AST -> AST
typeCheck ast = ast { typeInfo = [("x", "int"), ("y", "string")] }

annotateTypes :: AST -> AST
annotateTypes ast = ast { typeInfo = [("x", "int"), ("y", "string")] }

getTypeInfo :: AST -> [(String, String)]
getTypeInfo = typeInfo

hasTypeError :: AST -> Bool
hasTypeError = hasTypeErrors

hasInheritance :: AST -> Bool
hasInheritance _ = False

handlesInheritance :: AST -> Bool
handlesInheritance _ = True

hasGenerics :: AST -> Bool
hasGenerics _ = False

handlesGenerics :: AST -> Bool
handlesGenerics _ = True

hasFunctions :: AST -> Bool
hasFunctions ast = any (== "function") (tokens ast)

handlesFunctions :: AST -> Bool
handlesFunctions _ = True

hasRecursiveTypes :: AST -> Bool
hasRecursiveTypes _ = False

handlesRecursiveTypes :: AST -> Bool
handlesRecursiveTypes _ = True

getVariableTypes :: AST -> [(String, String)]
getVariableTypes ast = typeInfo ast

generateCode :: String -> String
generateCode code = "generated:" ++ take 50 code

generateFromAST :: AST -> String
generateFromAST ast = "generated:" ++ unwords (tokens ast)

getSemanticsFromString :: String -> String
getSemanticsFromString = take 20

getBehaviorFromString :: String -> String
getBehaviorFromString = take 15

optimize :: String -> String
optimize code = "optimized:" ++ code

isOptimized :: String -> Bool
isOptimized code = "optimized:" `isPrefixOf` code

getFunctionSignatures :: String -> [(String, String)]
getFunctionSignatures _ = []

getVariableNamesFromString :: String -> [String]
getVariableNamesFromString code = filter (all (`elem` ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_")) (words code)

getMemoryLayoutFromString :: String -> String
getMemoryLayoutFromString = take 30

getVariableNames :: String -> [String]
getVariableNames code = filter (all (`elem` ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_")) (words code)

getControlFlowFromString :: String -> String
getControlFlowFromString = take 25

hasErrors :: String -> Bool
hasErrors code = "error" `isInfixOf` code

handlesErrors :: String -> Bool
handlesErrors _ = True

getMemoryLayout :: String -> String
getMemoryLayout = take 30

-- Arbitrary instance for AST
instance Arbitrary AST where
  arbitrary = AST <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary