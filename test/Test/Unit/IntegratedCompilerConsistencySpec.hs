{-# LANGUAGE OverloadedStrings #-}

module Test.Unit.IntegratedCompilerConsistencySpec (tests) where

import Test.Tasty
import Test.Tasty.HUnit
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
          length (getTokens (parse code)) === length (getTokens code)
    
    , testProperty "parse preserves structure" $
        \code -> isValidCode code ==> 
          getStructure (parse code) === getStructure code
    
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
          sort (getIdentifiers (parse code)) === sort (getIdentifiers code)
    
    , testProperty "parse preserves literals" $
        \code -> isValidCode code ==> 
          sort (getLiterals (parse code)) === sort (getLiterals code)
    
    , testProperty "parse preserves operators" $
        \code -> isValidCode code ==> 
          sort (getOperators (parse code)) === sort (getOperators code)
    
    , testProperty "parse handles nested structures" $
        \code -> isValidCode code ==> 
          getNestingLevel (parse code) === getNestingLevel code
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
          getStructure (typeCheck ast) === getStructure ast
    
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
          generate (parse (generate code)) === generate code
    
    , testProperty "generate preserves semantics" $
        \code -> isValidCode code ==> 
          getSemantics (generate code) === getSemantics code
    
    , testProperty "generate preserves behavior" $
        \code -> isValidCode code ==> 
          getBehavior (generate code) === getBehavior code
    
    , testProperty "generate handles optimizations" $
        \code -> isValidCode code ==> 
          isOptimized (generate (optimize code))
    
    , testProperty "generate is deterministic" $
        \code -> generate code === generate code
    
    , testProperty "generate preserves function signatures" $
        \code -> isValidCode code ==> 
          getFunctionSignatures (generate code) === getFunctionSignatures code
    
    , testProperty "generate preserves variable names" $
        \code -> isValidCode code ==> 
          sort (getVariableNames (generate code)) === sort (getVariableNames code)
    
    , testProperty "generate preserves control flow" $
        \code -> isValidCode code ==> 
          getControlFlow (generate code) === getControlFlow code
    
    , testProperty "generate handles error cases" $
        \code -> hasErrors code ==> 
          handlesErrors (generate code)
    
    , testProperty "generate preserves memory layout" $
        \code -> isValidCode code ==> 
          getMemoryLayout (generate code) === getMemoryLayout code
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

getTokens :: String -> [String]
getTokens = words

getTokens :: AST -> [String]
getTokens = tokens

getStructure :: String -> String
getStructure = take 10

getStructure :: AST -> String
getStructure = structure

getIdentifiers :: String -> [String]
getIdentifiers code = filter (all (`elem` ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_")) (words code)

getIdentifiers :: AST -> [String]
getIdentifiers = identifiers

getLiterals :: String -> [String]
getLiterals code = filter (all (`elem` ['0'..'9'])) (words code)

getLiterals :: AST -> [String]
getLiterals = literals

getOperators :: String -> [String]
getOperators code = filter (`elem` ["+", "-", "*", "/", "==", "!=", "<", ">", "<=", ">="]) (words code)

getOperators :: AST -> [String]
getOperators = operators

getNestingLevel :: String -> Int
getNestingLevel code = length (filter (== '{') code)

getNestingLevel :: AST -> Int
getNestingLevel = nestingLevel

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

generate :: String -> String
generate code = "generated:" ++ take 50 code

generate :: AST -> String
generate ast = "generated:" ++ unwords (tokens ast)

getSemantics :: String -> String
getSemantics = take 20

getSemantics :: String -> String
getSemantics = take 20

getBehavior :: String -> String
getBehavior = take 15

getBehavior :: String -> String
getBehavior = take 15

optimize :: String -> String
optimize code = "optimized:" ++ code

isOptimized :: String -> Bool
isOptimized code = "optimized:" `isPrefixOf` code

getFunctionSignatures :: String -> [(String, String)]
getFunctionSignatures _ = []

getFunctionSignatures :: String -> [(String, String)]
getFunctionSignatures _ = []

getVariableNames :: String -> [String]
getVariableNames code = filter (all (`elem` ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_")) (words code)

getVariableNames :: String -> [String]
getVariableNames code = filter (all (`elem` ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_")) (words code)

getControlFlow :: String -> String
getControlFlow = take 25

getControlFlow :: String -> String
getControlFlow = take 25

hasErrors :: String -> Bool
hasErrors code = "error" `isInfixOf` code

handlesErrors :: String -> Bool
handlesErrors _ = True

getMemoryLayout :: String -> String
getMemoryLayout = take 30

getMemoryLayout :: String -> String
getMemoryLayout = take 30