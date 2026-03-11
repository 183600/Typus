{-# LANGUAGE OverloadedStrings #-}

module TestSupport.MemoryEfficientGenerators
  ( smallString
  , boundedList
  , smallInt
  , constrainedText
  , limitedArbitrary
  , memorySafeArbitrary
  , resizeSmall
  , generateSmall
  , smallProperty
  , boundedProperty
  ) where

import Test.QuickCheck (Arbitrary(..), Gen, Property, Testable, arbitrary, choose, elements, forAll, listOf, listOf1, oneof, property, resize)
import qualified Data.Text as T
import Data.Char (isAlphaNum)
import Data.List (intercalate)

-- | Generate small strings (max 32 characters) to reduce memory usage
smallString :: Gen String
smallString = resize 32 arbitrary

-- | Generate bounded lists with small size (max 10 elements)
boundedList :: Gen a -> Gen [a]
boundedList gen = resize 10 $ listOf gen

-- | Generate small integers (range: -100 to 100)
smallInt :: Gen Int
smallInt = choose (-100, 100)

-- | Generate constrained text with limited length (max 50 characters)
constrainedText :: Gen T.Text
constrainedText = T.pack <$> resize 50 arbitrary

-- | Create a memory-safe arbitrary instance with size limits
limitedArbitrary :: (Arbitrary a) => Gen a
limitedArbitrary = resize 10 arbitrary

-- | Memory-safe arbitrary with aggressive size limits
memorySafeArbitrary :: (Arbitrary a) => Gen a
memorySafeArbitrary = resize 5 arbitrary

-- | Helper to resize generators to small sizes
resizeSmall :: Gen a -> Gen a
resizeSmall = resize 8

-- | Generate small values with aggressive limits
generateSmall :: (Arbitrary a) => Gen a
generateSmall = resize 3 arbitrary

-- | Create properties with small generated data
smallProperty :: (Show a, Testable prop) => Gen a -> (a -> prop) -> Property
smallProperty gen prop = forAll (resizeSmall gen) prop

-- | Create properties with bounded generated data
boundedProperty :: (Show a, Testable prop) => Gen a -> (a -> prop) -> Property
boundedProperty gen prop = forAll (resize 5 gen) prop

-- | Generate small alphanumeric strings (max 20 characters)
smallAlphaNumString :: Gen String
smallAlphaNumString = resize 20 $ listOf (choose ('a', 'z'))

-- | Generate small positive integers (range: 1 to 50)
smallPositiveInt :: Gen Int
smallPositiveInt = choose (1, 50)

-- | Generate small natural numbers (range: 0 to 100)
smallNatural :: Gen Int
smallNatural = choose (0, 100)

-- | Generate identifiers with limited length
smallIdentifier :: Gen String
smallIdentifier = do
  first <- choose ('a', 'z')
  rest <- resize 7 $ listOf (choose ('a', 'z'))
  return (first : rest)

-- | Generate file paths with limited depth
smallFilePath :: Gen String
smallFilePath = do
  segments <- resize 2 $ listOf1 smallIdentifier
  return $ intercalate "/" segments

-- | Generate module names with limited depth
smallModuleName :: Gen String
smallModuleName = do
  segments <- resize 2 $ listOf1 smallIdentifier
  return $ intercalate "." segments

-- | Generate simple expressions with limited depth
simpleExpression :: Gen String
simpleExpression = do
  var <- smallIdentifier
  op <- elements ["+", "-", "*", "/"]
  num <- smallInt
  return $ var ++ " " ++ op ++ " " ++ show num

-- | Generate type annotations with limited complexity
simpleTypeAnnotation :: Gen String
simpleTypeAnnotation = do
  var <- smallIdentifier
  typ <- elements ["Int", "String", "Bool", "Float"]
  return $ var ++ " : " ++ typ

-- | Generate imports with limited complexity
simpleImport :: Gen String
simpleImport = do
  moduleName <- smallModuleName
  return $ "import " ++ moduleName

-- | Generate function definitions with limited complexity
simpleFunction :: Gen String
simpleFunction = do
  name <- smallIdentifier
  param <- smallIdentifier
  body <- simpleExpression
  return $ "func " ++ name ++ "(" ++ param ++ ") {" ++ body ++ "}"

-- | Generate variable declarations with limited complexity
simpleVariable :: Gen String
simpleVariable = do
  name <- smallIdentifier
  value <- smallInt
  return $ "var " ++ name ++ " = " ++ show value

-- | Generate small test programs
smallTestProgram :: Gen String
smallTestProgram = do
  imports <- resize 2 $ listOf simpleImport
  vars <- resize 3 $ listOf simpleVariable
  funcs <- resize 2 $ listOf simpleFunction
  let lines = imports ++ vars ++ funcs
  return $ unlines lines

-- | Generate memory-efficient test data for parser tests
memoryEfficientParserInput :: Gen String
memoryEfficientParserInput = resize 100 smallTestProgram

-- | Generate memory-efficient test data for compiler tests
memoryEfficientCompilerInput :: Gen String
memoryEfficientCompilerInput = resize 80 smallTestProgram

-- | Generate memory-efficient test data for type system tests
memoryEfficientTypeSystemInput :: Gen String
memoryEfficientTypeSystemInput = resize 60 smallTestProgram