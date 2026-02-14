#!/usr/bin/env runhaskell

import Parser (parseTypus)
import Data.List (isPrefixOf, isInfixOf)

-- 测试空字符串的情况
testEmptyString :: IO ()
testEmptyString = do
  putStrLn "Testing empty string..."
  let result = parseTypus ""
  putStrLn $ "Result: " ++ show result
  
-- 测试空baseType的情况
testEmptyBaseType :: IO ()
testEmptyBaseType = do
  putStrLn "Testing empty baseType..."
  let typeExpr = "type " ++ "" ++ "Refined = " ++ "" ++ " where { self > 0 }"
  putStrLn $ "Type expression: " ++ show typeExpr
  putStrLn $ "Has 'type ' prefix: " ++ show ("type " `isPrefixOf` typeExpr)
  putStrLn $ "Has ' = ' infix: " ++ show (" = " `isInfixOf` typeExpr)
  putStrLn $ "Has ' where ' infix: " ++ show (" where " `isInfixOf` typeExpr)
  putStrLn $ "Has ' =  where' infix: " ++ show (" =  where" `isInfixOf` typeExpr)
  putStrLn $ "Has 'type  ' infix: " ++ show ("type  " `isInfixOf` typeExpr)
  let hasMalformedTypeDef = "type " `isPrefixOf` typeExpr && 
                           " = " `isInfixOf` typeExpr && 
                           " where " `isInfixOf` typeExpr &&
                           (" =  where" `isInfixOf` typeExpr || "type  " `isInfixOf` typeExpr)
  putStrLn $ "Has malformed type def: " ++ show hasMalformedTypeDef
  let result = parseTypus typeExpr
  putStrLn $ "Result: " ++ show result

-- 测试Parser performance测试中的有效代码
testValidCode :: IO ()
testValidCode = do
  putStrLn "\nTesting valid code from Parser performance test..."
  let validCode = "//! dependent_types: on\n//! ownership: on\n\ntype Vector[n: int] struct {\n    data [n]float64\n}\n\nfunc zeros(n: Positive) -> Vector[n] {\n    return Vector[n]{data: make([]float64, n)}\n}\n\nfunc main() {\n    v := zeros(10)\n    fmt.Println(v)\n}"
  putStrLn $ "Has '[ : ' infix: " ++ show ("[ : " `isInfixOf` validCode)
  putStrLn $ "Has ':  ' infix: " ++ show (":  " `isInfixOf` validCode)
  putStrLn $ "Has '[]' infix: " ++ show ("[]" `isInfixOf` validCode)
  putStrLn $ "Has 'func ()' infix: " ++ show ("func ()" `isInfixOf` validCode)
  putStrLn $ "Has 'type  struct' infix: " ++ show ("type  struct" `isInfixOf` validCode)
  putStrLn $ "Has 'type  interface' infix: " ++ show ("type  interface" `isInfixOf` validCode)
  let result = parseTypus validCode
  putStrLn $ "Result: " ++ show result

main :: IO ()
main = do
  testEmptyString
  testEmptyBaseType
  testValidCode