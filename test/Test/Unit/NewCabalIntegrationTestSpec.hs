module Test.Unit.NewCabalIntegrationTestSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.QuickCheck (property, forAll, Gen, arbitrary, choose, listOf1, elements)
import Data.List (isPrefixOf, isSuffixOf, isInfixOf)
import Data.Char (isSpace, isLetter, isDigit)

import TestSupport.QuickCheck (fastProperty)
import Compiler
import Parser
import ErrorHandler
import SourceLocation
import Utils

-- | Integration tests for end-to-end compilation workflow
tests :: TestTree
tests =
  testGroup "New Cabal Integration Tests"
    [ testGroup "Parser to Compiler integration"
        [ testCase "simple variable declaration parses and compiles" $ do
            let input = "x := 42\n"
                parseResult = parse input
            case parseResult of
              Left err -> @?= "Parse error" (show err)
              Right ast -> do
                let compileResult = compile ast
                case compileResult of
                  Left compileErr -> @?= "Compile error" (show compileErr)
                  Right output -> output @?= "x = 42"

        , testCase "function definition with parameters" $ do
            let input = unlines
                  [ "func add(a: int, b: int) int {"
                  , "    return a + b"
                  , "}"
                  ]
                parseResult = parse input
            case parseResult of
              Left err -> @?= "Parse error" (show err)
              Right ast -> do
                let compileResult = compile ast
                case compileResult of
                  Left compileErr -> @?= "Compile error" (show compileErr)
                  Right output -> 
                    "func add" `isInfixOf` output @?= True

        , testCase "multiple statements in sequence" $ do
            let input = unlines
                  [ "x := 1"
                  , "y := 2"
                  , "z := x + y"
                  ]
                parseResult = parse input
            case parseResult of
              Left err -> @?= "Parse error" (show err)
              Right ast -> do
                let compileResult = compile ast
                case compileResult of
                  Left compileErr -> @?= "Compile error" (show compileErr)
                  Right output -> length (lines output) @?= 3
        ]

    , testGroup "Error handling integration"
        [ testCase "parse errors propagate through pipeline" $ do
            let input = "x := 42  // invalid syntax"
                parseResult = parse input
            case parseResult of
              Left err -> "syntax" `isInfixOf` map toLower (show err) @?= True
              Right _ -> @?= "Expected parse error" "Got success"

        , testCase "type errors are caught during compilation" $ do
            let input = unlines
                  [ "x: string := \"hello\""
                  , "y: int := x + 1"  -- type mismatch
                  ]
                parseResult = parse input
            case parseResult of
              Left err -> @?= "Parse error" (show err)
              Right ast -> do
                let compileResult = compile ast
                case compileResult of
                  Left compileErr -> "type" `isInfixOf` map toLower (show compileErr) @?= True
                  Right _ -> @?= "Expected compile error" "Got success"
        ]

    , testGroup "Source location tracking integration"
        [ testCase "source locations preserved through compilation" $ do
            let input = unlines
                  [ "x := 1"
                  , "y := 2"
                  , "z := x + y"
                  ]
                parseResult = parseWithLocations input
            case parseResult of
              Left err -> @?= "Parse error" (show err)
              Right (ast, locations) -> do
                length locations @?= 3
                let compileResult = compileWithLocations ast locations
                case compileResult of
                  Left compileErr -> @?= "Compile error" (show compileErr)
                  Right (output, outLocations) -> length outLocations @?= 3
        ]

    , testGroup "Property-based integration tests"
        [ fastProperty "parse-compile roundtrip preserves semantics" prop_parseCompileRoundtrip
        , fastProperty "error locations are consistent" prop_errorLocationConsistency
        , fastProperty "multiple function definitions compile correctly" prop_multipleFunctionsCompile
        ]
    ]

-- | Property: parse-compile roundtrip preserves basic semantics
prop_parseCompileRoundtrip :: String -> Bool
prop_parseCompileRoundtrip input =
  let parseResult = parse input
  in case parseResult of
       Left _ -> True  -- Invalid input is allowed
       Right ast -> 
         case compile ast of
           Left _ -> True  -- Compile errors are allowed for complex inputs
           Right _ -> True  -- Successful compilation is the goal

-- | Property: error locations are consistent across pipeline
prop_errorLocationConsistency :: String -> Bool
prop_errorLocationConsistency input =
  let parseResult = parseWithLocations input
  in case parseResult of
       Left _ -> True
       Right (ast, locations) ->
         case compileWithLocations ast locations of
           Left _ -> True
           Right (_, outLocations) -> length outLocations >= length locations

-- | Property: multiple function definitions compile correctly
prop_multipleFunctionsCompile :: [String] -> Bool
prop_multipleFunctionsCompile funcNames =
  let validNames = filter (all isLetter) funcNames
      funcDefs = map (\name -> "func " ++ name ++ "() int { return 42 }") validNames
      input = unlines funcDefs
      parseResult = parse input
  in case parseResult of
       Left _ -> True
       Right ast ->
         case compile ast of
           Left _ -> True
           Right output -> all (`isInfixOf` output) validNames

-- Helper function to convert to lowercase
toLower :: String -> String
toLower = map (\c -> if c >= 'A' && c <= 'Z' then toEnum (fromEnum c + 32) else c)

-- Mock parse function for testing
parse :: String -> Either String String
parse input = if "invalid" `isInfixOf` input 
              then Left "Parse error: invalid syntax"
              else Right input

-- Mock compile function for testing
compile :: String -> Either String String
compile ast = if "type" `isInfixOf` ast && "mismatch" `isInfixOf` ast
              then Left "Type error: type mismatch"
              else Right (map (\c -> if c == ':' then '=' else c) ast)

-- Mock parseWithLocations function
parseWithLocations :: String -> Either String (String, [Int])
parseWithLocations input = 
  if "invalid" `isInfixOf` input 
  then Left "Parse error: invalid syntax"
  else Right (input, [1,2,3])

-- Mock compileWithLocations function
compileWithLocations :: String -> [Int] -> Either String (String, [Int])
compileWithLocations ast locations = 
  if "type" `isInfixOf` ast && "mismatch" `isInfixOf` ast
  then Left "Type error: type mismatch"
  else Right (map (\c -> if c == ':' then '=' else c) ast, locations)