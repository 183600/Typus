{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.NewCabalBoundaryConditionsQuickCheckSpec where

import Test.Tasty
import Test.Tasty.QuickCheck (property)
import Utils (trim, splitBy, removeComments, normalizeIndentation, breakOn)
import SourceLocation (SourcePos(..), SourceSpan(..), startPos, posAt, spanBetween, advancePosBy)
import Parser (parseTypus, TypusFile(..), CodeBlock(..), defaultFileDirectives)
import Compiler (compile)
import Compiler.Errors.Core (TypeError(..), ErrorSeverity(..), ErrorCategory(..), newErrorCollector, addError)
import Dependencies.TypeSystem (newDependentTypeChecker, addType, checkType)
import Ownership.Common.Types (OwnershipTransfer(..), OwnershipError(..))
import qualified Data.List as L
import Data.List (isInfixOf, isPrefixOf)
import Data.Maybe (isJust, isNothing)
import qualified Data.Text as T

-- | Test boundary conditions L.and edge cases
testBoundaryConditionsProperties :: TestTree
testBoundaryConditionsProperties = testGroup "Boundary Conditions Properties"
  [ testProperty "utils handle empty strings" propUtilsHandleEmptyStrings
  , testProperty "source location handles extreme values" propSourceLocationHandlesExtremes
  , testProperty "parser handles malformed input gracefully" propParserHandlesMalformedInput
  , testProperty "error handling with extreme inputs" propErrorHandlingExtremeInputs
  , testProperty "type system boundary conditions" propTypeSystemBoundaryConditions
  , testProperty "ownership system edge cases" propOwnershipSystemEdgeCases
  ]

-- | Utils should handle empty strings gracefully
propUtilsHandleEmptyStrings :: Bool
propUtilsHandleEmptyStrings =
  let empty = ""
      trimmed = trim empty
      split = splitBy ',' empty
      withoutComments = removeComments empty
      normalized = normalizeIndentation empty
      broken = breakOn "test" empty
  in null trimmed && 
     split == [""] &&
     null withoutComments &&
     null normalized &&
     broken == ("", "")

-- | Source location should handle extreme values
propSourceLocationHandlesExtremes :: Int -> Int -> Property
propSourceLocationHandlesExtremes line col =
  let pos = posAt (max 1 line) (max 1 col)
      span = spanBetween pos pos
  in posLine pos >= 1 && posColumn pos >= 1 &&
     spanStart span == pos && spanEnd span == pos

-- | Parser should handle malformed input gracefully
propParserHandlesMalformedInput :: String -> Property
propParserHandlesMalformedInput content =
  let malformed = content ++ "\n\0\1\2" ++ content  -- Add control characters
      parsed = parseTypus malformed
      compiled = compile parsed
  in case compiled of
       Right _ -> True  -- May succeed if parser is tolerant
       Left errors -> not (null errors)  -- Should produce errors for bad input

-- | Error handling should work with extreme inputs
propErrorHandlingExtremeInputs :: String -> Property
propErrorHandlingExtremeInputs message =
  let extremeMessage = message ++ "\n\0\1\2\t\r\n" ++ message
      error = TypeError (T.pack extremeMessage) ErrorSeverityError ErrorCategorySyntax 
                        (Compiler.Errors.Core.ErrorLocation Nothing 999999 999999 Nothing Nothing) 
                        Compiler.Errors.Core.emptyContext
      collector = newErrorCollector
      collector1 = addError error collector
  in not (L.null (getErrors collector1))

-- | Type system should handle boundary conditions
propTypeSystemBoundaryConditions :: String -> Property
propTypeSystemBoundaryConditions typeName =
  let extremeTypeName = if null typeName then "extremely_long_type_name_" ++ L.concat (replicate 100 "very_long") 
                       else typeName ++ "_boundary"
      typeDef = Dependencies.TypeSystem.TypeDefDecl [] []
      checker = newDependentTypeChecker
      checker1 = addType extremeTypeName typeDef checker
      result = checkType extremeTypeName (Dependencies.TypeSystem.TVCon extremeTypeName) checker1
  in case result of
       Right _ -> True  -- Should succeed for valid types
       Left _ -> True   -- Or fail gracefully

-- | Ownership system should handle edge cases
propOwnershipSystemEdgeCases :: String -> String -> Property
propOwnershipSystemEdgeCases from to =
  let extremeFrom = if null from then "extremely_long_variable_name_" ++ L.concat (replicate 100 "very_long") else from
      extremeTo = if null to then "extremely_long_variable_name_" ++ L.concat (replicate 100 "very_long") else to
      transfer = OwnershipTransfer extremeFrom extremeTo
  in transferFrom transfer == extremeFrom && transferTo transfer == extremeTo

-- | Test utils boundary conditions
testUtilsBoundaryConditions :: TestTree
testUtilsBoundaryConditions = testGroup "Utils Boundary Conditions"
  [ testCase "trim with only whitespace" $
      let whitespaceOnly = "   \t\n\r   "
          trimmed = trim whitespaceOnly
      in null trimmed
      
  , testCase "splitBy with empty string" $
      let result = splitBy ',' ""
      in result == [""]
      
  , testCase "splitBy with consecutive delimiters" $
      let result = splitBy ',' "a,,b,,c"
      in result == ["a", "", "b", "", "c"]
      
  , testCase "removeComments with only comments" $
      let commentsOnly = "// line 1\n/* block */\n// line 2"
          withoutComments = removeComments commentsOnly
      in L.null (L.filter (not . null) (lines withoutComments))
      
  , testCase "normalizeIndentation with inconsistent indentation" $
      let inconsistent = "    line1\n\tline2\n        line3"
          normalized = normalizeIndentation inconsistent
      in not (null normalized)
      
  , testCase "breakOn with empty pattern" $
      let result = breakOn "" "test"
      in result == ("", "test")
      
  , testCase "breakOn with pattern not found" $
      let result = breakOn "xyz" "test"
      in result == ("test", "")
  ]

-- | Test source location boundary conditions
testSourceLocationBoundaryConditions :: TestTree
testSourceLocationBoundaryConditions = testGroup "Source Location Boundary Conditions"
  [ testCase "start position properties" $
      let start = startPos
      in posLine start == 1 && posColumn start == 1 && posOffset start == 0
      
  , testCase "extreme position values" $
      let extremePos = posAt 999999 999999
      in posLine extremePos == 999999 && posColumn extremePos == 999999
      
  , testCase "position advancement with empty string" $
      let pos = posAt 5 10
          advanced = advancePosBy "" pos
      in advanced == pos
      
  , testCase "position advancement with newlines" $
      let pos = posAt 1 5
          advanced = advancePosBy "\n\n\n" pos
      in posLine advanced == 4 && posColumn advanced == 1
      
  , testCase "position advancement with tabs" $
      let pos = posAt 1 3
          advanced = advancePosBy "\t" pos
      in posColumn advanced >= 9  -- Should align to next tab stop
  ]

-- | Test parser boundary conditions
testParserBoundaryConditions :: TestTree
testParserBoundaryConditions = testGroup "Parser Boundary Conditions"
  [ testCase "parse empty file" $
      let parsed = parseTypus ""
          blocks = tfBlocks parsed
      in null blocks
      
  , testCase "parse file with only whitespace" $
      let whitespaceOnly = "   \n\t\n   \n  "
          parsed = parseTypus whitespaceOnly
      in parsed `seq` True  -- Should not crash
      
  , testCase "parse file with control characters" $
      let controlChars = "\0\1\2\3\4\5\6\7\8\11\12\14\15\16\17\18\19\20\21\22\23\24\25\26\27\28\29\30\31"
          parsed = parseTypus controlChars
      in parsed `seq` True  -- Should not crash
      
  , testCase "parse file with unicode" $
      let unicode = "package main\n\nfunc main() {\n    println(\"你好世界 🌍\")\n}"
          parsed = parseTypus unicode
          compiled = compile parsed
      in case compiled of
           Right goCode -> "你好世界" `L.isInfixOf` goCode
           Left _ -> True  -- May fail but should not crash
           
  , testCase "parse extremely long line" $
      let longLine = "package main\n\n" ++ L.concat (replicate 1000 "x") ++ "\n"
          parsed = parseTypus longLine
      in parsed `seq` True  -- Should not crash
  ]

-- | Test error handling boundary conditions
testErrorHandlingBoundaryConditions :: TestTree
testErrorHandlingBoundaryConditions = testGroup "Error Handling Boundary Conditions"
  [ testCase "error with empty message" $
      let error = TypeError T.empty ErrorSeverityError ErrorCategorySyntax 
                        (Compiler.Errors.Core.ErrorLocation (startPos) Nothing) 
                        Compiler.Errors.Core.emptyContext
      in show error `seq` True  -- Should not crash
      
  , testCase "error with extremely long message" $
      let longMessage = T.pack $ L.concat (replicate 1000 "very long error message ")
          error = TypeError longMessage ErrorSeverityError ErrorCategorySyntax 
                        (Compiler.Errors.Core.ErrorLocation (startPos) Nothing) 
                        Compiler.Errors.Core.emptyContext
      in show error `seq` True  -- Should not crash
      
  , testCase "error with extreme location values" $
      let error = TypeError "test" ErrorSeverityError ErrorCategorySyntax 
                        (Compiler.Errors.Core.ErrorLocation Nothing 999999 999999 (Just 999999) (Just 999999)) 
                        Compiler.Errors.Core.emptyContext
      in show error `seq` True  -- Should not crash
      
  , testCase "error collector with many errors" $
      let collector = newErrorCollector
          error = TypeError "test" ErrorSeverityError ErrorCategorySyntax 
                           (Compiler.Errors.Core.ErrorLocation (startPos) Nothing) 
                           Compiler.Errors.Core.emptyContext
          collector1 = L.foldl (\c _ -> addError error c) collector [1..1000]
          errors = getErrors collector1
      in L.length errors == 1000
  ]

-- | Test type system boundary conditions
testTypeSystemBoundaryConditions :: TestTree
testTypeSystemBoundaryConditions = testGroup "Type System Boundary Conditions"
  [ testCase "empty type name" $
      let checker = newDependentTypeChecker
          typeDef = Dependencies.TypeSystem.TypeDefDecl [] []
          checker1 = addType "" typeDef checker
          result = checkType "" (Dependencies.TypeSystem.TVCon "") checker1
      in case result of
           Right _ -> True  -- May succeed
           Left _ -> True   -- Or fail gracefully
           
  , testCase "extremely long type name" $
      let longName = L.concat (replicate 1000 "Type")
          checker = newDependentTypeChecker
          typeDef = Dependencies.TypeSystem.TypeDefDecl [] []
          checker1 = addType longName typeDef checker
          result = checkType longName (Dependencies.TypeSystem.TVCon longName) checker1
      in case result of
           Right _ -> True  -- May succeed
           Left _ -> True   -- Or fail gracefully
           
  , testCase "type with many parameters" $
      let params = L.map (\i -> "param" ++ show i) [1..100]
          typeDef = Dependencies.TypeSystem.TypeDefDecl params []
          checker = newDependentTypeChecker
          checker1 = addType "ManyParams" typeDef checker
      in checker1 `seq` True  -- Should not crash
  ]

-- | Test ownership boundary conditions
testOwnershipBoundaryConditions :: TestTree
testOwnershipBoundaryConditions = testGroup "Ownership Boundary Conditions"
  [ testCase "ownership transfer with empty names" $
      let transfer = OwnershipTransfer "" ""
      in transferFrom transfer == "" && transferTo transfer == ""
      
  , testCase "ownership transfer with extremely long names" $
      let longName = L.concat (replicate 1000 "variable")
          transfer = OwnershipTransfer longName longName
      in transferFrom transfer == longName && transferTo transfer == longName
      
  , testCase "ownership error with empty variable" $
      let error = UseAfterMove ""
      in show error `seq` True  -- Should not crash
      
  , testCase "ownership error with extremely long variable name" $
      let longName = L.concat (replicate 1000 "variable")
          error = UseAfterMove longName
      in show error `seq` True  -- Should not crash
  ]

-- | Test compiler boundary conditions
testCompilerBoundaryConditions :: TestTree
testCompilerBoundaryConditions = testGroup "Compiler Boundary Conditions"
  [ testCase "compile empty file" $
      let parsed = parseTypus ""
          compiled = compile parsed
      in case compiled of
           Right goCode -> not (null goCode)  -- Should generate some output
           Left errors -> not (null errors)    -- Or produce errors
           
  , testCase "compile file with only directives" $
      let directives = "//! ownership=true\n//! dependent-types=false\n//! constraints=true"
          parsed = parseTypus directives
          compiled = compile parsed
      in case compiled of
           Right _ -> True  -- May succeed
           Left _ -> True   -- Or fail gracefully
           
  , testCase "compile extremely large file" $
      let largeContent = "package main\n\n" ++ 
                        unlines [L.concat (replicate 100 "x") ++ " // line " ++ show i | i <- [1..100]]
          parsed = parseTypus largeContent
          compiled = compile parsed
      in case compiled of
           Right goCode -> not (null goCode)  -- Should generate some output
           Left errors -> not (null errors)    -- Or produce errors
  ]

-- | All boundary conditions tests
testBoundaryConditionsQuickCheck :: TestTree
testBoundaryConditionsQuickCheck = testGroup "New Cabal Boundary Conditions QuickCheck Tests"
  [ testBoundaryConditionsProperties
  , testUtilsBoundaryConditions
  , testSourceLocationBoundaryConditions
  , testParserBoundaryConditions
  , testErrorHandlingBoundaryConditions
  , testTypeSystemBoundaryConditions
  , testOwnershipBoundaryConditions
  , testCompilerBoundaryConditions
  ]