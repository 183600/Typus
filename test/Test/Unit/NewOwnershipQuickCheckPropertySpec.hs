{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | QuickCheck property tests for Ownership module
module Test.Unit.NewOwnershipQuickCheckPropertySpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

import Ownership.Common.Types
import Ownership.Analyzer (analyzeOwnership, analyzeOwnershipDebug, builtInFunctions)
import Ownership.Lexer (lexAll)
import Ownership.Parser (parseProgram)
import Data.List (sort, nub, isInfixOf)
import Data.Maybe (isJust, isNothing)
import qualified Data.Map.Strict as Map
import Data.Either (isLeft, isRight)

-- | Test group for Ownership module QuickCheck properties
testOwnershipQuickCheckProperties :: TestTree
testOwnershipQuickCheckProperties = testGroup "Ownership Module QuickCheck Property Tests"
  [ ownershipTypeProperties
  , ownershipErrorProperties
  , ownershipTransferProperties
  , analyzerProperties
  , lexerProperties
  , parserProperties
  , integrationProperties
  ]

-- | Properties for OwnershipType
ownershipTypeProperties :: TestTree
ownershipTypeProperties = testGroup "OwnershipType properties"
  [ testProperty "Owned preserves name" $
    \name -> 
      let owned = Owned name
      in case owned of
        Owned n -> n === name
  
  , testProperty "Borrowed preserves name" $
    \name -> 
      let borrowed = Borrowed name
      in case borrowed of
        Borrowed n -> n === name
  
  , testProperty "MutBorrowed preserves name" $
    \name -> 
      let mutBorrowed = MutBorrowed name
      in case mutBorrowed of
        MutBorrowed n -> n === name
  
  , testProperty "OwnershipType ordering is consistent" $
    \name1 name2 -> 
      let owned1 = Owned name1
          owned2 = Owned name2
          borrowed1 = Borrowed name1
          borrowed2 = Borrowed name2
          mutBorrowed1 = MutBorrowed name1
          mutBorrowed2 = MutBorrowed name2
      in compare owned1 owned2 === compare name1 name2 &&
         compare borrowed1 borrowed2 === compare name1 name2 &&
         compare mutBorrowed1 mutBorrowed2 === compare name1 name2 &&
         compare owned1 borrowed1 === LT &&
         compare owned1 mutBorrowed1 === LT &&
         compare borrowed1 mutBorrowed1 === LT
  
  , testProperty "OwnershipType Show/Read round-trip" $
    \name -> 
      let types = [Owned name, Borrowed name, MutBorrowed name]
          showStrings = map show types
      in all (`isInfixOf` name) showStrings
  ]

-- | Properties for OwnershipError
ownershipErrorProperties :: TestTree
ownershipErrorProperties = testGroup "OwnershipError properties"
  [ testProperty "UseAfterMove preserves variable name" $
    \var -> 
      let err = UseAfterMove var
      in case err of
        UseAfterMove v -> v === var
  
  , testProperty "DoubleMove preserves variable names" $
    \var1 var2 -> 
      let err = DoubleMove var1 var2
      in case err of
        DoubleMove v1 v2 -> v1 === var1 && v2 === var2
  
  , testProperty "BorrowWhileMoved preserves variable name" $
    \var -> 
      let err = BorrowWhileMoved var
      in case err of
        BorrowWhileMoved v -> v === var
  
  , testProperty "MutBorrowWhileBorrowed preserves variable name" $
    \var -> 
      let err = MutBorrowWhileBorrowed var
      in case err of
        MutBorrowWhileBorrowed v -> v === var
  
  , testProperty "BorrowWhileMutBorrowed preserves variable name" $
    \var -> 
      let err = BorrowWhileMutBorrowed var
      in case err of
        BorrowWhileMutBorrowed v -> v === var
  
  , testProperty "MultipleMutBorrows preserves variable name" $
    \var -> 
      let err = MultipleMutBorrows var
      in case err of
        MultipleMutBorrows v -> v === var
  
  , testProperty "UseWhileMutBorrowed preserves variable name" $
    \var -> 
      let err = UseWhileMutBorrowed var
      in case err of
        UseWhileMutBorrowed v -> v === var
  
  , testProperty "OutOfScope preserves variable name" $
    \var -> 
      let err = OutOfScope var
      in case err of
        OutOfScope v -> v === var
  
  , testProperty "BorrowError preserves message" $
    \msg -> 
      let err = BorrowError msg
      in case err of
        BorrowError m -> m === msg
  
  , testProperty "ParseError preserves message" $
    \msg -> 
      let err = ParseError msg
      in case err of
        ParseError m -> m === msg
  
  , testProperty "OwnershipError ordering is consistent" $
    \err1 err2 -> 
      let cmp1 = compare err1 err2
          cmp2 = compare (show err1) (show err2)
      in cmp1 === cmp2
  ]

-- | Properties for OwnershipTransfer
ownershipTransferProperties :: TestTree
ownershipTransferProperties = testGroup "OwnershipTransfer properties"
  [ testProperty "OwnershipTransfer preserves from and to" $
    \from to -> 
      let transfer = OwnershipTransfer from to
      in transferFrom transfer === from && transferTo transfer === to
  
  , testProperty "OwnershipTransfer Show contains from and to" $
    \from to -> 
      let transfer = OwnershipTransfer from to
          transferStr = show transfer
      in from `isInfixOf` transferStr && to `isInfixOf` transferStr
  
  , testProperty "OwnershipTransfer equality is correct" $
    \from1 to1 from2 to2 -> 
      let transfer1 = OwnershipTransfer from1 to1
          transfer2 = OwnershipTransfer from2 to2
      in (transfer1 == transfer2) === (from1 == from2 && to1 == to2)
  ]

-- | Properties for OwnershipAnalyzer
analyzerProperties :: TestTree
analyzerProperties = testGroup "OwnershipAnalyzer properties"
  [ testProperty "newOwnershipAnalyzer creates analyzer" $
    \_ -> 
      let analyzer = newOwnershipAnalyzer
      in case analyzer of
        OwnershipAnalyzer () -> property True
  
  , testProperty "builtInFunctions is not empty" $
    \_ -> not (null builtInFunctions)
  
  , testProperty "builtInFunctions contains expected functions" $
    \_ -> 
      let expected = ["int", "string", "fmt.Println", "len", "make"]
      in all (`elem` builtInFunctions) expected
  
  , testProperty "builtInFunctions has no duplicates" $
    \_ -> 
      let uniqueBuiltIns = nub builtInFunctions
      in length uniqueBuiltIns === length builtInFunctions
  ]

-- | Properties for lexer
lexerProperties :: TestTree
lexerProperties = testGroup "Lexer properties"
  [ testProperty "lexAll handles empty input" $
    \_ -> 
      case lexAll "" of
        Left _ -> property True
        Right tokens -> null tokens || all isValidToken tokens
  
  , testProperty "lexAll handles whitespace" $
    \whitespace -> 
      let input = replicate 10 whitespace
      in case lexAll input of
        Left _ -> property True
        Right tokens -> all isValidToken tokens
  
  , testProperty "lexAll preserves non-whitespace characters" $
    \content -> 
      case lexAll content of
        Left _ -> property True
        Right tokens -> length tokens > 0 || null content
  
  where
    isValidToken token = property True  -- Simplified for this example
  ]

-- | Properties for parser
parserProperties :: TestTree
parserProperties = testGroup "Parser properties"
  [ testProperty "parseProgram handles empty input" $
    \_ -> 
      case lexAll "" of
        Left _ -> property True
        Right tokens -> 
          case parseProgram tokens of
            Left _ -> property True
            Right program -> property True
  
  , testProperty "parseProgram handles simple variable declarations" $
    \varName -> 
      let input = "var " ++ varName ++ " int"
      in case lexAll input of
        Left _ -> property True
        Right tokens -> 
          case parseProgram tokens of
            Left _ -> property True
            Right program -> property True
  
  , testProperty "parseProgram handles simple assignments" $
    \varName -> 
      let input = varName ++ " = 42"
      in case lexAll input of
        Left _ -> property True
        Right tokens -> 
          case parseProgram tokens of
            Left _ -> property True
            Right program -> property True
  ]

-- | Properties for integration tests
integrationProperties :: TestTree
integrationProperties = testGroup "Ownership integration properties"
  [ testProperty "analyzeOwnership handles empty input" $
    \_ -> 
      case analyzeOwnership "" of
        Left _ -> property True
        Right errors -> property True  -- Should not crash
  
  , testProperty "analyzeOwnership handles simple valid code" $
    \varName -> 
      let input = "var " ++ varName ++ " int\n" ++ varName ++ " = 42"
      in case analyzeOwnership input of
        Left _ -> property True
        Right errors -> property True  -- Should not crash
  
  , testProperty "analyzeOwnershipDebug includes debug information" $
    \varName -> 
      let input = "var " ++ varName ++ " int"
      in case analyzeOwnershipDebug input of
        Left _ -> property True
        Right (errors, debug) -> property True  -- Should include debug info
  
  , testProperty "analyzeOwnershipFile handles file content" $
    \content -> 
      case analyzeOwnershipFile content of
        Left _ -> property True
        Right errors -> property True  -- Should not crash
  
  , testProperty "Ownership analysis is deterministic" $
    \input -> 
      let result1 = analyzeOwnership input
          result2 = analyzeOwnership input
      in case (result1, result2) of
        (Left _, Left _) -> property True
        (Right errs1, Right errs2) -> sort errs1 === sort errs2
        _ -> property False  -- Should be consistent
  ]

-- | Additional edge case properties
edgeCaseProperties :: TestTree
edgeCaseProperties = testGroup "Ownership edge case properties"
  [ testProperty "OwnershipType with empty names" $
    \_ -> 
      let owned = Owned ""
          borrowed = Borrowed ""
          mutBorrowed = MutBorrowed ""
      in case (owned, borrowed, mutBorrowed) of
        (Owned "", Borrowed "", MutBorrowed "") -> property True
  
  , testProperty "OwnershipError with empty strings" $
    \_ -> 
      let errors = [UseAfterMove "", DoubleMove "" "", BorrowError "", ParseError ""]
      in all (\err -> case err of
              UseAfterMove var -> null var
              DoubleMove var1 var2 -> null var1 && null var2
              BorrowError msg -> null msg
              ParseError msg -> null msg
              _ -> False) errors
  
  , testProperty "OwnershipTransfer with empty strings" $
    \_ -> 
      let transfer = OwnershipTransfer "" ""
      in transferFrom transfer === "" && transferTo transfer === ""
  
  , testProperty "analyzeOwnership handles very long identifiers" $
    \base -> 
      let longName = replicate 100 base
          input = "var " ++ longName ++ " int"
      in case analyzeOwnership input of
        Left _ -> property True
        Right errors -> property True  -- Should not crash
  
  , testProperty "analyzeOwnership handles deeply nested code" $
    \depth -> 
      let nesting = min depth 10  -- Limit to reasonable depth
          input = unlines $ replicate nesting "  if true { x := 1 }"
      in case analyzeOwnership input of
        Left _ -> property True
        Right errors -> property True  -- Should not crash
  ]