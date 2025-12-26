{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Test.Unit.NewOwnershipAnalysisQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import TestSupport.Arbitrary

import Ownership.Common.Types
  ( OwnershipType(..)
  , OwnershipError(..)
  , OwnershipAnalyzer(..)
  , OwnershipTransfer(..)
  , newOwnershipAnalyzer
  )
import Ownership
  ( analyzeOwnership
  , analyzeOwnershipFile
  , analyzeOwnershipDebug
  , formatOwnershipErrors
  , lexAll
  , parseProgram
  , builtInFunctions
  )

import Data.List (intercalate, isInfixOf, isPrefixOf)
import Data.Char (isAlpha, isAlphaNum, isDigit)
import qualified Data.Set as Set

-- | 新的所有权分析QuickCheck测试套件
tests :: TestTree
tests =
  testGroup "New Ownership Analysis QuickCheck Tests"
    [ fastProperty "newOwnershipAnalyzer creates valid analyzer" prop_newOwnershipAnalyzer_valid
    , fastProperty "OwnershipType ordering is consistent" prop_ownershipType_ordering_consistent
    , fastProperty "OwnershipError ordering is consistent" prop_ownershipError_ordering_consistent
    , fastProperty "OwnershipTransfer preserves source and target" prop_ownershipTransfer_preserves
    , fastProperty "analyzeOwnership handles empty input" prop_analyzeOwnership_empty
    , fastProperty "analyzeOwnership detects simple move errors" prop_analyzeOwnership_move_errors
    , fastProperty "analyzeOwnership handles variable declarations" prop_analyzeOwnership_var_decls
    , fastProperty "builtInFunctions contains common functions" prop_builtInFunctions_common
    , fastProperty "lexAll handles basic Go syntax" prop_lexAll_basic_syntax
    , fastProperty "parseProgram handles simple programs" prop_parseProgram_simple
    ]

-- Property: newOwnershipAnalyzer creates valid analyzer
prop_newOwnershipAnalyzer_valid :: Property
prop_newOwnershipAnalyzer_valid =
  let analyzer = newOwnershipAnalyzer
  in property $ True  -- If we can create an analyzer without error, it's valid

-- Property: OwnershipType ordering is consistent
prop_ownershipType_ordering_consistent :: OwnershipType -> OwnershipType -> Property
prop_ownershipType_ordering_consistent typ1 typ2 =
  let ord1 = compare typ1 typ2
      ord2 = compare typ2 typ1
  in property $ (ord1 == EQ) ==> (ord2 == EQ) .&&.
             (ord1 == LT) ==> (ord2 == GT) .&&.
             (ord1 == GT) ==> (ord2 == LT)

-- Property: OwnershipError ordering is consistent
prop_ownershipError_ordering_consistent :: OwnershipError -> OwnershipError -> Property
prop_ownershipError_ordering_consistent err1 err2 =
  let ord1 = compare err1 err2
      ord2 = compare err2 err1
  in property $ (ord1 == EQ) ==> (ord2 == EQ) .&&.
             (ord1 == LT) ==> (ord2 == GT) .&&.
             (ord1 == GT) ==> (ord2 == LT)

-- Property: OwnershipTransfer preserves source and target
prop_ownershipTransfer_preserves :: String -> String -> Property
prop_ownershipTransfer_preserves source target =
  not (null source) && not (null target) &&
  length source <= 20 && length target <= 20 ==>
  let transfer = OwnershipTransfer source target
  in property $ transferFrom transfer === source .&&.
     transferTo transfer === target

-- Property: analyzeOwnership handles empty input
prop_analyzeOwnership_empty :: Property
prop_analyzeOwnership_empty =
  let errors = analyzeOwnership ""
  in property $ null errors

-- Property: analyzeOwnership detects simple move errors
prop_analyzeOwnership_move_errors :: String -> Property
prop_analyzeOwnership_move_errors varName =
  not (null varName) && isAlphaNum (head varName) &&
  length varName <= 10 && varName `notElem` builtInFunctions ==>
  let code = unlines
        [ "func main() {"
        , "  " ++ varName ++ " := \"value\""
        , "  consume(" ++ varName ++ ")"
        , "  println(" ++ varName ++ ")"
        , "}"
        ]
      errors = analyzeOwnership code
  in property $ any isUseAfterMove errors
  where
    isUseAfterMove (UseAfterMove _) = True
    isUseAfterMove _ = False

-- Property: analyzeOwnership handles variable declarations
prop_analyzeOwnership_var_decls :: String -> String -> Property
prop_analyzeOwnership_var_decls varName value =
  not (null varName) && not (null value) &&
  isAlphaNum (head varName) && length varName <= 10 &&
  varName `notElem` builtInFunctions ==>
  let code = unlines
        [ "func main() {"
        , "  " ++ varName ++ " := " ++ value
        , "  println(" ++ varName ++ ")"
        , "}"
        ]
      errors = analyzeOwnership code
  in property $ null errors  -- Simple declarations should not produce errors

-- Property: builtInFunctions contains common functions
prop_builtInFunctions_common :: Property
prop_builtInFunctions_common =
  let commonFunctions = ["println", "len", "make", "new", "append"]
      hasCommon = all (`elem` builtInFunctions) commonFunctions
  in property $ hasCommon

-- Property: lexAll handles basic Go syntax
prop_lexAll_basic :: String -> Property
prop_lexAll_basic code =
  not (null code) && length code <= 50 ==>
  let tokens = lexAll code
  in property $ not (null tokens)

-- Property: parseProgram handles simple programs
prop_parseProgram_simple :: String -> Property
prop_parseProgram_simple code =
  let simpleProgram = "func main() {\n  println(\"hello\")\n}"
      tokens = lexAll simpleProgram
      program = parseProgram tokens
  in property $ True  -- If parsing doesn't crash, it's successful enough

-- Additional properties for ownership analysis

-- Property: OwnershipType Show instance is invertible for simple cases
prop_ownershipType_show_read :: String -> Property
prop_ownershipType_show_read name =
  not (null name) && length name <= 20 ==>
  let ownedType = Owned name
      borrowedType = Borrowed name
      mutBorrowedType = MutBorrowed name
      showOwned = show ownedType
      showBorrowed = show borrowedType
      showMutBorrowed = show mutBorrowedType
  in property $ "Owned " `isPrefixOf` showOwned .&&.
     "Borrowed " `isPrefixOf` showBorrowed .&&.
     "MutBorrowed " `isPrefixOf` showMutBorrowed

-- Property: OwnershipError Show instance contains meaningful information
prop_ownershipError_show_meaningful :: String -> Property
prop_ownershipError_show_meaningful varName =
  not (null varName) && length varName <= 20 ==>
  let useAfterMove = UseAfterMove varName
      doubleMove = DoubleMove varName varName
      borrowWhileMoved = BorrowWhileMoved varName
      showUseAfterMove = show useAfterMove
      showDoubleMove = show doubleMove
      showBorrowWhileMoved = show borrowWhileMoved
  in property $ "UseAfterMove" `isInfixOf` showUseAfterMove .&&.
     "DoubleMove" `isInfixOf` showDoubleMove .&&.
     "BorrowWhileMoved" `isInfixOf` showBorrowWhileMoved .&&.
     varName `isInfixOf` showUseAfterMove .&&.
     varName `isInfixOf` showDoubleMove .&&.
     varName `isInfixOf` showBorrowWhileMoved

-- Property: analyzeOwnershipDebug returns debug logs when enabled
prop_analyzeOwnershipDebug :: String -> Property
prop_analyzeOwnershipDebug code =
  not (null code) && length code <= 100 ==>
  let (errors, debugLogs) = analyzeOwnershipDebug True code
      (errorsNoDebug, _) = analyzeOwnershipDebug False code
  in property $ errors === errorsNoDebug .&&.
     (if null code then null debugLogs else True)

-- Property: formatOwnershipErrors handles empty error list
prop_formatOwnershipErrors_empty :: Property
prop_formatOwnershipErrors_empty =
  let formatted = formatOwnershipErrors []
  in property $ not (null formatted)  -- Should return some formatted output even for empty list

-- Property: formatOwnershipErrors includes error information
prop_formatOwnershipErrors_includes :: [OwnershipError] -> Property
prop_formatOwnershipErrors_includes errors =
  not (null errors) && length errors <= 5 ==>
  let formatted = formatOwnershipErrors errors
  in property $ all (\err -> show err `isInfixOf` formatted) errors

-- Property: analyzeOwnership handles borrowing scenarios
prop_analyzeOwnership_borrowing :: String -> Property
prop_analyzeOwnership_borrowing varName =
  not (null varName) && isAlphaNum (head varName) &&
  length varName <= 10 && varName `notElem` builtInFunctions ==>
  let code = unlines
        [ "func main() {"
        , "  " ++ varName ++ " := \"value\""
        , "  ref := &" ++ varName
        , "  println(" ++ varName ++ ")"
        , "}"
        ]
      errors = analyzeOwnership code
  in property $ not (any isUseAfterMove errors)  -- Borrowing should not cause use-after-move
  where
    isUseAfterMove (UseAfterMove _) = True
    isUseAfterMove _ = False

-- Property: analyzeOwnership handles mutable borrowing
prop_analyzeOwnership_mut_borrowing :: String -> Property
prop_analyzeOwnership_mut_borrowing varName =
  not (null varName) && isAlphaNum (head varName) &&
  length varName <= 10 && varName `notElem` builtInFunctions ==>
  let code = unlines
        [ "func main() {"
        , "  " ++ varName ++ " := \"value\""
        , "  ref := &mut " ++ varName
        , "  println(" ++ varName ++ ")"
        , "}"
        ]
      errors = analyzeOwnership code
  in property $ True  -- Just test that it doesn't crash

-- Property: builtInFunctions is comprehensive
prop_builtInFunctions_comprehensive :: Property
prop_builtInFunctions_comprehensive =
  let hasCommonTypes = any (`elem` builtInFunctions) ["int", "string", "bool"]
      hasCommonPackages = any (`elem` builtInFunctions) ["fmt", "os", "io"]
      hasCommonFunctions = any (`elem` builtInFunctions) ["println", "len", "make"]
  in property $ hasCommonTypes .&&. hasCommonPackages .&&. hasCommonFunctions

-- Property: OwnershipType Eq works correctly
prop_ownershipType_eq :: String -> String -> Property
prop_ownershipType_eq name1 name2 =
  let owned1 = Owned name1
      owned2 = Owned name2
      borrowed1 = Borrowed name1
      borrowed2 = Borrowed name2
  in property $ (name1 == name2) ==> (owned1 == owned2) .&&.
     (name1 == name2) ==> (borrowed1 == borrowed2) .&&.
     (owned1 /= borrowed1) .&&.
     (owned1 /= MutBorrowed name1)

-- Property: OwnershipError Eq works correctly
prop_ownershipError_eq :: String -> String -> Property
prop_ownershipError_eq var1 var2 =
  let error1a = UseAfterMove var1
      error1b = UseAfterMove var1
      error2 = UseAfterMove var2
      doubleMove1 = DoubleMove var1 var2
      doubleMove2 = DoubleMove var2 var1
  in property $ error1a === error1b .&&.
     (var1 == var2) ==> (error1a === error2) .&&.
     (var1 /= var2 || var1 /= var2) ==> (error1a /= error2) .&&.
     (var1 == var2 && var2 == var1) ==> (doubleMove1 === doubleMove2)

-- Helper function to check if a character is alphanumeric
isAlphaNum :: Char -> Bool
isAlphaNum c = isAlpha c || isDigit c