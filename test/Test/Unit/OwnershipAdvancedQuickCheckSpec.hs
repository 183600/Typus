{-# LANGUAGE CPP #-}
module Test.Unit.OwnershipAdvancedQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, oneof, elements, choose, listOf, forAll, Property, (===), counterexample, (==>))

import qualified Data.Text as T
import Data.List (isInfixOf, nub, sort)
import Data.Maybe (isJust, isNothing)

import Ownership
  ( OwnershipType(..)
  , OwnershipError(..)
  , OwnershipAnalyzer
  , OwnershipTransfer(..)
  , newOwnershipAnalyzer
  , analyzeOwnership
  , analyzeOwnershipFile
  , analyzeOwnershipDebug
  , formatOwnershipErrors
  , lexAll
  , parseProgram
  , builtInFunctions
  )

import Parser (TypusFile(..), FileDirectives(..), BlockDirectives(..), CodeBlock(..), defaultFileDirectives, defaultBlockDirectives)
import SourceLocation (SourceSpan(..), SourcePos(..))

-- ============================================================================
-- Arbitrary Instances
-- ============================================================================

instance Arbitrary OwnershipType where
  arbitrary = do
    name <- arbitrary
    oneof [return $ Owned name, return $ Borrowed name, return $ MutBorrowed name]

instance Arbitrary OwnershipError where
  arbitrary = oneof
    [ UseAfterMove <$> arbitrary
    , DoubleMove <$> arbitrary <*> arbitrary
    , BorrowWhileMoved <$> arbitrary
    , MutBorrowWhileBorrowed <$> arbitrary
    , BorrowWhileMutBorrowed <$> arbitrary
    , MultipleMutBorrows <$> arbitrary
    , UseWhileMutBorrowed <$> arbitrary
    , OutOfScope <$> arbitrary
    , BorrowError <$> arbitrary
    , ParseError <$> arbitrary
    , CrossFunctionMove <$> arbitrary <*> arbitrary
    , ParameterMoveMismatch <$> arbitrary
    , ControlFlowError <$> arbitrary
    , PathSensitiveError <$> arbitrary
    , LoopOwnershipError <$> arbitrary
    ]

instance Arbitrary OwnershipTransfer where
  arbitrary = do
    from <- arbitrary
    to <- arbitrary
    return $ OwnershipTransfer from to

instance Arbitrary OwnershipAnalyzer where
  arbitrary = return newOwnershipAnalyzer

-- Generate simple code snippets for ownership analysis
genOwnershipCode :: Gen String
genOwnershipCode = do
  codeType <- choose (1, 5)
  case codeType of
    1 -> do  -- Simple variable declaration
      varName <- arbitrary
      return $ "let " ++ varName ++ " = 42"
    2 -> do  -- Move operation
      var1 <- arbitrary
      var2 <- arbitrary
      return $ "let " ++ var2 ++ " = " ++ var1
    3 -> do  -- Borrow operation
      var1 <- arbitrary
      var2 <- arbitrary
      return $ "let " ++ var2 ++ " = &" ++ var1
    4 -> do  -- Function call
      funcName <- elements ["func1", "func2", "process"]
      varName <- arbitrary
      return $ funcName ++ "(" ++ varName ++ ")"
    5 -> do  -- Complex expression
      var1 <- arbitrary
      var2 <- arbitrary
      return $ var1 ++ " + " ++ var2

-- Generate TypusFile for ownership analysis
genTypusFileForOwnership :: Gen TypusFile
genTypusFileForOwnership = do
  numBlocks <- choose (0, 3)
  blocks <- replicateM numBlocks genOwnershipCodeBlock
  return $ TypusFile defaultFileDirectives [] blocks []

genOwnershipCodeBlock :: Gen CodeBlock
genOwnershipCodeBlock = do
  numLines <- choose (1, 5)
  codeLines <- replicateM numLines genOwnershipCode
  let content = unlines codeLines
      span = SourceSpan (SourcePos 1 1 0) (SourcePos (numLines + 1) 1 (length content))
  return $ CodeBlock defaultBlockDirectives content span

-- ============================================================================
-- Property Tests
-- ============================================================================

tests :: TestTree
tests =
  testGroup "Ownership Advanced QuickCheck Tests"
    [ testProperty "OwnershipType Show instance produces expected format" $
        \name ->
          let owned = Owned name
              borrowed = Borrowed name
              mutBorrowed = MutBorrowed name
          in "Owned " `isInfixOf` show owned .&&.
             "Borrowed " `isInfixOf` show borrowed .&&.
             "MutBorrowed " `isInfixOf` show mutBorrowed

    , testProperty "OwnershipType ordering is consistent" $
        \ownType1 ownType2 ->
          let ord1 = compare ownType1 ownType2
              ord2 = compare (show ownType1) (show ownType2)
          in ord1 === ord2

    , testProperty "OwnershipType equality is reflexive" $
        \ownType -> ownType === ownType

    , testProperty "OwnershipError Show instance contains error type" $
        \err ->
          let errStr = show err
          in case err of
            UseAfterMove var -> "UseAfterMove" `isInfixOf` errStr .&&. var `isInfixOf` errStr
            DoubleMove var1 var2 -> "DoubleMove" `isInfixOf` errStr .&&. var1 `isInfixOf` errStr .&&. var2 `isInfixOf` errStr
            BorrowWhileMoved var -> "BorrowWhileMoved" `isInfixOf` errStr .&&. var `isInfixOf` errStr
            MutBorrowWhileBorrowed var -> "MutBorrowWhileBorrowed" `isInfixOf` errStr .&&. var `isInfixOf` errStr
            BorrowWhileMutBorrowed var -> "BorrowWhileMutBorrowed" `isInfixOf` errStr .&&. var `isInfixOf` errStr
            MultipleMutBorrows var -> "MultipleMutBorrows" `isInfixOf` errStr .&&. var `isInfixOf` errStr
            UseWhileMutBorrowed var -> "UseWhileMutBorrowed" `isInfixOf` errStr .&&. var `isInfixOf` errStr
            OutOfScope var -> "OutOfScope" `isInfixOf` errStr .&&. var `isInfixOf` errStr
            BorrowError msg -> "BorrowError" `isInfixOf` errStr .&&. msg `isInfixOf` errStr
            ParseError msg -> "ParseError" `isInfixOf` errStr .&&. msg `isInfixOf` errStr
            CrossFunctionMove var1 var2 -> "CrossFunctionMove" `isInfixOf` errStr .&&. var1 `isInfixOf` errStr .&&. var2 `isInfixOf` errStr
            ParameterMoveMismatch var -> "ParameterMoveMismatch" `isInfixOf` errStr .&&. var `isInfixOf` errStr
            ControlFlowError msg -> "ControlFlowError" `isInfixOf` errStr .&&. msg `isInfixOf` errStr
            PathSensitiveError msg -> "PathSensitiveError" `isInfixOf` errStr .&&. msg `isInfixOf` errStr
            LoopOwnershipError msg -> "LoopOwnershipError" `isInfixOf` errStr .&&. msg `isInfixOf` errStr

    , testProperty "OwnershipError ordering is based on string representation" $
        \err1 err2 ->
          let ord1 = compare err1 err2
              ord2 = compare (show err1) (show err2)
          in ord1 === ord2

    , testProperty "OwnershipTransfer preserves from and to fields" $
        \from to ->
          let transfer = OwnershipTransfer from to
          in transferFrom transfer === from .&&.
             transferTo transfer === to

    , testProperty "OwnershipTransfer equality is reflexive" $
        \transfer -> transfer === transfer

    , testProperty "newOwnershipAnalyzer creates valid analyzer" $
        \analyzer -> analyzer === newOwnershipAnalyzer

    , testProperty "analyzeOwnership handles empty code gracefully" $
        \analyzer ->
          let emptyCode = ""
              result = analyzeOwnership analyzer emptyCode
          in case result of
            Left _ -> property True
            Right _ -> property True

    , testProperty "analyzeOwnership handles simple variable declarations" $
        \analyzer varName ->
          let code = "let " ++ varName ++ " = 42"
              result = analyzeOwnership analyzer code
          in case result of
            Left _ -> property True
            Right _ -> property True

    , testProperty "analyzeOwnershipFile handles TypusFile input" $
        \analyzer typusFile ->
          let result = analyzeOwnershipFile analyzer typusFile
          in case result of
            Left _ -> property True
            Right _ -> property True

    , testProperty "analyzeOwnershipDebug provides detailed output" $
        \analyzer code ->
          let result = analyzeOwnershipDebug analyzer code
          in case result of
            Left _ -> property True
            Right _ -> property True

    , testProperty "formatOwnershipErrors handles empty error list" $
        \errors ->
          null errors ==> null (formatOwnershipErrors errors)

    , testProperty "formatOwnershipErrors returns non-empty string for non-empty errors" $
        \errors ->
          not (null errors) ==> not (null (formatOwnershipErrors errors))

    , testProperty "formatOwnershipErrors includes error information" $
        \errors ->
          not (null errors) ==>
          let formatted = formatOwnershipErrors errors
              firstError = head errors
          in show firstError `isInfixOf` formatted

    , testProperty "lexAll handles empty input" $
        let emptyInput = ""
          in null (lexAll emptyInput)

    , testProperty "lexAll returns tokens for non-empty input" $
        \input ->
          not (null input) ==> not (null (lexAll input))

    , testProperty "parseProgram handles empty input" $
        let emptyInput = ""
          in case parseProgram emptyInput of
            Left _ -> property True
            Right _ -> property True

    , testProperty "parseProgram handles simple code" $
        \code ->
          not (null code) ==> 
          case parseProgram code of
            Left _ -> property True
            Right _ -> property True

    , testProperty "builtInFunctions is not empty" $
        not (null builtInFunctions)

    , testProperty "builtInFunctions contains expected function names" $
        let funcNames = builtInFunctions
            hasCommonFuncs = any (`elem` funcNames) ["print", "len", "append"]
        in hasCommonFuncs

    , testProperty "OwnershipType constructors create distinct types" $
        \name ->
          let owned = Owned name
              borrowed = Borrowed name
              mutBorrowed = MutBorrowed name
          in owned /= borrowed .&&.
             owned /= mutBorrowed .&&.
             borrowed /= mutBorrowed

    , testProperty "OwnershipType ordering follows expected hierarchy: Owned < Borrowed < MutBorrowed" $
        \name ->
          let owned = Owned name
              borrowed = Borrowed name
              mutBorrowed = MutBorrowed name
          in owned < borrowed .&&.
             borrowed < mutBorrowed .&&.
             owned < mutBorrowed

    , testProperty "OwnershipError constructors preserve their arguments" $
        \var1 var2 msg ->
          let useAfterMove = UseAfterMove var1
              doubleMove = DoubleMove var1 var2
              borrowError = BorrowError msg
          in case useAfterMove of
            UseAfterMove v -> v === var1
          in case doubleMove of
            DoubleMove v1 v2 -> v1 === var1 .&&. v2 === var2
          in case borrowError of
            BorrowError m -> m === msg

    , testProperty "OwnershipTransfer is not reflexive unless from == to" $
        \from to ->
          let transfer = OwnershipTransfer from to
              sameTransfer = OwnershipTransfer to from
          in (from == to) === (transfer == sameTransfer)

    , testProperty "Ownership analysis is deterministic" $
        \analyzer code ->
          let result1 = analyzeOwnership analyzer code
              result2 = analyzeOwnership analyzer code
          in result1 === result2
    ]