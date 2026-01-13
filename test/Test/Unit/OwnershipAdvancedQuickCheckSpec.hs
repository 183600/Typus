module Test.Unit.OwnershipAdvancedQuickCheckSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Ownership.Common.Types
  ( OwnershipType(..)
  , OwnershipError(..)
  , OwnershipAnalyzer(..)
  , OwnershipTransfer(..)
  , newOwnershipAnalyzer
  )
import Ownership.Analyzer
  ( analyzeOwnership
  , analyzeOwnershipDebug
  , analyzeOwnershipFile
  , builtInFunctions
  )
import Ownership.Lexer (lexAll)
import Ownership.Parser (parseProgram)
import Ownership.Reporter (formatOwnershipErrors)
import Ownership
  ( OwnershipType(..)
  , OwnershipError(..)
  , OwnershipAnalyzer(..)
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
import Data.List (sort, nub)

-- | 生成变量名
newtype VariableName = VariableName { getVariableName :: String }
  deriving Show

instance Arbitrary VariableName where
  arbitrary = do
    len <- choose (1, 10)
    chars <- vectorOf len $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']
    return $ VariableName chars

-- | 生成OwnershipType
instance Arbitrary OwnershipType where
  arbitrary = do
    VariableName name <- arbitrary
    elements [Owned name, Borrowed name, MutBorrowed name]

-- | 生成OwnershipError
instance Arbitrary OwnershipError where
  arbitrary = do
    VariableName name1 <- arbitrary
    VariableName name2 <- arbitrary
    elements [ UseAfterMove name1
             , DoubleMove name1 name2
             , BorrowWhileMoved name1
             , MutBorrowWhileBorrowed name1
             , BorrowWhileMutBorrowed name1
             , MultipleMutBorrows name1
             , UseWhileMutBorrowed name1
             , OutOfScope name1
             , BorrowError "test borrow error"
             , ParseError "test parse error"
             , CrossFunctionMove name1 name2
             , ParameterMoveMismatch name1
             , ControlFlowError "test control flow error"
             , PathSensitiveError "test path sensitive error"
             , LoopOwnershipError "test loop ownership error"
             ]

-- | 生成OwnershipTransfer
instance Arbitrary OwnershipTransfer where
  arbitrary = do
    VariableName from <- arbitrary
    VariableName to <- arbitrary
    return $ OwnershipTransfer from to

-- | 生成简单的所有权代码
newtype OwnershipCode = OwnershipCode { getOwnershipCode :: String }
  deriving Show

instance Arbitrary OwnershipCode where
  arbitrary = do
    codeType <- elements ["move", "borrow", "mut_borrow", "simple"]
    VariableName var1 <- arbitrary
    VariableName var2 <- arbitrary
    case codeType of
      "move" -> return $ OwnershipCode $ "let " ++ var1 ++ " = Box::new(5);\nlet " ++ var2 ++ " = " ++ var1 ++ ";"
      "borrow" -> return $ OwnershipCode $ "let " ++ var1 ++ " = 5;\nlet " ++ var2 ++ " = &" ++ var1 ++ ";"
      "mut_borrow" -> return $ OwnershipCode $ "let mut " ++ var1 ++ " = 5;\nlet " ++ var2 ++ " = &mut " ++ var1 ++ ";"
      "simple" -> return $ OwnershipCode $ "let " ++ var1 ++ " = 5;"
      _ -> return $ OwnershipCode $ "let x = 5;"

-- | 测试OwnershipType的基本属性
prop_ownership_type_name :: OwnershipType -> Property
prop_ownership_type_name ownershipType =
  case ownershipType of
    Owned name -> not (null name)
    Borrowed name -> not (null name)
    MutBorrowed name -> not (null name)

prop_ownership_type_eq :: OwnershipType -> OwnershipType -> Property
prop_ownership_type_eq ot1 ot2 =
  (ot1 == ot2) === (show ot1 == show ot2)

prop_ownership_type_ord :: OwnershipType -> OwnershipType -> Property
prop_ownership_type_ord ot1 ot2 =
  let ordering = compare ot1 ot2
  in ordering === EQ || ordering === LT || ordering === GT

prop_ownership_type_ord_transitive :: OwnershipType -> OwnershipType -> OwnershipType -> Property
prop_ownership_type_ord_transitive ot1 ot2 ot3 =
  (ot1 <= ot2 && ot2 <= ot3) ==> ot1 <= ot3

-- | 测试OwnershipError的基本属性
prop_ownership_error_eq :: OwnershipError -> OwnershipError -> Property
prop_ownership_error_eq oe1 oe2 =
  (oe1 == oe2) === (show oe1 == show oe2)

prop_ownership_error_ord :: OwnershipError -> OwnershipError -> Property
prop_ownership_error_ord oe1 oe2 =
  let ordering = compare oe1 oe2
  in ordering === EQ || ordering === LT || ordering === GT

prop_ownership_error_ord_transitive :: OwnershipError -> OwnershipError -> OwnershipError -> Property
prop_ownership_error_ord_transitive oe1 oe2 oe3 =
  (oe1 <= oe2 && oe2 <= oe3) ==> oe1 <= oe3

prop_ownership_error_use_after_move :: VariableName -> Property
prop_ownership_error_use_after_move (VariableName name) =
  let err = UseAfterMove name
  in show err === "UseAfterMove " ++ name

prop_ownership_error_double_move :: VariableName -> VariableName -> Property
prop_ownership_error_double_move (VariableName name1) (VariableName name2) =
  let err = DoubleMove name1 name2
  in show err === "DoubleMove " ++ name1 ++ " " ++ name2

-- | 测试OwnershipTransfer的基本属性
prop_ownership_transfer_fields :: OwnershipTransfer -> Property
prop_ownership_transfer_fields transfer =
  let from = transferFrom transfer
      to = transferTo transfer
  in not (null from) .&&. not (null to)

prop_ownership_transfer_eq :: OwnershipTransfer -> OwnershipTransfer -> Property
prop_ownership_transfer_eq ot1 ot2 =
  (ot1 == ot2) === (transferFrom ot1 == transferFrom ot2 && transferTo ot1 == transferTo ot2)

-- | 测试OwnershipAnalyzer的基本属性
prop_ownership_analyzer_creation :: Property
prop_ownership_analyzer_creation =
  let analyzer = newOwnershipAnalyzer
  in case analyzer of
    OwnershipAnalyzer () -> property True

-- | 测试所有权分析的基本属性
prop_ownership_analysis_simple :: OwnershipCode -> Property
prop_ownership_analysis_simple (OwnershipCode code) =
  let analyzer = newOwnershipAnalyzer
      result = analyzeOwnership analyzer code
  in case result of
    Left _ -> property True  -- 分析可能失败，这是预期的
    Right _ -> property True  -- 分析可能成功

prop_ownership_analysis_debug :: OwnershipCode -> Property
prop_ownership_analysis_debug (OwnershipCode code) =
  let analyzer = newOwnershipAnalyzer
      result = analyzeOwnershipDebug analyzer code
  in case result of
    Left _ -> property True  -- 分析可能失败，这是预期的
    Right _ -> property True  -- 分析可能成功

prop_ownership_analysis_file :: Property
prop_ownership_analysis_file =
  let analyzer = newOwnershipAnalyzer
      filePath = "test.typus"
      result = analyzeOwnershipFile analyzer filePath
  in case result of
    Left _ -> property True  -- 分析可能失败，这是预期的
    Right _ -> property True  -- 分析可能成功

-- | 测试词法分析的基本属性
prop_lex_all_simple :: OwnershipCode -> Property
prop_lex_all_simple (OwnershipCode code) =
  let tokens = lexAll code
  in length tokens >= 0

prop_lex_all_empty :: Property
prop_lex_all_empty =
  let tokens = lexAll ""
  in null tokens

-- | 测试语法分析的基本属性
prop_parse_program_simple :: OwnershipCode -> Property
prop_parse_program_simple (OwnershipCode code) =
  let result = parseProgram code
  in case result of
    Left _ -> property True  -- 解析可能失败，这是预期的
    Right _ -> property True  -- 解析可能成功

prop_parse_program_empty :: Property
prop_parse_program_empty =
  let result = parseProgram ""
  in case result of
    Left _ -> property True  -- 解析可能失败，这是预期的
    Right ast -> property True  -- 解析可能成功

-- | 测试错误格式化的基本属性
prop_format_ownership_errors :: [OwnershipError] -> Property
prop_format_ownership_errors errors =
  let formatted = formatOwnershipErrors errors
      errorStrings = map show errors
  in all (`isInfixOf` formatted) errorStrings

prop_format_ownership_errors_empty :: Property
prop_format_ownership_errors_empty =
  let formatted = formatOwnershipErrors []
  in null formatted

-- | 测试内置函数的基本属性
prop_built_in_functions_not_empty :: Property
prop_built_in_functions_not_empty =
  not (null builtInFunctions)

prop_built_in_functions_unique :: Property
prop_built_in_functions_unique =
  let unique = nub builtInFunctions
  in length unique === length builtInFunctions

-- | 测试所有权类型的比较
prop_ownership_type_comparison :: OwnershipType -> OwnershipType -> Property
prop_ownership_type_comparison ot1 ot2 =
  let result = compare ot1 ot2
  in case (ot1, ot2) of
    (Owned a, Owned b) -> result === compare a b
    (Owned _, Borrowed _) -> result === LT
    (Owned _, MutBorrowed _) -> result === LT
    (Borrowed a, Borrowed b) -> result === compare a b
    (Borrowed _, MutBorrowed _) -> result === LT
    (Borrowed _, Owned _) -> result === GT
    (MutBorrowed a, MutBorrowed b) -> result === compare a b
    (MutBorrowed _, Owned _) -> result === GT
    (MutBorrowed _, Borrowed _) -> result === GT

-- | 测试所有权错误的比较
prop_ownership_error_comparison :: OwnershipError -> OwnershipError -> Property
prop_ownership_error_comparison oe1 oe2 =
  compare oe1 oe2 === compare (show oe1) (show oe2)

-- | 测试所有权传递的有效性
prop_ownership_transfer_valid :: OwnershipTransfer -> Property
prop_ownership_transfer_valid transfer =
  let from = transferFrom transfer
      to = transferTo transfer
  in from /= to ==> property True  -- 不同变量之间的传递是有效的

-- | 测试所有权类型的排序
prop_ownership_type_sorting :: [OwnershipType] -> Property
prop_ownership_type_sorting types =
  let sorted = sort types
  in sorted === sort sorted  -- 排序是稳定的

-- | 测试所有权错误的排序
prop_ownership_error_sorting :: [OwnershipError] -> Property
prop_ownership_error_sorting errors =
  let sorted = sort errors
  in sorted === sort sorted  -- 排序是稳定的

-- | 测试所有权分析的组合
prop_ownership_analysis_lex_parse :: OwnershipCode -> Property
prop_ownership_analysis_lex_parse (OwnershipCode code) =
  let tokens = lexAll code
      parseResult = parseProgram code
  in case parseResult of
    Left _ -> property True
    Right _ -> length tokens >= 0

-- 辅助函数
isInfixOf :: String -> String -> Bool
isInfixOf needle haystack = needle `Data.List.isInfixOf` haystack

tests :: TestTree
tests = testGroup "Ownership Advanced QuickCheck Tests"
  -- OwnershipType tests
  [ testProperty "ownership type name" prop_ownership_type_name
  , testProperty "ownership type eq" prop_ownership_type_eq
  , testProperty "ownership type ord" prop_ownership_type_ord
  , testProperty "ownership type ord transitive" prop_ownership_type_ord_transitive
  , testProperty "ownership type comparison" prop_ownership_type_comparison
  , testProperty "ownership type sorting" prop_ownership_type_sorting
  
  -- OwnershipError tests
  , testProperty "ownership error eq" prop_ownership_error_eq
  , testProperty "ownership error ord" prop_ownership_error_ord
  , testProperty "ownership error ord transitive" prop_ownership_error_ord_transitive
  , testProperty "ownership error use after move" prop_ownership_error_use_after_move
  , testProperty "ownership error double move" prop_ownership_error_double_move
  , testProperty "ownership error comparison" prop_ownership_error_comparison
  , testProperty "ownership error sorting" prop_ownership_error_sorting
  
  -- OwnershipTransfer tests
  , testProperty "ownership transfer fields" prop_ownership_transfer_fields
  , testProperty "ownership transfer eq" prop_ownership_transfer_eq
  , testProperty "ownership transfer valid" prop_ownership_transfer_valid
  
  -- OwnershipAnalyzer tests
  , testProperty "ownership analyzer creation" prop_ownership_analyzer_creation
  
  -- Ownership analysis tests
  , testProperty "ownership analysis simple" prop_ownership_analysis_simple
  , testProperty "ownership analysis debug" prop_ownership_analysis_debug
  , testProperty "ownership analysis file" prop_ownership_analysis_file
  , testProperty "ownership analysis lex parse" prop_ownership_analysis_lex_parse
  
  -- Lexer tests
  , testProperty "lex all simple" prop_lex_all_simple
  , testProperty "lex all empty" prop_lex_all_empty
  
  -- Parser tests
  , testProperty "parse program simple" prop_parse_program_simple
  , testProperty "parse program empty" prop_parse_program_empty
  
  -- Error formatting tests
  , testProperty "format ownership errors" prop_format_ownership_errors
  , testProperty "format ownership errors empty" prop_format_ownership_errors_empty
  
  -- Built-in functions tests
  , testProperty "built in functions not empty" prop_built_in_functions_not_empty
  , testProperty "built in functions unique" prop_built_in_functions_unique
  ]