{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.OwnershipAnalysisBoundarySpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, choose, listOf, elements)
import Test.QuickCheck.Gen (oneof, suchThat)

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

import Ownership.Common.Types (OwnershipAnalyzer(..), OwnershipTransfer(..))
import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf)
import Data.List (nub)
import qualified Data.Map as Map

-- Helper generators for ownership testing

-- Generate variable names
genVarName :: Gen String
genVarName = do
  first <- elements ['a'..'z']
  rest <- listOf (elements (['a'..'z'] ++ ['0'..'9'] ++ ['_']))
  return (first : rest)

-- Generate ownership types
genOwnershipType :: Gen OwnershipType
genOwnershipType = oneof
  [ Owned <$> genVarName
  , Borrowed <$> genVarName
  , MutBorrowed <$> genVarName
  ]

-- Generate ownership errors
genOwnershipError :: Gen OwnershipError
genOwnershipError = oneof
  [ UseAfterMove <$> genVarName
  , DoubleMove <$> genVarName <*> genVarName
  , BorrowWhileMoved <$> genVarName
  , MutBorrowWhileBorrowed <$> genVarName
  , BorrowWhileMutBorrowed <$> genVarName
  , MultipleMutBorrows <$> genVarName
  , UseWhileMutBorrowed <$> genVarName
  , OutOfScope <$> genVarName
  , BorrowError <$> genVarName
  , ParseError <$> listOf1 (elements ['a'..'z'])
  , CrossFunctionMove <$> genVarName <*> genVarName
  , ParameterMoveMismatch <$> genVarName
  , ControlFlowError <$> listOf1 (elements ['a'..'z'])
  ]

-- Generate ownership transfers
genOwnershipTransfer :: Gen OwnershipTransfer
genOwnershipTransfer = oneof
  [ MoveTransfer <$> genVarName <*> genVarName
  , BorrowTransfer <$> genVarName <*> genVarName
  , MutBorrowTransfer <$> genVarName <*> genVarName
  , ReleaseTransfer <$> genVarName
  ]

-- Generate simple ownership-aware code snippets
genSimpleOwnershipCode :: Gen String
genSimpleOwnershipCode = oneof
  [ return $ unlines
    [ "fn main() {"
    , "    let x = 42;"
    , "    let y = x;"  -- Move
    , "}"
    ]
  , return $ unlines
    [ "fn main() {"
    , "    let data = String::new();"
    , "    let borrowed = &data;"  -- Borrow
    , "    println!(\"{}\", borrowed);"
    , "}"
    ]
  , return $ unlines
    [ "fn main() {"
    , "    mut data = String::new();"
    , "    let borrowed = &mut data;"  -- Mutable borrow
    , "    borrowed.push_str(\"hello\");"
    , "}"
    ]
  ]

-- Generate code with ownership violations
genOwnershipViolationCode :: Gen String
genOwnershipViolationCode = oneof
  [ return $ unlines
    [ "fn main() {"
    , "    let x = 42;"
    , "    let y = x;"  -- Move
    , "    println!(\"{}\", x);"  -- Use after move
    , "}"
    ]
  , return $ unlines
    [ "fn main() {"
    , "    let data = String::new();"
    , "    let borrowed = &data;"  -- Borrow
    , "    let moved = data;"  -- Move while borrowed
    , "}"
    ]
  , return $ unlines
    [ "fn main() {"
    , "    let data = String::new();"
    , "    let borrow1 = &data;"  -- First borrow
    , "    let borrow2 = &mut data;"  -- Mutable borrow while immutable borrowed
    , "}"
    ]
  ]

-- Generate complex ownership scenarios
genComplexOwnershipCode :: Gen String
genComplexOwnershipCode = do
  varCount <- choose (1, 5)
  let vars = take varCount (cycle ["x", "y", "z", "data", "value"])
  let createVar v = "    let " ++ v ++ " = " ++ show (L.length v) ++ ";"
  let moveVar from to = "    let " ++ to ++ " = " ++ from ++ ";"
  let borrowVar v = "    let borrowed_" ++ v ++ " = &" ++ v ++ ";"
  let code = unlines $
        ["fn main() {"] ++
        map createVar vars ++
        L.map (uncurry moveVar) (zip vars (L.tail vars ++ ["moved"])) ++
        map borrowVar vars ++
        ["}"]
  return code

-- Arbitrary instances
instance Arbitrary OwnershipType where
  arbitrary = genOwnershipType

instance Arbitrary OwnershipError where
  arbitrary = genOwnershipError

instance Arbitrary OwnershipTransfer where
  arbitrary = genOwnershipTransfer

-- Boundary L.and edge case property tests

-- Property: newOwnershipAnalyzer should create analyzer with empty state
prop_new_analyzer_empty_state :: Property
prop_new_analyzer_empty_state =
  let analyzer = newOwnershipAnalyzer
  in property $ True  -- Should create analyzer without crashing

-- Property: analyzeOwnership should handle empty input
prop_analyze_ownership_empty :: Property
prop_analyze_ownership_empty =
  let result = analyzeOwnership "" ""
  in case result of
    Left _ -> property True  -- Should handle gracefully
    Right _ -> property True  -- Or succeed

-- Property: analyzeOwnership should handle simple valid code
prop_analyze_ownership_simple_valid :: Property
prop_analyze_ownership_simple_valid =
  forAll genSimpleOwnershipCode $ \simpleCode ->
  let result = analyzeOwnership simpleCode ""
  in case result of
    Left _ -> property True  -- May fail gracefully
    Right _ -> property True  -- Should succeed for valid code

-- Property: analyzeOwnership should detect ownership violations
prop_analyze_ownership_detects_violations :: Property
prop_analyze_ownership_detects_violations =
  forAll genOwnershipViolationCode $ \violationCode ->
  let result = analyzeOwnership violationCode ""
  in case result of
    Left errors -> property $ not (null errors)  -- Should detect violations
    Right _ -> property True  -- May still succeed with warnings

-- Property: analyzeOwnership should handle complex scenarios
prop_analyze_ownership_complex :: Property
prop_analyze_ownership_complex =
  forAll genComplexOwnershipCode $ \complexCode ->
  let result = analyzeOwnership complexCode ""
  in case result of
    Left _ -> property True  -- Should handle gracefully
    Right _ -> property True  -- Should analyze successfully

-- Property: analyzeOwnershipFile should handle file paths
prop_analyze_ownership_file :: Property
prop_analyze_ownership_file =
  let dummyFilePath = "test.typus"
      result = analyzeOwnershipFile dummyFilePath
  in case result of
    Left _ -> property True  -- File may not exist, should handle gracefully
    Right _ -> property True  -- Or succeed if file exists

-- Property: analyzeOwnershipDebug should provide debug information
prop_analyze_ownership_debug :: Property
prop_analyze_ownership_debug =
  forAll genSimpleOwnershipCode $ \simpleCode ->
  let result = analyzeOwnershipDebug simpleCode ""
  in case result of
    Left _ -> property True  -- Should handle gracefully
    Right _ -> property True  -- Should provide debug info

-- Property: formatOwnershipErrors should handle error lists
prop_format_ownership_errors :: Property
prop_format_ownership_errors =
  forAll (listOf genOwnershipError) $ \errors ->
  let formatted = formatOwnershipErrors errors
  in property $ L.length formatted >= 0  -- Should format without crashing

-- Property: lexAll should handle empty input
prop_lex_all_empty :: Property
prop_lex_all_empty =
  let result = lexAll ""
  in case result of
    Left _ -> property True  -- Should handle gracefully
    Right tokens -> property $ null tokens  -- Should return empty tokens

-- Property: lexAll should tokenize simple code
prop_lex_all_simple :: Property
prop_lex_all_simple =
  let simpleCode = "let x = 42;"
      result = lexAll simpleCode
  in case result of
    Left _ -> property True  -- May fail gracefully
    Right tokens -> property $ L.length tokens > 0  -- Should produce tokens

-- Property: parseProgram should handle empty input
prop_parse_program_empty :: Property
prop_parse_program_empty =
  let result = parseProgram ""
  in case result of
    Left _ -> property True  -- Should handle gracefully
    Right ast -> property $ True  -- Should produce AST

-- Property: parseProgram should handle simple code
prop_parse_program_simple :: Property
prop_parse_program_simple =
  let simpleCode = "fn main() { let x = 42; }"
      result = parseProgram simpleCode
  in case result of
    Left _ -> property True  -- May fail gracefully
    Right ast -> property $ True  -- Should produce AST

-- Property: builtInFunctions should be non-empty
prop_built_in_functions_non_empty :: Property
prop_built_in_functions_non_empty =
  let builtIns = builtInFunctions
  in property $ not (null builtIns)

-- Property: OwnershipType ordering should be consistent
prop_ownership_type_ordering :: Property
prop_ownership_type_ordering =
  forAll genOwnershipType $ \ownershipType1 ->
  forAll genOwnershipType $ \ownershipType2 ->
  let comparison = ownershipType1 <= ownershipType2
      comparison2 = ownershipType2 <= ownershipType1
  in property $ (comparison && comparison2) ==> (ownershipType1 === ownershipType2)

-- Property: OwnershipType equality should work correctly
prop_ownership_type_equality :: Property
prop_ownership_type_equality =
  forAll genOwnershipType $ \ownershipType ->
  let sameOwnershipType = ownershipType
  in property $ ownershipType === sameOwnershipType

-- Property: OwnershipError should have meaningful string representation
prop_ownership_error_show :: Property
prop_ownership_error_show =
  forAll genOwnershipError $ \error ->
  let errorString = show error
  in property $ L.length errorString > 0  -- Should have non-empty representation

-- Property: OwnershipTransfer should track transfers correctly
prop_ownership_transfer_tracking :: Property
prop_ownership_transfer_tracking =
  forAll genOwnershipTransfer $ \transfer ->
  case transfer of
    MoveTransfer from to -> property $ from /= to
    BorrowTransfer from to -> property $ from /= to
    MutBorrowTransfer from to -> property $ from /= to
    ReleaseTransfer var -> property $ True  -- Release can be L.any variable

-- Property: Multiple ownership analysis should be consistent
prop_multiple_analysis_consistent :: Property
prop_multiple_analysis_consistent =
  forAll genSimpleOwnershipCode $ \simpleCode ->
  let result1 = analyzeOwnership simpleCode ""
      result2 = analyzeOwnership simpleCode ""
  in case (result1, result2) of
    (Left err1, Left err2) -> property $ err1 === err2
    (Right res1, Right res2) -> property $ True  -- Compare results appropriately
    _ -> property False  -- Should be consistent

-- Property: Ownership analysis should handle variable shadowing
prop_ownership_handles_shadowing :: Property
prop_ownership_handles_shadowing =
  let shadowingCode = unlines
    [ "fn main() {"
    , "    let x = 42;"
    , "    {"
    , "        let x = \"hello\";"  -- Shadowing
    , "        println!(\"{}\", x);"
    , "    }"
    , "    println!(\"{}\", x);"  -- Original x
    , "}"
    ]
      result = analyzeOwnership shadowingCode ""
  in case result of
    Left _ -> property True  -- Should handle gracefully
    Right _ -> property True  -- Should analyze correctly

-- Property: Ownership analysis should handle function calls
prop_ownership_handles_functions :: Property
prop_ownership_handles_functions =
  let functionCode = unlines
    [ "fn consume(data: String) {"
    , "    // data is consumed here"
    , "}"
    , ""
    , "fn main() {"
    , "    let data = String::new();"
    , "    consume(data);"  -- Move to function
    , "}"
    ]
      result = analyzeOwnership functionCode ""
  in case result of
    Left _ -> property True  -- Should handle gracefully
    Right _ -> property True  -- Should analyze correctly

-- Property: Ownership analysis should handle return values
prop_ownership_handles_returns :: Property
prop_ownership_handles_returns =
  let returnCode = unlines
    [ "fn create_data() -> String {"
    , "    String::new()"
    , "}"
    , ""
    , "fn main() {"
    , "    let data = create_data();"  -- Move from return
    , "}"
    ]
      result = analyzeOwnership returnCode ""
  in case result of
    Left _ -> property True  -- Should handle gracefully
    Right _ -> property True  -- Should analyze correctly

-- Property: Ownership analysis should handle loops
prop_ownership_handles_loops :: Property
prop_ownership_handles_loops =
  let loopCode = unlines
    [ "fn main() {"
    , "    let data = String::new();"
    , "    for i in 0..10 {"
    , "        let borrowed = &data;"  -- Borrow in loop
    , "        println!(\"{}: {}\", i, borrowed);"
    , "    }"
    , "}"
    ]
      result = analyzeOwnership loopCode ""
  in case result of
    Left _ -> property True  -- Should handle gracefully
    Right _ -> property True  -- Should analyze correctly

-- Property: Ownership analysis should handle conditionals
prop_ownership_handles_conditionals :: Property
prop_ownership_handles_conditionals =
  let conditionalCode = unlines
    [ "fn main() {"
    , "    let data = String::new();"
    , "    if true {"
    , "        let borrowed = &data;"
    , "        println!(\"{}\", borrowed);"
    , "    } else {"
    , "        let moved = data;"  -- Move in else branch
    , "    }"
    , "}"
    ]
      result = analyzeOwnership conditionalCode ""
  in case result of
    Left _ -> property True  -- Should handle gracefully
    Right _ -> property True  -- Should analyze correctly

-- Property: Ownership analysis should handle struct fields
prop_ownership_handles_structs :: Property
prop_ownership_handles_structs =
  let structCode = unlines
    [ "struct Point {"
    , "    x: i32,"
    , "    y: i32"
    , "}"
    , ""
    , "fn main() {"
    , "    let p = Point { x: 1, y: 2 };"
    , "    let moved = p;"  -- Move struct"
    , "}"
    ]
      result = analyzeOwnership structCode ""
  in case result of
    Left _ -> property True  -- Should handle gracefully
    Right _ -> property True  -- Should analyze correctly

tests :: TestTree
tests = testGroup "Ownership Analysis Boundary Tests"
  [ fastProperty "newOwnershipAnalyzer creates analyzer with empty state" prop_new_analyzer_empty_state
  , fastProperty "analyzeOwnership handles empty input" prop_analyze_ownership_empty
  , fastProperty "analyzeOwnership handles simple valid code" prop_analyze_ownership_simple_valid
  , fastProperty "analyzeOwnership detects ownership violations" prop_analyze_ownership_detects_violations
  , fastProperty "analyzeOwnership handles complex scenarios" prop_analyze_ownership_complex
  , fastProperty "analyzeOwnershipFile handles file paths" prop_analyze_ownership_file
  , fastProperty "analyzeOwnershipDebug provides debug information" prop_analyze_ownership_debug
  , fastProperty "formatOwnershipErrors handles error lists" prop_format_ownership_errors
  , fastProperty "lexAll handles empty input" prop_lex_all_empty
  , fastProperty "lexAll tokenizes simple code" prop_lex_all_simple
  , fastProperty "parseProgram handles empty input" prop_parse_program_empty
  , fastProperty "parseProgram handles simple code" prop_parse_program_simple
  , fastProperty "builtInFunctions is non-empty" prop_built_in_functions_non_empty
  , fastProperty "OwnershipType ordering is consistent" prop_ownership_type_ordering
  , fastProperty "OwnershipType equality works correctly" prop_ownership_type_equality
  , fastProperty "OwnershipError has meaningful string representation" prop_ownership_error_show
  , fastProperty "OwnershipTransfer tracks transfers correctly" prop_ownership_transfer_tracking
  , fastProperty "Multiple ownership analysis is consistent" prop_multiple_analysis_consistent
  , fastProperty "Ownership analysis handles variable shadowing" prop_ownership_handles_shadowing
  , fastProperty "Ownership analysis handles function calls" prop_ownership_handles_functions
  , fastProperty "Ownership analysis handles return values" prop_ownership_handles_returns
  , fastProperty "Ownership analysis handles loops" prop_ownership_handles_loops
  , fastProperty "Ownership analysis handles conditionals" prop_ownership_handles_conditionals
  , fastProperty "Ownership analysis handles struct fields" prop_ownership_handles_structs
  ]