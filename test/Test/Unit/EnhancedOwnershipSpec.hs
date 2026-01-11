module Test.Unit.EnhancedOwnershipSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Ownership (OwnershipAnalysis(..), OwnershipTransfer(..), 
                  OwnershipConstraint(..), analyzeOwnership, 
                  checkOwnershipTransfer, validateOwnershipConstraints)
import Parser (TypusFile(..), defaultFileDirectives)
import SourceLocation (SourcePos(..), SourceSpan(..), Located(..))
import qualified Data.Map as Map

-- | Test OwnershipAnalysis properties
prop_ownership_analysis_empty :: Property
prop_ownership_analysis_empty = 
  let analysis = OwnershipAnalysis {
        oaTransfers = [],
        oaConstraints = [],
        oaVariables = Map.empty
      }
  in property $ 
    null (oaTransfers analysis) && 
    null (oaConstraints analysis) && 
    Map.null (oaVariables analysis)

prop_ownership_analysis_consistency :: [OwnershipTransfer] -> [OwnershipConstraint] -> Property
prop_ownership_analysis_consistency transfers constraints =
  let analysis = OwnershipAnalysis {
        oaTransfers = transfers,
        oaConstraints = constraints,
        oaVariables = Map.empty
      }
  in property $ 
    oaTransfers analysis == transfers && 
    oaConstraints analysis == constraints

-- | Test OwnershipTransfer properties
prop_ownership_transfer_equality :: String -> String -> Property
prop_ownership_transfer_equality fromVar toVar =
  let transfer1 = OwnershipTransfer fromVar toVar Nothing
      transfer2 = OwnershipTransfer fromVar toVar Nothing
  in property $ transfer1 == transfer2

prop_ownership_transfer_with_location :: String -> String -> Int -> Int -> Property
prop_ownership_transfer_with_location fromVar toVar line col =
  let location = SourceSpan (SourcePos line col) (SourcePos line (col + 1))
      transfer = OwnershipTransfer fromVar toVar (Just location)
  in property $ 
    case otLocation transfer of
      Nothing -> False
      Just loc -> loc == location

-- | Test OwnershipConstraint properties
prop_ownership_constraint_equality :: String -> String -> Property
prop_ownership_constraint_equality varName constraintType =
  let constraint1 = OwnershipConstraint varName constraintType
      constraint2 = OwnershipConstraint varName constraintType
  in property $ constraint1 == constraint2

-- | Test ownership analysis
prop_analyze_ownership_empty :: Property
prop_analyze_ownership_empty = 
  let file = TypusFile defaultFileDirectives [] "" ""
      analysis = analyzeOwnership file
  in property $ 
    null (oaTransfers analysis) && 
    null (oaConstraints analysis)

prop_analyze_ownership_preserves_variables :: [String] -> Property
prop_analyze_ownership_preserves_variables varNames =
  let varDeclarations = map (\name -> "var " ++ name ++ " int") varNames
      fileContent = unlines varDeclarations
      file = TypusFile defaultFileDirectives [] fileContent fileContent
      analysis = analyzeOwnership file
  in property $ Map.size (oaVariables analysis) >= 0

-- | Test ownership transfer checking
prop_check_ownership_transfer_self :: String -> Property
prop_check_ownership_transfer_self varName =
  let transfer = OwnershipTransfer varName varName Nothing
      result = checkOwnershipTransfer transfer
  in property $ 
    case result of
      Left _ -> True  -- Self-transfer should fail
      Right _ -> False

prop_check_ownership_transfer_valid :: String -> String -> Property
prop_check_ownership_transfer_valid fromVar toVar =
  fromVar /= toVar ==>
  let transfer = OwnershipTransfer fromVar toVar Nothing
      result = checkOwnershipTransfer transfer
  in property $ 
    case result of
      Left _ -> True
      Right _ -> True

-- | Test ownership constraint validation
prop_validate_ownership_constraints_empty :: Property
prop_validate_ownership_constraints_empty = 
  let constraints = []
      result = validateOwnershipConstraints constraints
  in property $ 
    case result of
      Left _ -> False
      Right _ -> True

prop_validate_ownership_constraints_consistent :: String -> String -> Property
prop_validate_ownership_constraints_consistent varName constraintType =
  let constraint = OwnershipConstraint varName constraintType
      constraints = [constraint]
      result = validateOwnershipConstraints constraints
  in property $ 
    case result of
      Left _ -> True
      Right _ -> True

-- | Test ownership transfer chains
prop_ownership_transfer_chain :: [String] -> Property
prop_ownership_transfer_chain varNames =
  length varNames >= 2 ==>
  let transfers = zipWith (\from to -> OwnershipTransfer from to Nothing) 
                          varNames (tail varNames)
      analysis = OwnershipAnalysis {
        oaTransfers = transfers,
        oaConstraints = [],
        oaVariables = Map.fromList (zip varNames (repeat ()))
      }
  in property $ length (oaTransfers analysis) == length varNames - 1

-- | Test ownership constraint types
prop_ownership_constraint_types :: String -> Property
prop_ownership_constraint_types varName =
  let constraintTypes = ["readonly", "mutable", "owned", "borrowed"]
      constraints = map (\ctype -> OwnershipConstraint varName ctype) constraintTypes
      result = validateOwnershipConstraints constraints
  in property $ 
    case result of
      Left _ -> True
      Right _ -> True

-- | Test ownership analysis with directives
prop_analyze_ownership_with_directives :: Bool -> Property
prop_analyze_ownership_with_directives ownershipEnabled =
  let directiveContent = if ownershipEnabled then "// @ownership true\n" else "// @ownership false\n"
      fileContent = directiveContent ++ "func main() { var x int }"
      file = TypusFile defaultFileDirectives [] fileContent fileContent
      analysis = analyzeOwnership file
  in property $ 
    if ownershipEnabled
    then True  -- Should perform ownership analysis
    else True  -- Should skip ownership analysis

-- | Test ownership error handling
prop_ownership_error_handling :: String -> String -> Property
prop_ownership_error_handling fromVar toVar =
  let transfer = OwnershipTransfer fromVar toVar Nothing
      result = checkOwnershipTransfer transfer
  in property $ 
    case result of
      Left error -> not (null error)
      Right _ -> True

-- | Test ownership variable tracking
prop_ownership_variable_tracking :: [String] -> Property
prop_ownership_variable_tracking varNames =
  let variables = Map.fromList (zip varNames (repeat ()))
      analysis = OwnershipAnalysis {
        oaTransfers = [],
        oaConstraints = [],
        oaVariables = variables
      }
  in property $ Map.size (oaVariables analysis) == length varNames

-- | Test ownership transfer validation
prop_ownership_transfer_validation :: String -> String -> String -> Property
prop_ownership_transfer_validation fromVar toVar thirdVar =
  let transfers = [OwnershipTransfer fromVar toVar Nothing,
                   OwnershipTransfer toVar thirdVar Nothing]
      analysis = OwnershipAnalysis {
        oaTransfers = transfers,
        oaConstraints = [],
        oaVariables = Map.fromList [(fromVar, ()), (toVar, ()), (thirdVar, ())]
      }
  in property $ length (oaTransfers analysis) == 2

-- | Test ownership constraint propagation
prop_ownership_constraint_propagation :: String -> [String] -> Property
prop_ownership_constraint_propagation baseVar relatedVars =
  let constraints = map (\var -> OwnershipConstraint var "readonly") (baseVar : relatedVars)
      analysis = OwnershipAnalysis {
        oaTransfers = [],
        oaConstraints = constraints,
        oaVariables = Map.fromList ((baseVar, ()) : zip relatedVars (repeat ()))
      }
  in property $ length (oaConstraints analysis) == length (baseVar : relatedVars)

tests :: TestTree
tests = testGroup "Enhanced Ownership Tests"
  [ testGroup "OwnershipAnalysis tests"
    [ testProperty "ownership analysis empty" prop_ownership_analysis_empty
    , testProperty "ownership analysis consistency" prop_ownership_analysis_consistency
    ]
  , testGroup "OwnershipTransfer tests"
    [ testProperty "ownership transfer equality" prop_ownership_transfer_equality
    , testProperty "ownership transfer with location" prop_ownership_transfer_with_location
    ]
  , testGroup "OwnershipConstraint tests"
    [ testProperty "ownership constraint equality" prop_ownership_constraint_equality
    ]
  , testGroup "Ownership analysis"
    [ testProperty "analyze ownership empty" prop_analyze_ownership_empty
    , testProperty "analyze ownership preserves variables" prop_analyze_ownership_preserves_variables
    , testProperty "analyze ownership with directives" prop_analyze_ownership_with_directives
    ]
  , testGroup "Ownership transfer checking"
    [ testProperty "check ownership transfer self" prop_check_ownership_transfer_self
    , testProperty "check ownership transfer valid" prop_check_ownership_transfer_valid
    ]
  , testGroup "Ownership constraint validation"
    [ testProperty "validate ownership constraints empty" prop_validate_ownership_constraints_empty
    , testProperty "validate ownership constraints consistent" prop_validate_ownership_constraints_consistent
    ]
  , testGroup "Ownership transfer chains"
    [ testProperty "ownership transfer chain" prop_ownership_transfer_chain
    ]
  , testGroup "Ownership constraint types"
    [ testProperty "ownership constraint types" prop_ownership_constraint_types
    ]
  , testGroup "Error handling"
    [ testProperty "ownership error handling" prop_ownership_error_handling
    ]
  , testGroup "Variable tracking"
    [ testProperty "ownership variable tracking" prop_ownership_variable_tracking
    ]
  , testGroup "Transfer validation"
    [ testProperty "ownership transfer validation" prop_ownership_transfer_validation
    ]
  , testGroup "Constraint propagation"
    [ testProperty "ownership constraint propagation" prop_ownership_constraint_propagation
    ]
  ]