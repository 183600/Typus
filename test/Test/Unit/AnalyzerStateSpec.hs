module Test.Unit.AnalyzerStateSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Analyzer.State

-- Test analyzer state transitions
prop_state_transition_valid :: AnalyzerState -> AnalysisPhase -> Property
prop_state_transition_valid state phase =
  let newState = transitionToPhase state phase
      currentPhase = getCurrentPhase newState
  in property $ currentPhase === phase

-- Test analyzer state history
prop_state_history_preservation :: AnalyzerState -> [AnalysisPhase] -> Property
prop_state_history_preservation initialState phases =
  let finalState = foldl transitionToPhase initialState phases
      history = getStateHistory finalState
  in property $ length history >= length phases

-- Test analyzer state rollback
prop_state_rollback :: AnalyzerState -> [AnalysisPhase] -> Property
prop_state_rollback initialState phases =
  let intermediateState = foldl transitionToPhase initialState phases
      rolledBackState = rollbackToPhase intermediateState (head phases)
      currentPhase = getCurrentPhase rolledBackState
  in property $ 
    if null phases 
    then property True
    else currentPhase === head phases

-- Test analyzer state checkpoint
prop_state_checkpoint :: AnalyzerState -> String -> Property
prop_state_checkpoint state checkpointId =
  let stateWithCheckpoint = createCheckpoint state checkpointId
      restoredState = restoreFromCheckpoint stateWithCheckpoint checkpointId
  in property $ getAnalyzerStateId restoredState === getAnalyzerStateId state

-- Test analyzer state merging
prop_state_merging :: AnalyzerState -> AnalyzerState -> Property
prop_state_merging state1 state2 =
  let mergedState = mergeAnalyzerStates state1 state2
      mergedResults = getAllAnalysisResults mergedState
      results1 = getAllAnalysisResults state1
      results2 = getAllAnalysisResults state2
  in property $ 
    all (`elem` mergedResults) results1 && 
    all (`elem` mergedResults) results2

tests :: TestTree
tests = testGroup "Analyzer State Tests"
  [ testProperty "state transition valid" prop_state_transition_valid
  , testProperty "state history preservation" prop_state_history_preservation
  , testProperty "state rollback" prop_state_rollback
  , testProperty "state checkpoint" prop_state_checkpoint
  , testProperty "state merging" prop_state_merging
  ]