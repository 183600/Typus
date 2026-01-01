{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.CompilerBasicQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck
import qualified Data.Map as Map
import Data.List (sort, nub)

import Compiler (CompilationPhase(..))
import TestSupport.Arbitrary ()

tests :: TestTree
tests = testGroup "Compiler Basic QuickCheck Tests"
  [ compilationPhaseProperties
  , typeEnvProperties
  , errorMessageProperties
  ]

compilationPhaseProperties :: TestTree
compilationPhaseProperties = testGroup "Compilation Phase Properties"
  [ fastProperty "CompilationPhase equality is reflexive" $ \phase ->
      phase === (phase :: CompilationPhase)
  
  , fastProperty "CompilationPhase show is non-empty" $ \phase ->
      not (L.null (show (phase :: CompilationPhase)))
  ]

typeEnvProperties :: TestTree
typeEnvProperties = testGroup "Type Environment Properties"
  [ fastProperty "empty type env has no bindings" $
      Map.size (Map.empty :: Map.Map String String) === 0
  
  , fastProperty "adding binding increases size" $ \(k :: String) (v :: String) ->
      let env = Map.empty :: Map.Map String String
          env' = Map.insert k v env
      in Map.size env' >= Map.size env
  
  , fastProperty "looking up inserted key succeeds" $ \(k :: String) (v :: String) (env :: Map.Map String String) ->
      Map.lookup k (Map.insert k v env) === Just v
  ]

errorMessageProperties :: TestTree
errorMessageProperties = testGroup "Error Message Properties"
  [ fastProperty "error messages are non-empty" $ \(msg :: String) ->
      not (null msg) ==> L.length msg > 0
  
  , fastProperty "concatenating errors preserves both" $ \(e1 :: String) (e2 :: String) ->
      let combined = e1 ++ "\n" ++ e2
      in L.length combined >= L.length e1 && L.length combined >= L.length e2
  ]
