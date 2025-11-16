module Test.Unit.OwnershipBridgeSpec (tests) where

import Analyzer.OwnershipBridge (processOwnershipErrors)
import Analyzer.State (newIntegratedAnalyzer)
import Analyzer.Types (AnalyzerState(..), SymbolInfo(..))
import Control.Monad.Except (runExceptT)
import Control.Monad.State (runStateT)
import qualified Data.Map.Strict as Map
import qualified Ownership as Own
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

tests :: TestTree
tests =
  testGroup "OwnershipBridge"
    [ testCase "filtered ownership errors do not mutate the symbol table" $ do
        let code = unlines
              [ "package main"
              , ""
              , "func main() {}"
              ]
            reservedName = "type"
            reservedInfo = SymbolInfo
              { symbolName = reservedName
              , symbolType = Nothing
              , ownershipState = Just (Own.Owned reservedName)
              , symbolScope = 0
              , isMoved = False
              , isBorrowed = False
              , constraints = []
              }
            baseState = newIntegratedAnalyzer True True
            initialState = baseState { symbolTable = Map.singleton reservedName reservedInfo }
            ownershipErrs = [Own.UseAfterMove reservedName]
        runResult <- runExceptT $ runStateT (processOwnershipErrors code ownershipErrs) initialState
        case runResult of
          Left err -> assertFailure ("processOwnershipErrors failed: " <> err)
          Right (labeledErrors, finalState) -> do
            labeledErrors @?= []
            case Map.lookup reservedName (symbolTable finalState) of
              Nothing -> assertFailure "reserved symbol missing from symbol table"
              Just info -> do
                isMoved info @?= False
                isBorrowed info @?= False
    ]
