{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.CabalOwnershipQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), elements, listOf)
import qualified Data.List as List
import Data.Char (isSpace, isAlphaNum, isLetter)
import Data.Maybe (isJust, isNothing)

import Ownership (analyzeOwnership, OwnershipError(..))
import Parser (parseTypus)
import SourceLocation (SourceSpan(..))
import Test.QuickCheck (Arbitrary(..), elements, listOf)

-- Simple arbitrary instances for ownership testing
newtype VariableName = VariableName String deriving (Show, Eq)

instance Arbitrary VariableName where
  arbitrary = do
    first <- elements ['a'..'z']
    rest <- listOf $ elements (['a'..'z'] ++ ['0'..'9'] ++ ['_'])
    return $ VariableName (first : rest)

data OwnershipAction = Move | Borrow | Copy deriving (Show, Eq)

instance Arbitrary OwnershipAction where
  arbitrary = elements [Move, Borrow, Copy]

-- Property: Ownership analysis should detect moves correctly
prop_ownership_detects_moves :: VariableName -> Property
prop_ownership_detects_moves (VariableName var) =
  let typusCode = unlines
        [ "//! ownership: on"
        , "func test() {"
        , "    let " ++ var ++ " = String{\"hello\"}"
        , "    let moved = " ++ var
        , "    return moved"
        , "}"
        ]
  in case parseTypus typusCode of
       Left err -> counterexample ("Parse failed: " ++ err) $ property False
       Right parsed -> 
         let errors = analyzeOwnership (show parsed)
         in property $ not $ null errors  -- Ownership errors are expected

-- Property: Ownership analysis should allow borrows
prop_ownership_allows_borrows :: VariableName -> Property
prop_ownership_allows_borrows (VariableName var) =
  let typusCode = unlines
        [ "//! ownership: on"
        , "func test() {"
        , "    let " ++ var ++ " = String{\"hello\"}"
        , "    let borrowed = &" ++ var
        , "    return " ++ var
        , "}"
        ]
  in case parseTypus typusCode of
       Left err -> counterexample ("Parse failed: " ++ err) $ property False
       Right parsed -> 
         let errors = analyzeOwnership (show parsed)
         in property $ null errors  -- Should not fail with borrowing

-- Property: Ownership analysis should track variable lifetimes
prop_ownership_tracks_lifetimes :: VariableName -> Property
prop_ownership_tracks_lifetimes (VariableName var) =
  let typusCode = unlines
        [ "//! ownership: on"
        , "func test() {"
        , "    {"
        , "        let " ++ var ++ " = String{\"hello\"}"
        , "    }"
        , "    return " ++ var  -- This should be an error
        , "}"
        ]
  in case parseTypus typusCode of
       Left err -> counterexample ("Parse failed: " ++ err) $ property False
       Right parsed -> 
         let errors = analyzeOwnership (show parsed)
         in property $ not $ null errors  -- Should fail due to lifetime error

-- Property: Ownership analysis should handle multiple moves
prop_ownership_handles_multiple_moves :: [VariableName] -> Property
prop_ownership_handles_multiple_moves vars =
  let varNames = List.map (\(VariableName v) -> v) vars
      typusCode = unlines $ ["//! ownership: on", "func test() {"] ++
        List.map (\v -> "    let " ++ v ++ " = String{\"hello\"}") varNames ++
        ["    let result = " ++ (if null varNames then "x" else List.head varNames), "}"]
  in case parseTypus typusCode of
       Left err -> counterexample ("Parse failed: " ++ err) $ property False
       Right parsed -> 
         let errors = analyzeOwnership (show parsed)
         in property $ not $ null errors  -- Ownership errors are acceptable

tests :: TestTree
tests = testGroup "Cabal Ownership QuickCheck Tests"
  [ fastProperty "Ownership detects moves correctly" prop_ownership_detects_moves
  , fastProperty "Ownership allows borrows" prop_ownership_allows_borrows
  , fastProperty "Ownership tracks lifetimes" prop_ownership_tracks_lifetimes
  , fastProperty "Ownership handles multiple moves" prop_ownership_handles_multiple_moves
  , testCase "Ownership handles complex transfer scenarios" $ do
      let source = unlines
            [ "//! ownership: on"
            , "func complex_transfer() {"
            , "    let data = String{\"important\"}"
            , "    let processor = create_processor()"
            , "    processor.process(data)  -- Move data to processor"
            , "    return processor.get_result()"
            , "}"
            ]
      case parseTypus source of
        Left err -> assertFailure $ "parseTypus failed: " ++ err
        Right parsed -> 
          let errors = analyzeOwnership (show parsed)
          in if null errors
             then return ()
             else assertFailure $ "analyzeOwnership failed: " ++ show errors
  ]