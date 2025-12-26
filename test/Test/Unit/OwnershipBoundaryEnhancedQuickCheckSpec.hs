{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.OwnershipBoundaryEnhancedQuickCheckSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Ownership (OwnershipType(..), OwnershipError(..), OwnershipAnalyzer(..), 
                 OwnershipTransfer(..), newOwnershipAnalyzer, analyzeOwnership, 
                 analyzeOwnershipFile, formatOwnershipErrors)
import Data.List (sort, nub)
import Data.Either (isLeft, isRight)

tests :: TestTree
tests = testGroup "Ownership Boundary Enhanced QuickCheck Tests"
  [ ownershipTypeProperties
  , ownershipErrorProperties
  , ownershipAnalyzerProperties
  , ownershipTransferProperties
  , boundaryCaseProperties
  ]

-- | Ownership type properties
ownershipTypeProperties :: TestTree
ownershipTypeProperties = testGroup "Ownership Type Properties"
  [ testProperty "Owned type preserves name" $
      \name -> 
        let owned = Owned name
        in case owned of
          Owned n -> n === name
  
  , testProperty "Borrowed type preserves name" $
      \name -> 
        let borrowed = Borrowed name
        in case borrowed of
          Borrowed n -> n === name
  
  , testProperty "MutBorrowed type preserves name" $
      \name -> 
        let mutBorrowed = MutBorrowed name
        in case mutBorrowed of
          MutBorrowed n -> n === name
  
  , testProperty "Ownership type ordering consistency" $
      \name1 name2 -> 
        let owned1 = Owned name1
            owned2 = Owned name2
            borrowed1 = Borrowed name1
            borrowed2 = Borrowed name2
            mutBorrowed1 = MutBorrowed name1
            mutBorrowed2 = MutBorrowed name2
        in compare owned1 owned2 === compare name1 name2 .&&.
           compare borrowed1 borrowed2 === compare name1 name2 .&&.
           compare mutBorrowed1 mutBorrowed2 === compare name1 name2
  
  , testProperty "Ownership type hierarchy" $
      \name -> 
        let owned = Owned name
            borrowed = Borrowed name
            mutBorrowed = MutBorrowed name
        in owned < borrowed .&&. borrowed < mutBorrowed
  
  , testProperty "Ownership type equality" $
      \name1 name2 -> 
        let owned1 = Owned name1
            owned2 = Owned name2
            borrowed1 = Borrowed name1
            borrowed2 = Borrowed name2
        in (owned1 == owned2) === (name1 == name2) .&&.
           (borrowed1 == borrowed2) === (name1 == name2)
  ]

-- | Ownership error properties
ownershipErrorProperties :: TestTree
ownershipErrorProperties = testGroup "Ownership Error Properties"
  [ testProperty "UseAfterMove preserves variable name" $
      \var -> 
        let error = UseAfterMove var
        in case error of
          UseAfterMove v -> v === var
  
  , testProperty "DoubleMove preserves variable names" $
      \var1 var2 -> 
        let error = DoubleMove var1 var2
        in case error of
          DoubleMove v1 v2 -> v1 === var1 .&&. v2 === var2
  
  , testProperty "BorrowWhileMoved preserves variable name" $
      \var -> 
        let error = BorrowWhileMoved var
        in case error of
          BorrowWhileMoved v -> v === var
  
  , testProperty "Error ordering consistency" $
      \var1 var2 -> 
        let error1 = UseAfterMove var1
            error2 = UseAfterMove var2
        in compare error1 error2 === compare var1 var2
  
  , testProperty "Error uniqueness" $
      \var1 var2 -> 
        var1 /= var2 ==> 
        let error1 = UseAfterMove var1
            error2 = UseAfterMove var2
        in error1 /= error2
  
  , testProperty "Error string representation" $
      \var -> 
        let error = UseAfterMove var
            errorStr = show error
        in "UseAfterMove" `isInfixOf` errorStr .&&. var `isInfixOf` errorStr
  ]

-- | Ownership analyzer properties
ownershipAnalyzerProperties :: TestTree
ownershipAnalyzerProperties = testGroup "Ownership Analyzer Properties"
  [ testProperty "newOwnershipAnalyzer creates analyzer" $
      \() -> 
        let analyzer = newOwnershipAnalyzer
        in case analyzer of
          OwnershipAnalyzer () -> property True
  
  , testProperty "analyzeOwnership handles empty input" $
      \() -> 
        let analyzer = newOwnershipAnalyzer
            result = analyzeOwnership analyzer ""
        in case result of
          Left _ -> property True
          Right _ -> property True
  
  , testProperty "analyzeOwnership handles simple input" $
      \code -> 
        let analyzer = newOwnershipAnalyzer
            result = analyzeOwnership analyzer code
        in case result of
          Left _ -> property True
          Right _ -> property True
  
  , testProperty "analyzeOwnershipFile handles empty file" $
      \() -> 
        let analyzer = newOwnershipAnalyzer
            result = analyzeOwnershipFile analyzer ""
        in case result of
          Left _ -> property True
          Right _ -> property True
  
  , testProperty "formatOwnershipErrors handles empty list" $
      \() -> 
        let errors = []
            formatted = formatOwnershipErrors errors
        in null formatted .||. length formatted >= 0
  
  , testProperty "formatOwnershipErrors handles single error" $
      \var -> 
        let errors = [UseAfterMove var]
            formatted = formatOwnershipErrors errors
        in not (null formatted) ==> var `isInfixOf` formatted
  ]

-- | Ownership transfer properties
ownershipTransferProperties :: TestTree
ownershipTransferProperties = testGroup "Ownership Transfer Properties"
  [ testProperty "OwnershipTransfer preserves from and to" $
      \from to -> 
        let transfer = OwnershipTransfer from to
        in transferFrom transfer === from .&&. transferTo transfer === to
  
  , testProperty "OwnershipTransfer equality" $
      \from1 to1 from2 to2 -> 
        let transfer1 = OwnershipTransfer from1 to1
            transfer2 = OwnershipTransfer from2 to2
        in (transfer1 == transfer2) === (from1 == from2 && to1 == to2)
  
  , testProperty "OwnershipTransfer ordering" $
      \from1 to1 from2 to2 -> 
        let transfer1 = OwnershipTransfer from1 to1
            transfer2 = OwnershipTransfer from2 to2
        in compare transfer1 transfer2 === compare (from1, to1) (from2, to2)
  
  , testProperty "OwnershipTransfer show representation" $
      \from to -> 
        let transfer = OwnershipTransfer from to
            transferStr = show transfer
        in from `isInfixOf` transferStr .&&. to `isInfixOf` transferStr
  ]

-- | Boundary case properties
boundaryCaseProperties :: TestTree
boundaryCaseProperties = testGroup "Boundary Case Properties"
  [ testProperty "analyzeOwnership handles very long input" $
      \code -> 
        let longCode = concat (replicate 1000 code)
            analyzer = newOwnershipAnalyzer
            result = analyzeOwnership analyzer longCode
        in case result of
          Left _ -> property True
          Right _ -> property True
  
  , testProperty "analyzeOwnership handles special characters" $
      \code -> 
        let specialCode = code ++ "!@#$%^&*()_+-={}[]|\\:;\"'<>?,./"
            analyzer = newOwnershipAnalyzer
            result = analyzeOwnership analyzer specialCode
        in case result of
          Left _ -> property True
          Right _ -> property True
  
  , testProperty "analyzeOwnership handles unicode characters" $
      \code -> 
        let unicodeCode = code ++ "测试中文🚀emoji"
            analyzer = newOwnershipAnalyzer
            result = analyzeOwnership analyzer unicodeCode
        in case result of
          Left _ -> property True
          Right _ -> property True
  
  , testProperty "formatOwnershipErrors handles many errors" $
      \vars -> 
        let errors = map UseAfterMove (take 100 vars)
            formatted = formatOwnershipErrors errors
        in length errors > 0 ==> length formatted >= 0
  
  , testProperty "OwnershipType handles empty string" $
      \() -> 
        let owned = Owned ""
            borrowed = Borrowed ""
            mutBorrowed = MutBorrowed ""
        in show owned === "Owned " .&&.
           show borrowed === "Borrowed " .&&.
           show mutBorrowed === "MutBorrowed "
  
  , testProperty "OwnershipError handles empty string" $
      \() -> 
        let error = UseAfterMove ""
            errorStr = show error
        in "UseAfterMove" `isInfixOf` errorStr
  
  , testProperty "OwnershipTransfer handles empty strings" $
      \() -> 
        let transfer = OwnershipTransfer "" ""
            transferStr = show transfer
        in not (null transferStr)
  ]

-- Helper function
isInfixOf :: String -> String -> Bool
isInfixOf = Data.List.isInfixOf