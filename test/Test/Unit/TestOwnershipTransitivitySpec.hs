{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Unit.TestOwnershipTransitivitySpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Ownership
import Ownership.Common.Types
import SourceLocation (SourcePos(..))
import qualified Data.Text as T
import TestSupport.Arbitrary ()

-- | Test suite for Ownership transitivity properties
testOwnershipTransitivity :: TestTree
testOwnershipTransitivity = testGroup "Ownership Transitivity Tests"
  [ testCase "analyzeOwnership: simple ownership transfer" $
      let input = "package main\n\nfunc main() {\n    data := make([]byte, 100)\n    processData(data)\n}\n\nfunc processData(d []byte) {\n    // Process data\n}"
          result = analyzeOwnership input
      in let errors = result in if null errors then do
             return ()  -- Success case
           else
             assertFailure $ "Ownership analysis failed: " ++ show errors
             
  , testCase "analyzeOwnership: multiple ownership transfers" $
      let input = "package main\n\nfunc main() {\n    data := make([]byte, 100)\n    processData(data)\n    moreProcessing(data)\n}\n\nfunc processData(d []byte) {\n    // Process data\n}\n\nfunc moreProcessing(d []byte) {\n    // More processing\n}"
          result = analyzeOwnership input
      in let errors = result in if null errors then do
             length transfers @?= 2  -- data transferred twice
             
  testCase "analyzeOwnership: ownership transfer chain" $
      let input = "package main\n\nfunc main() {\n    data := make([]byte, 100)\n    first(data)\n}\n\nfunc first(d []byte) {\n    second(d)\n}\n\nfunc second(d []byte) {\n    third(d)\n}\n\nfunc third(d []byte) {\n    // Final processing\n}"
          result = analyzeOwnership input
      in let errors = result in if null errors then do
             length transfers @?= 3  -- data transferred through chain
             
  testCase "analyzeOwnership: conditional ownership transfer" $
      let input = "package main\n\nfunc main() {\n    data := make([]byte, 100)\n    if condition {\n        processData(data)\n    } else {\n        otherProcess(data)\n    }\n}\n\nfunc processData(d []byte) {\n    // Process data\n}\n\nfunc otherProcess(d []byte) {\n    // Other process\n}"
          result = analyzeOwnership input
      in let errors = result in if null errors then do
             length transfers @?= 2  -- data transferred conditionally
             
  testCase "analyzeOwnership: loop ownership transfer" $
      let input = "package main\n\nfunc main() {\n    data := make([]byte, 100)\n    for i := 0; i < 10; i++ {\n        processData(data)\n    }\n}\n\nfunc processData(d []byte) {\n    // Process data\n}"
          result = analyzeOwnership input
      in let errors = result in if null errors then do
             length transfers @?= 1  -- data transferred in loop
             
  , testCase "analyzeOwnership: struct ownership transfer" $
      let input = "package main\n\ntype Data struct {\n    content []byte\n}\n\nfunc main() {\n    d := Data{content: make([]byte, 100)}\n    processData(d)\n}\n\nfunc processData(d Data) {\n    // Process data\n}"
          result = analyzeOwnership input
      in let errors = result in if null errors then do
             length transfers @?= 1  -- struct transferred
             
  , testCase "analyzeOwnership: pointer ownership transfer" $
      let input = "package main\n\nfunc main() {\n    data := make([]byte, 100)\n    processData(&data)\n}\n\nfunc processData(d *[]byte) {\n    // Process data\n}"
          result = analyzeOwnership input
      in let errors = result in if null errors then do
             length transfers @?= 1  -- pointer transferred
             
  , testCase "analyzeOwnership: channel ownership transfer" $
      let input = "package main\n\nfunc main() {\n    ch := make(chan []byte, 1)\n    data := make([]byte, 100)\n    ch <- data\n    received := <-ch\n    processData(received)\n}\n\nfunc processData(d []byte) {\n    // Process data\n}"
          result = analyzeOwnership input
      in let errors = result in if null errors then do
             length transfers @?= 2  -- data transferred through channel
             
  , testCase "analyzeOwnership: closure ownership transfer" $
      let input = "package main\n\nfunc main() {\n    data := make([]byte, 100)\n    func() {\n        processData(data)\n    }()\n}\n\nfunc processData(d []byte) {\n    // Process data\n}"
          result = analyzeOwnership input
      in let errors = result in if null errors then do
             length transfers @?= 1  -- data transferred to closure
             
  , testCase "analyzeOwnership: goroutine ownership transfer" $
      let input = "package main\n\nfunc main() {\n    data := make([]byte, 100)\n    go processData(data)\n}\n\nfunc processData(d []byte) {\n    // Process data\n}"
          result = analyzeOwnership input
      in let errors = result in if null errors then do
             length transfers @?= 1  -- data transferred to goroutine
             
  , testCase "analyzeOwnership: method ownership transfer" $
      let input = "package main\n\ntype Data struct {\n    content []byte\n}\n\nfunc (d Data) process() {\n    // Process data\n}\n\nfunc main() {\n    data := Data{content: make([]byte, 100)}\n    data.process()\n}"
          result = analyzeOwnership input
      in let errors = result in if null errors then do
             length transfers @?= 1  -- data transferred to method
             
  , testCase "analyzeOwnership: interface ownership transfer" $
      let input = "package main\n\ntype Processor interface {\n    process()\n}\n\ntype Data struct {\n    content []byte\n}\n\nfunc (d Data) process() {\n    // Process data\n}\n\nfunc main() {\n    var p Processor = Data{content: make([]byte, 100)}\n    p.process()\n}"
          result = analyzeOwnership input
      in let errors = result in if null errors then do
             length transfers @?= 1  -- data transferred through interface
             
  , testCase "analyzeOwnership: ownership violation detection" $
      let input = "package main\n\nfunc main() {\n    data := make([]byte, 100)\n    processData(data)\n    // Using data after transfer - violation\n    println(len(data))\n}\n\nfunc processData(d []byte) {\n    // Process data\n}"
          result = analyzeOwnership input
      in let errors = result in if null errors then do
             length transfers @?= 1
             -- Should detect ownership violation
             
  , testCase "analyzeOwnership: shared ownership" $
      let input = "package main\n\nfunc main() {\n    data := make([]byte, 100)\n    processData(data)  // Shared access\n    moreProcessing(data)  // Another shared access\n}\n\nfunc processData(d []byte) {\n    // Process data (shared)\n}\n\nfunc moreProcessing(d []byte) {\n    // More processing (shared)\n}"
          result = analyzeOwnership input
      in let errors = result in if null errors then do
             length transfers @?= 2  -- Shared access
             
  , testCase "analyzeOwnership: ownership borrowing" $
      let input = "package main\n\nfunc main() {\n    data := make([]byte, 100)\n    processData(&data)  // Borrowing\n    // Can still use data after borrowing\n    println(len(data))\n}\n\nfunc processData(d *[]byte) {\n    // Process data (borrowed)\n}"
          result = analyzeOwnership input
      in let errors = result in if null errors then do
             length transfers @?= 1  -- Borrowed access
  ]