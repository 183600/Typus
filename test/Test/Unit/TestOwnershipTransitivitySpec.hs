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
      in length result @?= 1
             
  , testCase "analyzeOwnership: multiple ownership transfers" $
      let input = "package main\n\nfunc main() {\n    data := make([]byte, 100)\n    processData(data)\n    moreProcessing(data)\n}\n\nfunc processData(d []byte) {\n    // Process data\n}\n\nfunc moreProcessing(d []byte) {\n    // More processing\n}"
          result = analyzeOwnership input
      in length result @?= 2
             
  , testCase "analyzeOwnership: ownership transfer chain" $
      let input = "package main\n\nfunc main() {\n    data := make([]byte, 100)\n    first(data)\n}\n\nfunc first(d []byte) {\n    second(d)\n}\n\nfunc second(d []byte) {\n    third(d)\n}\n\nfunc third(d []byte) {\n    // Final processing\n}"
          result = analyzeOwnership input
      in length result @?= 3
             
  , testCase "analyzeOwnership: conditional ownership transfer" $
      let input = "package main\n\nfunc main() {\n    data := make([]byte, 100)\n    if condition {\n        processData(data)\n    } else {\n        otherProcess(data)\n    }\n}\n\nfunc processData(d []byte) {\n    // Process data\n}\n\nfunc otherProcess(d []byte) {\n    // Other process\n}"
          result = analyzeOwnership input
      in length result @?= 2
             
  , testCase "analyzeOwnership: loop ownership transfer" $
      let input = "package main\n\nfunc main() {\n    data := make([]byte, 100)\n    for i := 0; i < 10; i++ {\n        processData(data)\n    }\n}\n\nfunc processData(d []byte) {\n    // Process data\n}"
          result = analyzeOwnership input
      in length result @?= 1
             
  , testCase "analyzeOwnership: ownership transfer with return" $
      let input = "package main\n\nfunc main() {\n    data := make([]byte, 100)\n    result := processData(data)\n    moreProcessing(result)\n}\n\nfunc processData(d []byte) []byte {\n    // Process and return data\n    return d\n}\n\nfunc moreProcessing(d []byte) {\n    // More processing\n}"
          result = analyzeOwnership input
      in length result @?= 1
             
  , testCase "analyzeOwnership: ownership transfer with struct field" $
      let input = "package main\n\ntype Data struct {\n    content []byte\n}\n\nfunc main() {\n    data := Data{content: make([]byte, 100)}\n    processData(data.content)\n}\n\nfunc processData(d []byte) {\n    // Process data\n}"
          result = analyzeOwnership input
      in length result @?= 1
             
  , testCase "analyzeOwnership: ownership transfer with channel" $
      let input = "package main\n\nfunc main() {\n    ch := make(chan []byte, 1)\n    data := make([]byte, 100)\n    ch <- data\n    processData(<-ch)\n}\n\nfunc processData(d []byte) {\n    // Process data\n}"
          result = analyzeOwnership input
      in length result @?= 1
             
  , testCase "analyzeOwnership: ownership transfer with slice" $
      let input = "package main\n\nfunc main() {\n    data := make([]byte, 100)\n    slice := data[10:50]\n    processData(slice)\n}\n\nfunc processData(d []byte) {\n    // Process data\n}"
          result = analyzeOwnership input
      in length result @?= 1
             
  , testCase "analyzeOwnership: ownership transfer with map value" $
      let input = "package main\n\nfunc main() {\n    m := make(map[string][]byte)\n    data := make([]byte, 100)\n    m[\"key\"] = data\n    processData(m[\"key\"])\n}\n\nfunc processData(d []byte) {\n    // Process data\n}"
          result = analyzeOwnership input
      in length result @?= 1
             
  , testCase "analyzeOwnership: ownership transfer with interface" $
      let input = "package main\n\ntype Processor interface {\n    Process([]byte)\n}\n\ntype DataProcessor struct{}\n\nfunc (dp DataProcessor) Process(d []byte) {\n    // Process data\n}\n\nfunc main() {\n    data := make([]byte, 100)\n    var processor Processor = DataProcessor{}\n    processor.Process(data)\n}"
          result = analyzeOwnership input
      in length result @?= 1
             
  , testCase "analyzeOwnership: ownership transfer with function pointer" $
      let input = "package main\n\nfunc main() {\n    data := make([]byte, 100)\n    processors := []func([]byte){processData, moreProcessing}\n    for _, processor := range processors {\n        processor(data)\n    }\n}\n\nfunc processData(d []byte) {\n    // Process data\n}\n\nfunc moreProcessing(d []byte) {\n    // More processing\n}"
          result = analyzeOwnership input
      in length result @?= 2
             
  , testCase "analyzeOwnership: ownership transfer with defer" $
      let input = "package main\n\nfunc main() {\n    data := make([]byte, 100)\n    defer processData(data)\n    // Other work\n}\n\nfunc processData(d []byte) {\n    // Process data\n}"
          result = analyzeOwnership input
      in length result @?= 1
             
  , testCase "analyzeOwnership: ownership transfer with goroutine" $
      let input = "package main\n\nfunc main() {\n    data := make([]byte, 100)\n    go processData(data)\n}\n\nfunc processData(d []byte) {\n    // Process data\n}"
          result = analyzeOwnership input
      in length result @?= 1
             
  , testCase "analyzeOwnership: ownership transfer with closure" $
      let input = "package main\n\nfunc main() {\n    data := make([]byte, 100)\n    func(d []byte) {\n        processData(d)\n    }(data)\n}\n\nfunc processData(d []byte) {\n    // Process data\n}"
          result = analyzeOwnership input
      in length result @?= 1
             
  , testCase "analyzeOwnership: no ownership transfer" $
      let input = "package main\n\nfunc main() {\n    data := make([]byte, 100)\n    // Use data without transferring ownership\n    println(len(data))\n}"
          result = analyzeOwnership input
      in length result @?= 0
  ]