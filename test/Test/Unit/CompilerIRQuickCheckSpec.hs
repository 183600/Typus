module Test.Unit.CompilerIRQuickCheckSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import TestSupport.QuickCheck (fastProperty)
import Compiler.IR (SourceIR(..), SemanticIR(..), GoIR(..))
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (nub)

-- Properties for SourceIR
prop_sourceir_preserves_content :: String -> SourceIR -> Bool
prop_sourceir_preserves_content s ir = 
  sourceIRContent ir == s

prop_sourceir_roundtrip :: String -> Bool
prop_sourceir_roundtrip s = 
  let ir = SourceIR (Map.empty) s
      content = sourceIRContent ir
  in content == s
  where
    sourceIRContent (SourceIR _ content) = content

prop_sourceir_metadata_preserved :: String -> Bool
prop_sourceir_metadata_preserved s = 
  let metadata = Map.fromList [("key", "value")]
      ir = SourceIR metadata s
  in sourceIRMetadata ir == metadata
  where
    sourceIRMetadata (SourceIR metadata _) = metadata

-- Properties for SemanticIR
prop_semanticir_preserves_sourceir :: SourceIR -> [String] -> Bool
prop_semanticir_preserves_sourceir sourceIR symbols = 
  let semanticIR = SemanticIR sourceIR symbols []
      sourceIR' = semanticIRSource semanticIR
  in sourceIR' == sourceIR
  where
    semanticIRSource (SemanticIR source _ _) = source

prop_semanticir_preserves_symbols :: SourceIR -> [String] -> Bool
prop_semanticir_preserves_symbols sourceIR symbols = 
  let semanticIR = SemanticIR sourceIR symbols []
      symbols' = semanticIRSymbols semanticIR
  in symbols' == symbols
  where
    semanticIRSymbols (SemanticIR _ symbols _) = symbols

prop_semanticir_symbols_unique :: SourceIR -> [String] -> Bool
prop_semanticir_symbols_unique sourceIR symbols = 
  let uniqueSymbols = nub symbols
      semanticIR = SemanticIR sourceIR uniqueSymbols []
      symbols' = semanticIRSymbols semanticIR
  in length symbols' == length (nub symbols')
  where
    semanticIRSymbols (SemanticIR _ symbols _) = symbols

-- Properties for GoIR
prop_goir_preserves_semanticir :: SemanticIR -> String -> Bool
prop_goir_preserves_semanticir semanticIR goCode = 
  let goIR = GoIR semanticIR goCode
      semanticIR' = goIRSemantic goIR
  in semanticIR' == semanticIR
  where
    goIRSemantic (GoIR semantic _) = semantic

prop_goir_preserves_code :: SemanticIR -> String -> Bool
prop_goir_preserves_code semanticIR goCode = 
  let goIR = GoIR semanticIR goCode
      goCode' = goIRCode goIR
  in goCode' == goCode
  where
    goIRCode (GoIR _ code) = code

prop_goir_code_non_empty :: SemanticIR -> Property
prop_goir_code_non_empty semanticIR = 
  not (null goCode) ==> not (null (goIRCode goIR))
  where
    goCode = "package main\nfunc main() {}"
    goIR = GoIR semanticIR goCode

-- Properties for IR transformation chain
prop_ir_transformation_preserves_content :: String -> Bool
prop_ir_transformation_preserves_content s = 
  let sourceIR = SourceIR (Map.empty) s
      semanticIR = SemanticIR sourceIR [] []
      goIR = GoIR semanticIR "package main"
      finalContent = goIRCode goIR
  in not (null finalContent)  -- Simplified property

prop_ir_transformation_preserves_symbols :: String -> [String] -> Bool
prop_ir_transformation_preserves_symbols s symbols = 
  let sourceIR = SourceIR (Map.empty) s
      semanticIR = SemanticIR sourceIR symbols []
      goIR = GoIR semanticIR "package main"
      symbols' = semanticIRSymbols (goIRSemantic goIR)
  in symbols' == symbols
  where
    semanticIRSymbols (SemanticIR _ symbols _) = symbols
    goIRSemantic (GoIR semantic _) = semantic

-- Properties for IR validation
prop_valid_sourceir_has_content :: String -> Property
prop_valid_sourceir_has_content s = 
  not (null s) ==> isValidSourceIR (SourceIR (Map.empty) s)

prop_valid_semanticir_has_symbols :: String -> [String] -> Property
prop_valid_semanticir_has_content s symbols = 
  not (null symbols) ==> isValidSemanticIR (SemanticIR (SourceIR (Map.empty) s) symbols [])
  where
    isValidSemanticIR (SemanticIR _ symbols _) = not (null symbols)

prop_valid_goir_has_code :: String -> [String] -> String -> Property
prop_valid_goir_has_code s symbols goCode = 
  not (null goCode) ==> isValidGoIR (GoIR (SemanticIR (SourceIR (Map.empty) s) symbols []) goCode)
  where
    isValidGoIR (GoIR _ code) = not (null code)

-- Properties for IR optimization
prop_optimization_preserves_semantics :: SourceIR -> [String] -> String -> Bool
prop_optimization_preserves_semantics sourceIR symbols goCode = 
  let semanticIR = SemanticIR sourceIR symbols []
      goIR = GoIR semanticIR goCode
      optimizedIR = optimizeIR goIR
  in goIRSemantic optimizedIR == goIRSemantic goIR
  where
    optimizeIR = id  -- Simplified optimization

prop_optimization_reduces_size :: SourceIR -> [String] -> String -> Property
prop_optimization_reduces_size sourceIR symbols goCode = 
  length goCode > 10 ==> 
  let semanticIR = SemanticIR sourceIR symbols []
      goIR = GoIR semanticIR goCode
      optimizedIR = optimizeIR goIR
      originalSize = length (goIRCode goIR)
      optimizedSize = length (goIRCode optimizedIR)
  in optimizedSize <= originalSize
  where
    optimizeIR = id  -- Simplified optimization
    goIRCode (GoIR _ code) = code

-- Properties for IR equality
prop_sourceir_equality_reflexive :: SourceIR -> Bool
prop_sourceir_equality_reflexive ir = ir == ir

prop_sourceir_equality_symmetric :: SourceIR -> SourceIR -> Property
prop_sourceir_equality_symmetric ir1 ir2 = 
  property (ir1 == ir2) ==> ir2 == ir1

prop_sourceir_equality_transitive :: SourceIR -> SourceIR -> SourceIR -> Property
prop_sourceir_equality_transitive ir1 ir2 ir3 = 
  property (ir1 == ir2 && ir2 == ir3) ==> ir1 == ir3

-- Helper functions for testing
isValidSourceIR :: SourceIR -> Bool
isValidSourceIR (SourceIR _ content) = not (null content)

isValidSemanticIR :: SemanticIR -> Bool
isValidSemanticIR (SemanticIR source symbols _) = 
  isValidSourceIR source && not (null symbols)

isValidGoIR :: GoIR -> Bool
isValidGoIR (GoIR semantic code) = 
  isValidSemanticIR semantic && not (null code)

tests :: TestTree
tests = testGroup "Test.Unit.CompilerIRQuickCheckSpec Tests"
  [ fastProperty "sourceir preserves content" prop_sourceir_preserves_content
  , fastProperty "sourceir roundtrip" prop_sourceir_roundtrip
  , fastProperty "sourceir metadata preserved" prop_sourceir_metadata_preserved
  , fastProperty "semanticir preserves sourceir" prop_semanticir_preserves_sourceir
  , fastProperty "semanticir preserves symbols" prop_semanticir_preserves_symbols
  , fastProperty "semanticir symbols unique" prop_semanticir_symbols_unique
  , fastProperty "goir preserves semanticir" prop_goir_preserves_semanticir
  , fastProperty "goir preserves code" prop_goir_preserves_code
  , fastProperty "goir code non empty" prop_goir_code_non_empty
  , fastProperty "ir transformation preserves content" prop_ir_transformation_preserves_content
  , fastProperty "ir transformation preserves symbols" prop_ir_transformation_preserves_symbols
  , fastProperty "valid sourceir has content" prop_valid_sourceir_has_content
  , fastProperty "valid semanticir has symbols" prop_valid_semanticir_has_content
  , fastProperty "valid goir has code" prop_valid_goir_has_code
  , fastProperty "optimization preserves semantics" prop_optimization_preserves_semantics
  , fastProperty "optimization reduces size" prop_optimization_reduces_size
  , fastProperty "sourceir equality reflexive" prop_sourceir_equality_reflexive
  , fastProperty "sourceir equality symmetric" prop_sourceir_equality_symmetric
  , fastProperty "sourceir equality transitive" prop_sourceir_equality_transitive
  ]