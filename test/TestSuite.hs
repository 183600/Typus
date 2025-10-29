module Main (main) where

import Test.Tasty (TestTree, defaultMain, testGroup)

import qualified ParserSpec
import qualified OwnershipSpec
import qualified DependentTypesSpec
import qualified CompilerSpec
import qualified CLISpec

main :: IO ()
main = defaultMain allTests

allTests :: TestTree
allTests =
  testGroup "Typus compiler"
    [ ParserSpec.tests
    , OwnershipSpec.tests
    , DependentTypesSpec.tests
    , CompilerSpec.tests
    , CLISpec.tests
    ]
