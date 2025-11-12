module Test.Unit.SymbolTableSpec (tests) where

import Analyzer.State (newIntegratedAnalyzer)
import Analyzer.SymbolTable (augmentSymbolTableWithLocals, collectSymbolsAndTypes)
import Analyzer.Types (SymbolInfo(..))
import Control.Monad.Except (runExceptT)
import Control.Monad.State (evalStateT)
import qualified Data.Map.Strict as Map
import Data.List (isInfixOf)
import qualified Dependencies as Dep
import qualified Ownership as Own
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

runSymbolCollection :: String -> IO (Either String (Map.Map String SymbolInfo))
runSymbolCollection source =
  runExceptT $
    evalStateT (collectSymbolsAndTypes source) (newIntegratedAnalyzer True True)

requireSymbol :: String -> Map.Map String SymbolInfo -> IO SymbolInfo
requireSymbol name table =
  case Map.lookup name table of
    Nothing -> do
      assertFailure ("expected symbol '" <> name <> "' to be present")
      fail "unreachable"
    Just info -> pure info

tests :: TestTree
tests =
  testGroup "SymbolTable"
    [ testCase "collectSymbolsAndTypes extracts top-level declarations" $ do
        let source = unlines
              [ "package main"
              , ""
              , "var globalCounter int = 0"
              , "const Pi = 3.14"
              , ""
              , "type Widget struct {"
              , "    value int"
              , "}"
              , ""
              , "func helper() {"
              , "    globalCounter = globalCounter + 1"
              , "}"
              , ""
              , "func main() {"
              , "    helper()"
              , "}"
              ]
        result <- runSymbolCollection source
        case result of
          Left err -> assertFailure ("collectSymbolsAndTypes failed: " <> err)
          Right table -> do
            globalInfo <- requireSymbol "globalCounter" table
            symbolScope globalInfo @?= 0
            symbolType globalInfo @?= Just (Dep.TVCon "int")
            ownershipState globalInfo @?= Just (Own.Owned "globalCounter")

            constInfo <- requireSymbol "Pi" table
            symbolScope constInfo @?= 0
            symbolType constInfo @?= Just (Dep.TVCon "const")
            ownershipState constInfo @?= Just (Own.Owned "Pi")

            typeInfo <- requireSymbol "Widget" table
            symbolScope typeInfo @?= 0
            symbolType typeInfo @?= Just (Dep.TVCon "Widget")
            ownershipState typeInfo @?= Nothing

            helperInfo <- requireSymbol "helper" table
            symbolScope helperInfo @?= 0
            symbolType helperInfo @?= Just (Dep.TVFun [] (Dep.TVCon "void"))
            ownershipState helperInfo @?= Nothing

            _ <- requireSymbol "main" table
            pure ()

    , testCase "augmentSymbolTableWithLocals adds owned locals" $ do
        let source = unlines
              [ "package main"
              , ""
              , "func provide() owned String {"
              , "    return \"value\""
              , "}"
              , ""
              , "func main() {"
              , "    var safe = provide()"
              , "    {"
              , "        var local owned String = provide()"
              , "    }"
              , "    for i := 0; i < 1; i++ {"
              , "        var loopVar owned String = provide()"
              , "    }"
              , "}"
              ]
        result <- runSymbolCollection source
        case result of
          Left err -> assertFailure ("collectSymbolsAndTypes failed: " <> err)
          Right baseTable -> do
            assertBool "local should not exist before augmentation" (Map.notMember "local" baseTable)
            assertBool "loopVar should not exist before augmentation" (Map.notMember "loopVar" baseTable)
            let augmented = augmentSymbolTableWithLocals source baseTable

            localInfo <- requireSymbol "local" augmented
            symbolScope localInfo @?= 2
            symbolType localInfo @?= Just (Dep.TVCon "owned String")
            ownershipState localInfo @?= Just (Own.Owned "local")

            loopInfo <- requireSymbol "loopVar" augmented
            symbolScope loopInfo @?= 2
            symbolType loopInfo @?= Just (Dep.TVCon "owned String")
            ownershipState loopInfo @?= Just (Own.Owned "loopVar")

            assertBool "safe should remain absent" (Map.notMember "safe" augmented)

    , testCase "collectSymbolsAndTypes surfaces parse errors" $ do
        let broken = unlines
              [ "package main"
              , "func main() {"
              , "    var x = 1"
              ]
        result <- runSymbolCollection broken
        case result of
          Left err -> assertBool "expected Go AST parse failure" ("Go AST parsing failed" `isInfixOf` err)
          Right _ -> assertFailure "expected collectSymbolsAndTypes to fail on malformed input"
    ]
