module Test.Golden.CompilerSpec (tests) where

import qualified Data.ByteString.Lazy.Char8 as BL8
import System.FilePath ((</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsStringDiff)
import Test.Tasty.HUnit (assertFailure)

import qualified Compiler
import qualified Parser

fixtureDir :: FilePath
fixtureDir = "test" </> "data"

goldenDir :: FilePath
goldenDir = "test" </> "golden" </> "fixtures"

parseFixture :: FilePath -> IO Parser.TypusFile
parseFixture name = do
  contents <- readFile (fixtureDir </> name)
  case Parser.parseTypus contents of
    Left err -> assertFailure ("parseTypus failed: " <> err)
    Right parsed -> pure parsed

compileFixture :: FilePath -> IO BL8.ByteString
compileFixture name = do
  typusFile <- parseFixture name
  case Compiler.compile typusFile of
    Left err -> assertFailure ("compile failed: " <> Compiler.renderCompilationError err)
    Right goSrc -> pure (BL8.pack goSrc)

tests :: TestTree
tests =
  testGroup "Golden"
    [ goldenVsStringDiff
        "simple program renders canonical Go"
        diffCmd
        (goldenDir </> "simple_go_code.go")
        (compileFixture "simple_go_code.typus")
    , goldenVsStringDiff
        "statements without package/import infer a runnable main"
        diffCmd
        (goldenDir </> "statements_without_package.go")
        (compileFixture "statements_without_package.typus")
    , goldenVsStringDiff
        "generic type parameters render Go square bracket notation"
        diffCmd
        (goldenDir </> "generic_type.go")
        (compileFixture "generic_type.typus")
    ]

  where
    diffCmd ref new = ["diff", "-u", ref, new]
