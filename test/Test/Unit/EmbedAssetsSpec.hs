module Test.Unit.EmbedAssetsSpec (tests) where

import CompilerUtils (Logger(..), silentLogger)
import Control.Monad (when)
import Control.Monad.Trans.Except (runExceptT)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.List (isInfixOf)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

import EmbedAssets
  ( MissingEmbed(..)
  , copyEmbeddedForBuild
  , handleMissingEmbeds
  , mirrorEmbeddedResources
  )
import Tooling.Error (ToolingError(..), MissingEmbedInfo(..))

-- | Tests for embedded asset helpers used by the CLI build and run commands.
tests :: TestTree
tests =
  testGroup "Embedded assets"
    [ testCase "handleMissingEmbeds warns but succeeds in non-strict mode" $ do
        (logger, readLogs) <- recordingLogger
        let missing = [MissingEmbed "assets/config.json" "/project" "main.typus"]
        result <- runExceptT (handleMissingEmbeds logger False missing)
        result @?= Right ()
        logs <- readLogs
        assertBool "expected warning about missing assets"
          (any ("Missing embedded assets" `isInfixOf`) logs)

    , testCase "handleMissingEmbeds fails fast when strict mode is enabled" $ do
        (logger, _) <- recordingLogger
        let missing = [MissingEmbed "assets/config.json" "/project" "main.typus"]
        result <- runExceptT (handleMissingEmbeds logger True missing)
        case result of
          Left (MissingEmbeddedAssets infos) ->
            infos @?=
              [ MissingEmbedInfo
                  { meiPattern = "assets/config.json"
                  , meiRoot = "/project"
                  , meiReference = "main.typus"
                  }
              ]
          Left other -> assertFailure $ "unexpected tooling error: " ++ show other
          Right _ -> assertFailure "expected strict mode to fail with missing embeds"

    , testCase "mirrorEmbeddedResources copies referenced files" $ do
        withSystemTempDirectory "embed-assets" $ \root -> do
          let srcDir = root </> "src"
              sourcePath = srcDir </> "main.typus"
              assetPath = srcDir </> "assets" </> "hello.txt"
              tempDir = root </> "out"
              tempGoPath = tempDir </> "main.go"
          createDirectoryIfMissing True (srcDir </> "assets")
          createDirectoryIfMissing True tempDir
          writeFile sourcePath "package main"
          writeFile assetPath "hello"
          writeFile tempGoPath $ unlines
            [ "package main"
            , "//go:embed assets/hello.txt"
            , "var data string"
            ]

          missing <- mirrorEmbeddedResources silentLogger sourcePath tempDir tempGoPath
          missing @?= []

          let copiedPath = tempDir </> "assets" </> "hello.txt"
          copiedExists <- doesFileExist copiedPath
          assertBool "expected embedded file to be mirrored" copiedExists
          when copiedExists $ do
            contents <- readFile copiedPath
            contents @?= "hello"

    , testCase "mirrorEmbeddedResources reports missing patterns" $ do
        withSystemTempDirectory "embed-assets" $ \root -> do
          let srcDir = root </> "src"
              sourcePath = srcDir </> "main.typus"
              tempDir = root </> "out"
              tempGoPath = tempDir </> "main.go"
          createDirectoryIfMissing True srcDir
          createDirectoryIfMissing True tempDir
          writeFile sourcePath "package main"
          writeFile tempGoPath $ unlines
            [ "package main"
            , "//go:embed assets/missing.txt"
            , "var data string"
            ]

          missing <- mirrorEmbeddedResources silentLogger sourcePath tempDir tempGoPath
          missing @?=
            [ MissingEmbed
                { missingPattern = "assets/missing.txt"
                , missingRoot = srcDir
                , missingReferencedFrom = sourcePath
                }
            ]

    , testCase "copyEmbeddedForBuild mirrors directories for go:embed patterns" $ do
        withSystemTempDirectory "embed-assets" $ \root -> do
          let inputRoot = root </> "project"
              tempRoot = root </> "output"
              moduleDir = inputRoot </> "module"
              tempModuleDir = tempRoot </> "module"
              assetDir = moduleDir </> "assets"
              goPath = tempModuleDir </> "module.go"
          createDirectoryIfMissing True assetDir
          createDirectoryIfMissing True tempModuleDir
          writeFile (moduleDir </> "file.typus") "package main"
          writeFile (assetDir </> "a.txt") "A"
          writeFile (assetDir </> "b.txt") "B"
          writeFile goPath $ unlines
            [ "package main"
            , "//go:embed assets/*.txt"
            , "var files embed.FS"
            ]

          missing <- copyEmbeddedForBuild silentLogger inputRoot tempRoot
          missing @?= []

          let copiedA = tempModuleDir </> "assets" </> "a.txt"
              copiedB = tempModuleDir </> "assets" </> "b.txt"
          aExists <- doesFileExist copiedA
          bExists <- doesFileExist copiedB
          assertBool "expected a.txt to be copied" aExists
          assertBool "expected b.txt to be copied" bExists
          when aExists $ readFile copiedA >>= (@?= "A")
          when bExists $ readFile copiedB >>= (@?= "B")
    ]

recordingLogger :: IO (Logger, IO [String])
recordingLogger = do
  ref <- newIORef []
  let append prefix msg = modifyIORef' ref (\xs -> xs ++ [prefix ++ msg])
  pure
    ( Logger
        { logInfo = append "info: "
        , logDebug = append "debug: "
        , logWarning = append "warn: "
        }
    , readIORef ref
    )
