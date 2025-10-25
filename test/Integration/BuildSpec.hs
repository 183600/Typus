module Integration.BuildSpec (
    runFullProjectBuildIntegrationTest
) where

import Test.Tasty
import Test.Tasty.HUnit as HU
import System.Exit (ExitCode(..))
import System.Directory
    ( doesDirectoryExist
    , createDirectoryIfMissing
    , listDirectory
    , doesFileExist
    , copyFile
    , findExecutable
    )
import System.FilePath ((</>))
import System.Process (readProcessWithExitCode)
import System.IO.Temp (withSystemTempDirectory)
import Control.Monad (forM_, when)

runFullProjectBuildIntegrationTest :: IO ()
runFullProjectBuildIntegrationTest = defaultMain buildTestSuite

buildTestSuite :: TestTree
buildTestSuite = testGroup "Integration: Full project build"
    [ testCase "typus build builds a full example project end-to-end" fullBuildTest
    ]

fullBuildTest :: IO ()
fullBuildTest = do
    -- Detect Go toolchain
    goAvail <- isGoAvailable
    if not goAvail
      then do
        putStrLn "[SKIP] Go toolchain not found in PATH; skipping full-project build integration test."
        return ()
      else do
        let fixtureRoot = "test" </> "fixtures" </> "full_project"
        exists <- doesDirectoryExist fixtureRoot
        HU.assertBool ("Fixture project not found: " ++ fixtureRoot) exists

        withSystemTempDirectory "typus_full_project" $ \tmpDir -> do
          -- Copy fixture project into temp dir
          copyDirRecursive fixtureRoot tmpDir

          -- Run typus build on the temp project root
          putStrLn $ "[INFO] Building full project in temp dir: " ++ tmpDir
          (ec, out, err) <- readProcessWithExitCode "stack" ["exec","--","typus","build", tmpDir] ""

          case ec of
            ExitSuccess -> do
              putStrLn "[OK] Full project build succeeded."
              when (not (null out)) $ putStrLn ("[stdout]\n" ++ out)
              return ()
            ExitFailure code -> do
              putStrLn "[ERROR] Full project build failed. Dumping logs:"
              when (not (null out)) $ putStrLn ("[stdout]\n" ++ out)
              when (not (null err)) $ putStrLn ("[stderr]\n" ++ err)
              HU.assertFailure $ "typus build failed with exit code " ++ show code

-- Simple recursive directory copy (files only)
copyDirRecursive :: FilePath -> FilePath -> IO ()
copyDirRecursive src dst = do
  createDirectoryIfMissing True dst
  entries <- listDirectory src
  forM_ entries $ \name -> do
    let s = src </> name
    let d = dst </> name
    isDir <- doesDirectoryExist s
    if isDir
      then copyDirRecursive s d
      else do
        isFile <- doesFileExist s
        when isFile $ copyFile s d
        return ()

-- Detect if Go toolchain is available in PATH
isGoAvailable :: IO Bool
isGoAvailable = do
  m <- findExecutable "go"
  return (m /= Nothing)
