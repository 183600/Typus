module GoToolchain
    ( IOResult
    , GoExecutor(..)
    , defaultGoExecutor
    , runGoCommand
    , goModContents
    , writeGoModule
    , withTemporaryGoProject
    , createTempGoFile
    , nullDevice
    , isEnvVarEnabled
    , shouldSkipGoToolchain
    ) where

import Control.Concurrent.MVar (MVar, modifyMVar, newMVar)
import Control.Monad (unless)
import Control.Monad.Except (ExceptT(..), throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Char (toLower)
import System.Directory (createDirectoryIfMissing, findExecutable)
import System.Environment (lookupEnv)
import System.Exit (ExitCode(..))
import System.FilePath ((</>), takeBaseName)
import System.IO (hClose)
import System.IO.Temp (withSystemTempDirectory, openTempFile)
import System.Info (os)
import System.Process (CreateProcess(cwd), proc, readCreateProcessWithExitCode)

-- | Common IO result type used across the CLI utilities.
type IOResult a = ExceptT String IO a

-- | Abstraction over executing Go tooling. The record can be replaced with
-- fake implementations in tests.
data GoExecutor = GoExecutor
    { goShouldSkip :: IO Bool
    , goRunCommandInDir :: [String] -> FilePath -> IOResult ()
    }

-- | Create the default Go executor that shells out to the system `go` binary.
-- A logging function is injected so callers can control where messages go.
defaultGoExecutor :: (String -> IO ()) -> IO GoExecutor
defaultGoExecutor logFn = do
    cache <- newMVar Nothing
    pure GoExecutor
        { goShouldSkip = shouldSkipGoToolchain
        , goRunCommandInDir = runGoCommandImpl cache logFn
        }
  where
    runGoCommandImpl :: MVar (Maybe (Either String ())) -> (String -> IO ()) -> [String] -> FilePath -> IOResult ()
    runGoCommandImpl availabilityCache logger args dir = do
        skip <- liftIO shouldSkipGoToolchain
        if skip
            then liftIO $ logger $ "Skipping Go command: go " ++ unwords args ++ " (TYPUS_SKIP_GO_BUILD is enabled)."
            else do
                ensureGoAvailable availabilityCache
                let processSpec = (proc "go" args) { cwd = Just dir }
                (exitCode, stdout, stderr) <- liftIO $ readCreateProcessWithExitCode processSpec ""
                case exitCode of
                    ExitSuccess ->
                        liftIO $ unless (null stdout) (logger stdout)
                    ExitFailure code -> do
                        let cmd = "go " ++ unwords args
                        throwError $ cmd ++ " failed with exit code " ++ show code ++ ".\nStdout: " ++ stdout ++ "\nStderr: " ++ stderr

    ensureGoAvailable :: MVar (Maybe (Either String ())) -> IOResult ()
    ensureGoAvailable availabilityCache = do
        result <- liftIO $ modifyMVar availabilityCache $ \cached ->
            case cached of
                Just r  -> pure (cached, r)
                Nothing -> do
                    found <- findExecutable "go"
                    let r = maybe
                            (Left "Go is not installed or not in PATH. Please install Go.")
                            (const (Right ()))
                            found
                    pure (Just r, r)
        case result of
            Left err -> throwError err
            Right () -> pure ()

-- | Convenience helper to run a Go command in the current directory.
runGoCommand :: GoExecutor -> [String] -> IOResult ()
runGoCommand executor args = goRunCommandInDir executor args "."

goModContents :: String
goModContents = "module temp\n\ngo 1.21\n"

writeGoModule :: FilePath -> IOResult ()
writeGoModule dir = do
    let goModPath = dir </> "go.mod"
    liftIO $ createDirectoryIfMissing True dir
    liftIO $ writeFile goModPath goModContents

withTemporaryGoProject :: String -> (FilePath -> IOResult a) -> IOResult a
withTemporaryGoProject prefix action =
    ExceptT $ withSystemTempDirectory prefix $ \tempDir ->
        runExceptT $ do
            writeGoModule tempDir
            action tempDir

createTempGoFile :: FilePath -> FilePath -> IOResult FilePath
createTempGoFile sourcePath tempDir = do
    let baseName = takeBaseName sourcePath
        prefix = if null baseName then "typus" else baseName
        template = prefix ++ "-XXXXXX.go"
    (tempPath, handle) <- liftIO $ openTempFile tempDir template
    liftIO $ hClose handle
    pure tempPath

nullDevice :: FilePath
nullDevice = if os == "mingw32" then "NUL" else "/dev/null"

isEnvVarEnabled :: String -> IO Bool
isEnvVarEnabled name = do
    value <- lookupEnv name
    pure $ case fmap (map toLower) value of
        Just "1"    -> True
        Just "true" -> True
        Just "yes"  -> True
        Just "on"   -> True
        _            -> False

shouldSkipGoToolchain :: IO Bool
shouldSkipGoToolchain = isEnvVarEnabled "TYPUS_SKIP_GO_BUILD"
