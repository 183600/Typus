module Main (main) where

import Cli
import CompilerUtils (CompilerContext(..), defaultLogger, newCompilerContext)
import qualified CompilerUtils as CU
import Control.Monad (unless)
import Control.Monad.Except
import Control.Monad.IO.Class (liftIO)
import EmbedAssets (copyEmbeddedForBuild, handleMissingEmbeds, mirrorEmbeddedResources)
import GoToolchain (IOResult, createTempGoFile, withTemporaryGoProject)
import System.Directory (doesDirectoryExist, doesFileExist)
import System.Exit (exitFailure)
import System.FilePath (takeFileName)
import Tooling.Error (ToolingError(..), renderToolingError)

main :: IO ()
main = do
    ctx <- newCompilerContext defaultLogger
    cliArgs <- parseArgs
    result <- runExceptT (dispatch ctx cliArgs)
    case result of
        Left err -> putStrLn ("Error: " ++ renderToolingError err) >> exitFailure
        Right _  -> pure ()

dispatch :: CompilerContext -> Args -> IOResult ()
dispatch ctx (Convert inputPath outputPath) = do
    isDir <- liftIO $ doesDirectoryExist inputPath
    if isDir
        then CU.batchConvert ctx inputPath outputPath
        else CU.convertFile ctx inputPath outputPath

dispatch ctx (Check inputPath) = do
    isDir <- liftIO $ doesDirectoryExist inputPath
    if isDir
        then CU.batchCheck ctx inputPath
        else
            withTemporaryGoProject "typus_check" $ \tempDir -> do
                tempGoPath <- prepareSingleFileProject ctx False inputPath tempDir
                CU.runGoCommandInDir ctx ["build", tempGoPath] tempDir
                liftIO $ putStrLn $ "Typus syntax and compilation OK: " ++ inputPath

dispatch ctx (Build strict buildArgs) = do
    -- Support optional first arg as project path; remaining are passed to `go build`
    let (targetPath, goArgs) = case buildArgs of
            (p:rest) -> (p, rest)
            []       -> (".", [])
    isDir <- liftIO $ doesDirectoryExist targetPath
    isFile <- liftIO $ doesFileExist targetPath
    if isDir
        then withTemporaryGoProject "typus_build" $ \tempDir -> do
                prepareDirectoryProject ctx strict targetPath tempDir
                CU.runGoCommandInDir ctx ("build" : goArgs) tempDir
        else if isFile
            then withTemporaryGoProject "typus_build_single" $ \tempDir -> do
                    _ <- prepareSingleFileProject ctx strict targetPath tempDir
                    CU.runGoCommandInDir ctx ("build" : goArgs) tempDir
            else throwError (PathDoesNotExist targetPath)

dispatch ctx (Run strict runArgs) =
    case runArgs of
        [] -> throwError (InvalidArgument "Please specify a .typus file to run")
        (inputFile:restArgs) -> do
            exists <- liftIO $ doesFileExist inputFile
            unless exists $ throwError (FileNotFound inputFile)
            withTemporaryGoProject "typus_run" $ \tempDir -> do
                tempGoPath <- prepareSingleFileProject ctx strict inputFile tempDir
                let goArgs = "run" : takeFileName tempGoPath : restArgs
                CU.runGoCommandInDir ctx goArgs tempDir

dispatch _ Version =
    liftIO $ putStrLn "typus version 0.1.0"

prepareSingleFileProject :: CompilerContext -> Bool -> FilePath -> FilePath -> IOResult FilePath
prepareSingleFileProject ctx strict sourcePath tempDir = do
    tempGoPath <- createTempGoFile sourcePath tempDir
    CU.convertFile ctx sourcePath tempGoPath
    let logger = contextLogger ctx
    missing <- liftIO $ mirrorEmbeddedResources logger sourcePath tempDir tempGoPath
    handleMissingEmbeds logger strict missing
    pure tempGoPath

prepareDirectoryProject :: CompilerContext -> Bool -> FilePath -> FilePath -> IOResult ()
prepareDirectoryProject ctx strict sourceRoot tempDir = do
    CU.batchConvert ctx sourceRoot tempDir
    let logger = contextLogger ctx
    missing <- liftIO $ copyEmbeddedForBuild logger sourceRoot tempDir
    handleMissingEmbeds logger strict missing
