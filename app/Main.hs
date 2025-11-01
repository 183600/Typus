module Main (main) where

import Cli
import CompilerUtils
import Control.Monad (unless, forM_, forM)
import Control.Monad.Except
import Control.Monad.IO.Class (liftIO)
import System.Directory (doesFileExist, doesDirectoryExist, copyFile, createDirectoryIfMissing, listDirectory)
import System.FilePath
import System.IO.Temp (withSystemTempDirectory, openTempFile)
import System.IO (hClose)
import System.Exit (exitFailure)
import Data.List (isPrefixOf, nub)
import Data.Char (isSpace)
import System.FilePath.Glob (glob)

-- Define the IOResult type alias
type IOResult = ExceptT String IO

data MissingEmbed = MissingEmbed
    { missingPattern :: String
    , missingRoot :: FilePath
    , missingReferencedFrom :: FilePath
    } deriving (Eq, Ord, Show)

formatMissingMessage :: [MissingEmbed] -> String
formatMissingMessage missing =
    let uniqueMissing = nub missing
        header = "Missing embedded assets detected:"
        toLine (MissingEmbed pat root ref) =
            "  pattern \"" ++ pat ++ "\" relative to " ++ root ++ " (referenced in " ++ ref ++ ")"
    in unlines (header : map toLine uniqueMissing)

warnMissingEmbeds :: [MissingEmbed] -> IO ()
warnMissingEmbeds missing =
    unless (null missing) $ do
        putStrLn (formatMissingMessage missing)
        putStrLn "Continuing because strict embed mode is disabled."

handleMissingEmbeds :: Bool -> [MissingEmbed] -> IOResult ()
handleMissingEmbeds strict missing
    | null missing = pure ()
    | strict = throwError (formatMissingMessage missing)
    | otherwise = liftIO $ warnMissingEmbeds missing

-- Extract embedded file patterns from Go code
-- Extract raw patterns from //go:embed directives (supports multiple patterns per line)
extractEmbeddedPatterns :: String -> [String]
extractEmbeddedPatterns content =
    [ normalize token
    | line <- lines content
    , "//go:embed" `isPrefixOf` dropWhile isSpace line
    , token <- words (dropWhile isSpace (drop 11 line))
    , not (null token)
    ]
  where
    normalize t =
      case stripQuoted '"' t of
        Just s  -> s
        Nothing -> case stripQuoted '`' t of
                      Just s' -> s'
                      Nothing -> t
    stripQuoted :: Char -> String -> Maybe String
    stripQuoted q s = case s of
      (c:xs) | c == q -> case unsnoc xs of
                            Just (body, qc) | qc == q -> Just body
                            _                          -> Nothing
      _               -> Nothing
    unsnoc :: [a] -> Maybe ([a], a)
    unsnoc []       = Nothing
    unsnoc [x]      = Just ([], x)
    unsnoc (x:xs)   = do (ys, z) <- unsnoc xs
                         pure (x:ys, z)

-- Copy embedded files to temporary directory
copyEmbeddedFiles :: FilePath -> FilePath -> FilePath -> String -> IO [MissingEmbed]
copyEmbeddedFiles sourceDir destDir reference sourceContent = do
    let patterns = extractEmbeddedPatterns sourceContent
    fmap concat $
        forM patterns $ \pat -> do
            let absPattern = sourceDir </> pat
            matches <- glob absPattern
            if not (null matches)
              then do
                  forM_ matches $ \src -> do
                      let rel = makeRelative sourceDir src
                          dest = destDir </> rel
                      createDirectoryIfMissing True (takeDirectory dest)
                      copyFile src dest
                      putStrLn $ "Copied embedded file: " ++ src ++ " -> " ++ dest
                  pure []
              else do
                  let asDir = sourceDir </> pat
                  isDir <- doesDirectoryExist asDir
                  if isDir
                    then do
                        files <- listFilesRecursively asDir
                        forM_ files $ \src -> do
                            let rel = makeRelative sourceDir src
                                dest = destDir </> rel
                            createDirectoryIfMissing True (takeDirectory dest)
                            copyFile src dest
                            putStrLn $ "Copied embedded dir file: " ++ src ++ " -> " ++ dest
                        pure []
                    else do
                        putStrLn $ "Warning: No embedded files matched pattern: " ++ pat ++ " under " ++ sourceDir
                        pure [MissingEmbed pat sourceDir reference]

-- Recursively list files under a directory
listFilesRecursively :: FilePath -> IO [FilePath]
listFilesRecursively dir = do
    names <- listDirectory dir
    paths <- forM names $ \n -> do
        let p = dir </> n
        isDir <- doesDirectoryExist p
        if isDir then listFilesRecursively p else return [p]
    return (concat paths)

-- For directory builds: scan converted Go files in temp tree and mirror embedded assets
copyEmbeddedForBuild :: FilePath -> FilePath -> IO [MissingEmbed]
copyEmbeddedForBuild inputRoot tempRoot = do
    goFiles <- listGoFiles tempRoot
    fmap concat $
        forM goFiles $ \goOut -> do
            content <- readFile goOut
            let relDir = makeRelative tempRoot (takeDirectory goOut)
                srcDir = inputRoot </> relDir
                destDir = tempRoot  </> relDir
                reference = makeRelative tempRoot goOut
            copyEmbeddedFiles srcDir destDir reference content

listGoFiles :: FilePath -> IO [FilePath]
listGoFiles dir = do
    names <- listDirectory dir
    paths <- forM names $ \n -> do
        let p = dir </> n
        isDir <- doesDirectoryExist p
        if isDir then listGoFiles p else return [p]
    return [ p | p <- concat paths, takeExtension p == ".go" ]

goModContents :: String
goModContents = "module temp\n\ngo 1.21\n"

withTemporaryGoProject :: String -> (FilePath -> IOResult a) -> IOResult a
withTemporaryGoProject prefix action =
    ExceptT $
        withSystemTempDirectory prefix $ \tempDir ->
            runExceptT $ do
                writeGoModule tempDir
                action tempDir

writeGoModule :: FilePath -> IOResult ()
writeGoModule dir = liftIO $ writeFile (dir </> "go.mod") goModContents

createTempGoFile :: FilePath -> FilePath -> IOResult FilePath
createTempGoFile sourcePath tempDir = do
    let baseName = takeBaseName sourcePath
        prefix = if null baseName then "typus" else baseName
        template = prefix ++ "-XXXXXX.go"
    (tempPath, handle) <- liftIO $ openTempFile tempDir template
    liftIO $ hClose handle
    return tempPath

prepareSingleFileProject :: Bool -> FilePath -> FilePath -> IOResult FilePath
prepareSingleFileProject strict sourcePath tempDir = do
    tempGoPath <- createTempGoFile sourcePath tempDir
    convertFile sourcePath tempGoPath
    missing <- liftIO $ mirrorEmbeddedResources sourcePath tempDir tempGoPath
    handleMissingEmbeds strict missing
    return tempGoPath

mirrorEmbeddedResources :: FilePath -> FilePath -> FilePath -> IO [MissingEmbed]
mirrorEmbeddedResources sourcePath tempDir tempGoPath = do
    content <- readFile tempGoPath
    let srcDir = takeDirectory sourcePath
        reference = sourcePath
    copyEmbeddedFiles srcDir tempDir reference content

prepareDirectoryProject :: Bool -> FilePath -> FilePath -> IOResult ()
prepareDirectoryProject strict sourceRoot tempDir = do
    batchConvert sourceRoot tempDir
    missing <- liftIO $ copyEmbeddedForBuild sourceRoot tempDir
    handleMissingEmbeds strict missing

main :: IO ()
main = do
    cliArgs <- parseArgs  -- 重命名为 cliArgs
    result <- runExceptT (dispatch cliArgs)
    case result of
        Left err -> putStrLn ("Error: " ++ err) >> exitFailure
        Right _  -> return ()

dispatch :: Args -> IOResult ()
dispatch (Convert inputPath outputPath) = do
    isDir <- liftIO $ doesDirectoryExist inputPath
    if isDir
    then batchConvert inputPath outputPath
    else convertFile inputPath outputPath

dispatch (Check inputPath) = do
    isDir <- liftIO $ doesDirectoryExist inputPath
    if isDir
    then batchCheck inputPath
    else
        withTemporaryGoProject "typus_check" $ \tempDir -> do
            tempGoPath <- prepareSingleFileProject False inputPath tempDir
            runGoCommandInDir ["build", tempGoPath] tempDir
            liftIO $ putStrLn $ "Typus syntax and compilation OK: " ++ inputPath

dispatch (Build strict buildArgs) = do
    -- Support optional first arg as project path; remaining are passed to `go build`
    let (targetPath, goArgs) = case buildArgs of
            (p:rest) -> (p, rest)
            []       -> (".", [])
    isDir <- liftIO $ doesDirectoryExist targetPath
    isFile <- liftIO $ doesFileExist targetPath
    if isDir
      then withTemporaryGoProject "typus_build" $ \tempDir -> do
             prepareDirectoryProject strict targetPath tempDir
             runGoCommandInDir ("build" : goArgs) tempDir
      else if isFile
        then withTemporaryGoProject "typus_build_single" $ \tempDir -> do
             _ <- prepareSingleFileProject strict targetPath tempDir
             runGoCommandInDir ("build" : goArgs) tempDir
        else throwError $ "Path does not exist: " ++ targetPath

dispatch (Run strict runArgs) = do
    case runArgs of
        [] -> throwError "Please specify a .typus file to run"
        (inputFile:restArgs) -> do
            exists <- liftIO $ doesFileExist inputFile
            unless exists $ throwError $ "Input file does not exist: " ++ inputFile
            withTemporaryGoProject "typus_run" $ \tempDir -> do
                tempGoPath <- prepareSingleFileProject strict inputFile tempDir
                let goArgs = "run" : takeFileName tempGoPath : restArgs
                runGoCommandInDir goArgs tempDir

dispatch Version = do
    liftIO $ putStrLn "typus version 0.1.0"
