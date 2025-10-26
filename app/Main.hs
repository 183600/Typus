module Main (main) where

import Cli
import CompilerUtils
import Control.Monad (unless, forM_, forM)
import Control.Monad.Except
import Control.Monad.IO.Class (liftIO)
import System.Directory (doesFileExist, doesDirectoryExist, copyFile, createDirectoryIfMissing, listDirectory)
import System.FilePath
import System.IO.Temp (withSystemTempDirectory)
import System.Exit (exitFailure)
import Data.List (isInfixOf, isPrefixOf)
import Data.Char (isSpace)
import System.FilePath.Glob (glob)

-- Define the IOResult type alias
type IOResult = ExceptT String IO

-- Helper function to replace substrings
replace :: String -> String -> String -> String
replace old new = go
  where
    go [] = []
    go str
      | old `isPrefixOf` str = new ++ go (drop (length old) str)
      | (x:xs) <- str = x : go xs

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
copyEmbeddedFiles :: FilePath -> FilePath -> String -> IO ()
copyEmbeddedFiles sourceDir destDir sourceContent = do
    let patterns = extractEmbeddedPatterns sourceContent
    forM_ patterns $ \pat -> do
        let absPattern = sourceDir </> pat
        matches <- glob absPattern
        case matches of
          (m:ms) -> do
            forM_ (m:ms) $ \src -> do
                let rel = makeRelative sourceDir src
                let dest = destDir </> rel
                createDirectoryIfMissing True (takeDirectory dest)
                copyFile src dest
                putStrLn $ "Copied embedded file: " ++ src ++ " -> " ++ dest
          [] -> do
            let asDir = sourceDir </> pat
            isDir <- doesDirectoryExist asDir
            if isDir
              then do
                files <- listFilesRecursively asDir
                forM_ files $ \src -> do
                    let rel = makeRelative sourceDir src
                    let dest = destDir </> rel
                    createDirectoryIfMissing True (takeDirectory dest)
                    copyFile src dest
                    putStrLn $ "Copied embedded dir file: " ++ src ++ " -> " ++ dest
              else
                putStrLn $ "Warning: No embedded files matched pattern: " ++ pat ++ " under " ++ sourceDir

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
copyEmbeddedForBuild :: FilePath -> FilePath -> IO ()
copyEmbeddedForBuild inputRoot tempRoot = do
    goFiles <- listGoFiles tempRoot
    forM_ goFiles $ \goOut -> do
        content <- readFile goOut
        let relDir = makeRelative tempRoot (takeDirectory goOut)
            srcDir = inputRoot </> relDir
            destDir = tempRoot  </> relDir
        copyEmbeddedFiles srcDir destDir content

listGoFiles :: FilePath -> IO [FilePath]
listGoFiles dir = do
    names <- listDirectory dir
    paths <- forM names $ \n -> do
        let p = dir </> n
        isDir <- doesDirectoryExist p
        if isDir then listGoFiles p else return [p]
    return [ p | p <- concat paths, takeExtension p == ".go" ]

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
    else do
        -- Convert file to temp directory and check
        liftIO $ withSystemTempDirectory "typus_check" $ \tempDir -> do
            result <- runExceptT $ do
                -- Generate a safe filename that doesn't contain "test"
                let baseName = takeBaseName inputPath
                    safeName = if "test" `isInfixOf` baseName 
                               then replace "test" "exec" baseName 
                               else baseName
                    tempGoPath = tempDir </> (safeName ++ ".go")
                
                convertFile inputPath tempGoPath
                -- Create go.mod for proper Go module support
                liftIO $ writeFile (tempDir </> "go.mod") "module temp\ngo 1.21\n"
                -- Try to build the generated Go code
                runGoCommandInDir ["build", tempGoPath] tempDir
                liftIO $ putStrLn $ "Typus syntax and compilation OK: " ++ inputPath
            case result of
                Left err -> do
                    putStrLn ("Error: Compilation error: " ++ err)
                    exitFailure
                Right _ -> return ()

dispatch (Build buildArgs) = do
    -- Support optional first arg as project path; remaining are passed to `go build`
    let (targetPath, goArgs) = case buildArgs of
            (p:rest) -> (p, rest)
            []       -> (".", [])
    isDir <- liftIO $ doesDirectoryExist targetPath
    isFile <- liftIO $ doesFileExist targetPath
    if isDir
      then liftIO $ withSystemTempDirectory "typus_build" $ \tempDir -> do
            result <- runExceptT $ do
                -- Convert entire directory preserving structure
                batchConvert targetPath tempDir
                -- Mirror embedded resources referenced by generated Go files
                liftIO $ copyEmbeddedForBuild targetPath tempDir
                -- Create minimal go.mod
                liftIO $ writeFile (tempDir </> "go.mod") "module temp\n\ngo 1.21\n"
                -- Run go build inside temp dir with any extra args
                runGoCommandInDir ("build" : goArgs) tempDir
            case result of
                Left err -> putStrLn ("Error: " ++ err) >> exitFailure
                Right _  -> return ()
      else if isFile
        then liftIO $ withSystemTempDirectory "typus_build_single" $ \tempDir -> do
            result <- runExceptT $ do
                -- Convert single file to temp dir root
                let baseName = takeBaseName targetPath
                    safeName = if "test" `isInfixOf` baseName then replace "test" "exec" baseName else baseName
                    outGo = tempDir </> (safeName ++ ".go")
                convertFile targetPath outGo
                -- Copy embedded assets relative to source file
                srcContent <- liftIO $ readFile outGo
                let srcDir = takeDirectory targetPath
                liftIO $ copyEmbeddedFiles srcDir tempDir srcContent
                -- Create minimal go.mod
                liftIO $ writeFile (tempDir </> "go.mod") "module temp\n\ngo 1.21\n"
                runGoCommandInDir ("build" : goArgs) tempDir
            case result of
                Left err -> putStrLn ("Error: " ++ err) >> exitFailure
                Right _  -> return ()
        else throwError $ "Path does not exist: " ++ targetPath

dispatch (Run runArgs) = do
    case runArgs of
        [] -> throwError "Please specify a .typus file to run"
        (inputFile:restArgs) -> do
            exists <- liftIO $ doesFileExist inputFile
            unless exists $ throwError $ "Input file does not exist: " ++ inputFile
            
            liftIO $ withSystemTempDirectory "typus_run" $ \tempDir -> do
                result <- runExceptT $ do
                    -- Generate a safe filename that doesn't contain "test"
                    let baseName = takeBaseName inputFile
                        safeName = if "test" `isInfixOf` baseName 
                                   then replace "test" "exec" baseName 
                                   else baseName
                        tempGoPath = tempDir </> (safeName ++ ".go")
                    
                    -- Convert file
                    convertFile inputFile tempGoPath
                    
                    -- Read generated Go to find embedded files and mirror from source tree
                    sourceContent <- liftIO $ readFile tempGoPath
                    let srcDir = takeDirectory inputFile
                    liftIO $ copyEmbeddedFiles srcDir tempDir sourceContent
                    
                    -- Create go.mod
                    liftIO $ writeFile (tempDir </> "go.mod") "module temp\n\ngo 1.21\n"
                    
                    -- Run the generated Go file
                    let goArgs = "run" : [takeFileName tempGoPath] ++ restArgs
                    runGoCommandInDir goArgs tempDir
                
                case result of
                    Left err -> putStrLn ("Error: " ++ err) >> exitFailure
                    Right _ -> return ()

dispatch CheckHaskellEnv = checkHaskellEnvironment

dispatch Version = do
    liftIO $ putStrLn "typus version 0.1.0"

