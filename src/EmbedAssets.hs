module EmbedAssets
    ( MissingEmbed(..)
    , formatMissingMessage
    , extractEmbeddedPatterns
    , handleMissingEmbeds
    , mirrorEmbeddedResources
    , copyEmbeddedForBuild
    ) where

import CompilerUtils (Logger(..))
import Control.Monad (forM, forM_, unless)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Char (isSpace)
import Data.List (isPrefixOf, nub)
import GoToolchain (IOResult)
import Tooling.Error (MissingEmbedInfo(..), ToolingError(..))
import System.Directory
    ( copyFile
    , createDirectoryIfMissing
    , doesDirectoryExist
    , listDirectory
    )
import System.FilePath
    ( (</>)
    , makeRelative
    , takeDirectory
    , takeExtension
    )
import System.FilePath.Glob (glob)

-- | Captures information about missing embedded resources.
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

toMissingEmbedInfo :: MissingEmbed -> MissingEmbedInfo
toMissingEmbedInfo (MissingEmbed pat root ref) =
    MissingEmbedInfo
        { meiPattern = pat
        , meiRoot = root
        , meiReference = ref
        }

warnMissingEmbeds :: Logger -> [MissingEmbed] -> IO ()
warnMissingEmbeds logger missing =
    unless (null missing) $ do
        let Logger { logWarning = logW } = logger
        logW (formatMissingMessage missing)
        logW "Continuing because strict embed mode is disabled."

handleMissingEmbeds :: Logger -> Bool -> [MissingEmbed] -> IOResult ()
handleMissingEmbeds logger strict missing
    | null missing = pure ()
    | strict = throwError (MissingEmbeddedAssets (map toMissingEmbedInfo missing))
    | otherwise = liftIO $ warnMissingEmbeds logger missing

extractEmbeddedPatterns :: String -> [String]
extractEmbeddedPatterns content =
    [ normalize token
    | line <- lines content
    , let trimmed = dropWhile isSpace line
    , directive `isPrefixOf` trimmed
    , let rest = dropWhile isSpace (drop (length directive) trimmed)
    , token <- words rest
    , not (null token)
    ]
  where
    directive = "//go:embed"

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

copyEmbeddedFiles :: Logger -> FilePath -> FilePath -> FilePath -> String -> IO [MissingEmbed]
copyEmbeddedFiles logger sourceDir destDir reference sourceContent = do
    let Logger { logInfo = logI, logWarning = logW } = logger
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
                      logI $ "Copied embedded file: " ++ src ++ " -> " ++ dest
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
                            logI $ "Copied embedded dir file: " ++ src ++ " -> " ++ dest
                        pure []
                    else do
                        logW $ "Warning: No embedded files matched pattern: " ++ pat ++ " under " ++ sourceDir
                        pure [MissingEmbed pat sourceDir reference]

listFilesRecursively :: FilePath -> IO [FilePath]
listFilesRecursively dir = do
    names <- listDirectory dir
    paths <- forM names $ \n -> do
        let path = dir </> n
        isDir <- doesDirectoryExist path
        if isDir then listFilesRecursively path else return [path]
    return (concat paths)

mirrorEmbeddedResources :: Logger -> FilePath -> FilePath -> FilePath -> IO [MissingEmbed]
mirrorEmbeddedResources logger sourcePath tempDir tempGoPath = do
    content <- readFile tempGoPath
    let srcDir = takeDirectory sourcePath
        reference = sourcePath
    copyEmbeddedFiles logger srcDir tempDir reference content

copyEmbeddedForBuild :: Logger -> FilePath -> FilePath -> IO [MissingEmbed]
copyEmbeddedForBuild logger inputRoot tempRoot = do
    goFiles <- listGoFiles tempRoot
    fmap concat $
        forM goFiles $ \goOut -> do
            content <- readFile goOut
            let relDir = makeRelative tempRoot (takeDirectory goOut)
                srcDir = inputRoot </> relDir
                destDir = tempRoot  </> relDir
                reference = makeRelative tempRoot goOut
            copyEmbeddedFiles logger srcDir destDir reference content

listGoFiles :: FilePath -> IO [FilePath]
listGoFiles dir = do
    names <- listDirectory dir
    paths <- forM names $ \n -> do
        let path = dir </> n
        isDir <- doesDirectoryExist path
        if isDir then listGoFiles path else return [path]
    return [ path | path <- concat paths, takeExtension path == ".go" ]
