module Tooling.Error
    ( ToolingError(..)
    , MissingEmbedInfo(..)
    , renderToolingError
    ) where

import Compiler (CompilerError, renderCompilationError)
import Data.List (intercalate, nub)
import qualified SyntaxValidator as SV

-- | Structured error type for compiler tooling utilities.
data ToolingError
    = FileNotFound FilePath
    | PathDoesNotExist FilePath
    | NotADirectory FilePath
    | InvalidArgument String
    | ParserError FilePath String
    | CompilationFailed FilePath [CompilerError]
    | SyntaxValidationFailed FilePath [SV.SyntaxError]
    | GoToolchainUnavailable String
    | GoCommandFailed
        { teCommand :: String
        , teArgs :: [String]
        , teWorkingDir :: FilePath
        , teExitCode :: Int
        , teStdout :: String
        , teStderr :: String
        }
    | MissingEmbeddedAssets [MissingEmbedInfo]
    | BatchCheckFailures [(FilePath, ToolingError)]
    deriving (Eq, Show)

-- | Lightweight description of a missing embedded asset.
data MissingEmbedInfo = MissingEmbedInfo
    { meiPattern :: String
    , meiRoot :: FilePath
    , meiReference :: FilePath
    }
    deriving (Eq, Ord, Show)

renderToolingError :: ToolingError -> String
renderToolingError err = case err of
    FileNotFound path -> "File not found: " ++ path
    PathDoesNotExist path -> "Path does not exist: " ++ path
    NotADirectory path -> "Expected a directory but found: " ++ path
    InvalidArgument msg -> "Invalid argument: " ++ msg
    ParserError file msg ->
        "Parse error in " ++ file ++ ":\n" ++ indent msg
    CompilationFailed file errs ->
        let header = "Compilation failed for " ++ file ++ ":"
            body = renderCompilationError errs
        in header ++ "\n" ++ indent body
    SyntaxValidationFailed file errs ->
        let header = "Syntax validation failed for " ++ file ++ ":"
            details =
                if null errs
                then indent "(no additional details)"
                else indent (unlines (map SV.formatSyntaxError errs))
        in header ++ "\n" ++ details
    GoToolchainUnavailable msg -> msg
    GoCommandFailed{ teCommand = cmd, teArgs = args, teWorkingDir = dir, teExitCode = code, teStdout = out, teStderr = errOut } ->
        let commandLine = unwords (cmd : args)
            base = "Go command failed: " ++ commandLine ++ " (exit code " ++ show code ++ ") in " ++ dir
            stdoutSection = if null out then "" else "\nStdout:\n" ++ indent out
            stderrSection = if null errOut then "" else "\nStderr:\n" ++ indent errOut
        in base ++ stdoutSection ++ stderrSection
    MissingEmbeddedAssets infos ->
        let header = "Missing embedded assets detected:"
            uniqueInfos = nub infos
            details = map renderMissing uniqueInfos
        in unlines (header : details)
    BatchCheckFailures failures ->
        let header = show (length failures) ++ " file(s) failed syntax check."
            details = map renderFailure failures
        in unlines (header : details)
  where
    indent :: String -> String
    indent text = intercalate "\n" (map ("  " ++) (lines text))

    renderMissing :: MissingEmbedInfo -> String
    renderMissing (MissingEmbedInfo pat root reference) =
        "  pattern \"" ++ pat ++ "\" relative to " ++ root ++ " (referenced in " ++ reference ++ ")"

    renderFailure :: (FilePath, ToolingError) -> String
    renderFailure (file, failure) =
        "  - " ++ file ++ "\n" ++ indent (renderToolingError failure)
