module CompilerUtils (
    Logger(..), CompilerContext(..), 
    defaultLogger, silentLogger, newCompilerContext,
    convertFile, batchConvert, batchCheck, runGoCommand, runGoCommandInDir
) where

import Compiler (compile, renderCompilationError)
import qualified Parser as P
import Control.Monad (forM, forM_, unless, when)
import Control.Monad.Except
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent.MVar (MVar, newMVar, modifyMVar)
import System.Directory
    ( doesFileExist
    , doesDirectoryExist
    , listDirectory
    , createDirectoryIfMissing
    , findExecutable
    )
import System.FilePath
    ( (</>)
    , takeDirectory
    , takeExtension
    , replaceExtension
    , makeRelative
    )
import System.Process (readCreateProcessWithExitCode, proc, CreateProcess(cwd))
import System.IO.Temp (withSystemTempDirectory)
import System.Exit (ExitCode(..))
import System.Info (os)
import Data.Either (partitionEithers)
import System.Environment (lookupEnv)
import Data.Char (toLower)

type IOResult a = ExceptT String IO a

-- | Logger abstraction for dependency injection
data Logger = Logger
    { logInfo :: String -> IO ()
    , logDebug :: String -> IO ()
    , logWarning :: String -> IO ()
    }

-- | Compiler context holding logger and thread-safe cache
data CompilerContext = CompilerContext
    { contextLogger :: Logger
    , goAvailabilityCache :: MVar (Maybe (Either String ()))
    }

-- | Default logger that writes to stdout
defaultLogger :: Logger
defaultLogger = Logger
    { logInfo = putStrLn
    , logDebug = putStrLn
    , logWarning = putStrLn
    }

-- | Silent logger for testing
silentLogger :: Logger
silentLogger = Logger
    { logInfo = const (return ())
    , logDebug = const (return ())
    , logWarning = const (return ())
    }

-- | Create a new compiler context with the given logger
newCompilerContext :: Logger -> IO CompilerContext
newCompilerContext logger = do
    cache <- newMVar Nothing
    return CompilerContext
        { contextLogger = logger
        , goAvailabilityCache = cache
        }

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

-- | Check if Go is available, using thread-safe caching via MVar
ensureGoAvailable :: CompilerContext -> IOResult ()
ensureGoAvailable ctx = do
    result <- liftIO $ modifyMVar (goAvailabilityCache ctx) $ \cached ->
        case cached of
            Just r -> return (cached, r)
            Nothing -> do
                found <- findExecutable "go"
                let r = maybe
                        (Left "Go is not installed or not in PATH. Please install Go.")
                        (const (Right ()))
                        found
                return (Just r, r)
    case result of
        Left err -> throwError err
        Right () -> return ()

-- 单文件转换：Typus -> Go 或 Go -> Go
convertFile :: CompilerContext -> FilePath -> FilePath -> IOResult ()
convertFile ctx input output = do
    let Logger { logInfo = logI, logDebug = logD } = contextLogger ctx

    exists <- liftIO $ doesFileExist input
    unless exists $ throwError $ "Input file does not exist: " ++ input

    source <- liftIO $ readFile input
    let isGoFile = takeExtension input == ".go"

    -- 如果是Go文件，直接使用源代码；如果是.typus文件，进行解析
    goCode <- if isGoFile
        then do
            liftIO $ logI $ "Go file detected, using original code: " ++ input
            return source
        else do
            typusFile <- case P.parseTypus source of
                Left err   -> throwError $ "Parse error in " ++ input ++ ": " ++ err
                Right file -> return file

            -- Integrated analysis and compilation
            debug <- liftIO $ isEnvVarEnabled "TYPUS_DEBUG"
            when debug $ liftIO $ logD $ "Parsing completed for: " ++ input
            when debug $ liftIO $ logD "Running integrated analysis..."

            -- Compile to Go code with enhanced analysis
            case compile typusFile of
                Left err   -> throwError $ "Compilation error: " ++ renderCompilationError err
                Right code -> do
                    liftIO $ logI "Compilation successful"
                    -- Only print full generated code in debug mode to avoid excessive I/O
                    when debug $ do
                        let codeLength = length code
                        liftIO $ logD $ "Generated Go code (" ++ show codeLength ++ " characters):"
                        liftIO $ logD "----------------------------------------"
                        liftIO $ logD code
                        liftIO $ logD "----------------------------------------"
                    return code

    -- 调试模式下可通过设置环境变量 TYPUS_DEBUG=1 查看完整生成代码

    -- 确保输出目录存在并写入
    let parentDir = takeDirectory output
    liftIO $ createDirectoryIfMissing True parentDir
    liftIO $ writeFile output goCode
    liftIO $ logI $ "Converted: " ++ input ++ " -> " ++ output

-- 批量转换：保持目录结构，并将 .typus 扩展名替换为 .go
batchConvert :: CompilerContext -> FilePath -> FilePath -> IOResult ()
batchConvert ctx inputDir outputDir = do
    isDir <- liftIO $ doesDirectoryExist inputDir
    unless isDir $ throwError $ "Input is not a directory: " ++ inputDir

    liftIO $ createDirectoryIfMissing True outputDir
    files <- liftIO $ findTypusFiles inputDir

    forM_ files $ \inputFile -> do
        let relPath    = makeRelative inputDir inputFile
            outputPath = outputDir </> replaceExtension relPath "go"
        convertFile ctx inputFile outputPath

-- 递归查找 .typus 文件
findTypusFiles :: FilePath -> IO [FilePath]
findTypusFiles dir = do
    names <- listDirectory dir
    paths <- forM names $ \name -> do
        let path = dir </> name
        isDir' <- doesDirectoryExist path
        if isDir'
            then findTypusFiles path
            else return [path]
    return $ filter (\p -> takeExtension p == ".typus") (concat paths)

-- 批量检查：解析、编译并用 go build 验证语法（使用临时目录）
batchCheck :: CompilerContext -> FilePath -> IOResult ()
batchCheck ctx inputDir = do
    let Logger { logInfo = logI, logWarning = logW } = contextLogger ctx

    isDir <- liftIO $ doesDirectoryExist inputDir
    unless isDir $ throwError $ "Input is not a directory: " ++ inputDir

    files <- liftIO $ findTypusFiles inputDir

    -- 函数式地收集每个文件的检查结果
    results <- forM files $ \file -> do
        result <- liftIO $ runExceptT $ checkSingleFile ctx file
        case result of
            Right _ -> do
                liftIO $ logI $ "✓ All checks passed: " ++ file
                return $ Right ()
            Left err -> do
                liftIO $ logW $ "✗ Check failed: " ++ file ++ " - " ++ err
                return $ Left (file, err)

    let (failures, _successes) = partitionEithers results
    if null failures
        then liftIO $ logI $ "\nCheck Summary: " ++ show (length files) ++ " files OK."
        else throwError $ show (length failures) ++ " file(s) failed syntax check."

-- 检查单个文件：Typus 语法、编译为 Go、go build 语法验证
checkSingleFile :: CompilerContext -> FilePath -> IOResult ()
checkSingleFile ctx file = do
    let Logger { logInfo = logI } = contextLogger ctx

    liftIO $ logI $ "\nChecking file: " ++ file

    -- 1. Typus 语法检查
    liftIO $ logI "  1. Checking Typus syntax..."
    source <- liftIO $ readFile file
    parsed <- case P.parseTypus source of
        Left err -> throwError err
        Right p  -> return p
    liftIO $ logI "     ✓ Typus syntax OK"

    -- 2. 编译为 Go
    liftIO $ logI "  2. Compiling to Go..."
    goCode <- case compile parsed of
        Left err -> throwError (renderCompilationError err)
        Right c  -> return c
    liftIO $ logI "     ✓ Compilation successful"

    -- 3. 调用 Go 编译器做语法检查（在临时目录构建）
    skipGo <- liftIO shouldSkipGoToolchain
    if skipGo
        then liftIO $ logI "  3. Skipping Go syntax check (TYPUS_SKIP_GO_BUILD is enabled)."
        else do
            liftIO $ logI "  3. Checking Go syntax..."
            goCheckResult <- liftIO $ withSystemTempDirectory "typus_check" $ \tempDir -> do
                let tempGoPath = tempDir </> "main.go"
                writeFile tempGoPath goCode
                writeFile (tempDir </> "go.mod") "module temp\n\ngo 1.21\n"

                -- 平台相关的空设备
                let nullOutput = if os == "mingw32" then "NUL" else "/dev/null"
                let goArgs = ["build", "-o", nullOutput, "main.go"]

                -- 在 IO 中运行 ExceptT，返回 IO (Either String ())
                runExceptT $ runGoCommandInDir ctx goArgs tempDir

            case goCheckResult of
                Left err -> throwError err
                Right _  -> liftIO $ logI "     ✓ Go syntax OK"

-- 运行 go 命令（当前目录）
runGoCommand :: CompilerContext -> [String] -> IOResult ()
runGoCommand ctx args = runGoCommandInDir ctx args "."

-- 运行 go 命令（指定目录）
runGoCommandInDir :: CompilerContext -> [String] -> FilePath -> IOResult ()
runGoCommandInDir ctx args dir = do
    let Logger { logInfo = logI } = contextLogger ctx

    skipGo <- liftIO shouldSkipGoToolchain
    if skipGo
        then liftIO $ logI $ "Skipping Go command: go " ++ unwords args ++ " (TYPUS_SKIP_GO_BUILD is enabled)."
        else do
            ensureGoAvailable ctx

            let processSpec = (proc "go" args) { cwd = Just dir }
            (exitCode, stdout, stderr) <- liftIO $ readCreateProcessWithExitCode processSpec ""

            case exitCode of
                ExitSuccess ->
                    if not (null stdout)
                        then liftIO $ logI stdout
                        else return ()
                ExitFailure code -> do
                    let cmd = "go " ++ unwords args
                    throwError $ cmd ++ " failed with exit code " ++ show code ++ ".\nStdout: " ++ stdout ++ "\nStderr: " ++ stderr
