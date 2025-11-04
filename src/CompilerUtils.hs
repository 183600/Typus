module CompilerUtils (
    Logger(..), CompilerContext(..),
    defaultLogger, silentLogger, newCompilerContext, newCompilerContextWithExecutor,
    convertFile, batchConvert, batchCheck, runGoCommand, runGoCommandInDir
) where

import Compiler (compile)
import qualified Parser as P
import Control.Monad (forM, forM_, unless, when)
import Control.Monad.Except
import Control.Monad.IO.Class (liftIO)
import Data.Either (partitionEithers)
import GoToolchain
    ( IOResult
    , GoExecutor(..)
    , defaultGoExecutor
    , isEnvVarEnabled
    , nullDevice
    , writeGoModule
    )
import Tooling.Error (ToolingError(..), renderToolingError)
import System.Directory
    ( doesFileExist
    , doesDirectoryExist
    , listDirectory
    , createDirectoryIfMissing
    )
import System.FilePath
    ( (</>)
    , takeDirectory
    , takeExtension
    , replaceExtension
    , makeRelative
    )
import System.IO.Temp (withSystemTempDirectory)

-- | Logger abstraction for dependency injection
data Logger = Logger
    { logInfo :: String -> IO ()
    , logDebug :: String -> IO ()
    , logWarning :: String -> IO ()
    }

-- | Compiler context holding logger and Go toolchain executor
data CompilerContext = CompilerContext
    { contextLogger :: Logger
    , contextGoExecutor :: GoExecutor
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
    executor <- defaultGoExecutor (logInfo logger)
    newCompilerContextWithExecutor logger executor

-- | Create a compiler context with a custom Go executor (useful in tests).
newCompilerContextWithExecutor :: Logger -> GoExecutor -> IO CompilerContext
newCompilerContextWithExecutor logger executor =
    pure CompilerContext
        { contextLogger = logger
        , contextGoExecutor = executor
        }

-- 单文件转换：Typus -> Go 或 Go -> Go
convertFile :: CompilerContext -> FilePath -> FilePath -> IOResult ()
convertFile ctx input output = do
    let Logger { logInfo = logI, logDebug = logD } = contextLogger ctx

    exists <- liftIO $ doesFileExist input
    unless exists $ throwError $ FileNotFound input

    source <- liftIO $ readFile input
    let isGoFile = takeExtension input == ".go"

    -- 如果是Go文件，直接使用源代码；如果是.typus文件，进行解析
    goCode <- if isGoFile
        then do
            liftIO $ logI $ "Go file detected, using original code: " ++ input
            return source
        else do
            typusFile <- case P.parseTypus source of
                Left err   -> throwError $ ParserError input err
                Right file -> return file

            -- Integrated analysis and compilation
            debug <- liftIO $ isEnvVarEnabled "TYPUS_DEBUG"
            when debug $ liftIO $ logD $ "Parsing completed for: " ++ input
            when debug $ liftIO $ logD "Running integrated analysis..."

            -- Compile to Go code with enhanced analysis
            case compile typusFile of
                Left errs  -> throwError $ CompilationFailed input errs
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
    unless isDir $ throwError $ NotADirectory inputDir

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
    unless isDir $ throwError $ NotADirectory inputDir

    files <- liftIO $ findTypusFiles inputDir

    -- 函数式地收集每个文件的检查结果
    results <- forM files $ \file -> do
        result <- liftIO $ runExceptT $ checkSingleFile ctx file
        case result of
            Right _ -> do
                liftIO $ logI $ "✓ All checks passed: " ++ file
                return $ Right ()
            Left err -> do
                liftIO $ logW $ "✗ Check failed: " ++ file ++ "\n" ++ renderToolingError err
                return $ Left (file, err)

    let (failures, _successes) = partitionEithers results
    if null failures
        then liftIO $ logI $ "\nCheck Summary: " ++ show (length files) ++ " files OK."
        else throwError $ BatchCheckFailures failures

-- 检查单个文件：Typus 语法、编译为 Go、go build 语法验证
checkSingleFile :: CompilerContext -> FilePath -> IOResult ()
checkSingleFile ctx file = do
    let Logger { logInfo = logI } = contextLogger ctx

    liftIO $ logI $ "\nChecking file: " ++ file

    -- 1. Typus 语法检查
    liftIO $ logI "  1. Checking Typus syntax..."
    source <- liftIO $ readFile file
    parsed <- case P.parseTypus source of
        Left err -> throwError (ParserError file err)
        Right p  -> return p
    liftIO $ logI "     ✓ Typus syntax OK"

    -- 2. 编译为 Go
    liftIO $ logI "  2. Compiling to Go..."
    goCode <- case compile parsed of
        Left errs -> throwError (CompilationFailed file errs)
        Right c   -> return c
    liftIO $ logI "     ✓ Compilation successful"

    -- 3. 调用 Go 编译器做语法检查（在临时目录构建）
    let goExec = contextGoExecutor ctx
    skipGo <- liftIO $ goShouldSkip goExec
    if skipGo
        then liftIO $ logI "  3. Skipping Go syntax check (TYPUS_SKIP_GO_BUILD is enabled)."
        else do
            liftIO $ logI "  3. Checking Go syntax..."
            goCheckResult <- liftIO $ withSystemTempDirectory "typus_check" $ \tempDir ->
                runExceptT $ do
                    let tempGoPath = tempDir </> "main.go"
                    liftIO $ writeFile tempGoPath goCode
                    writeGoModule tempDir
                    let goArgs = ["build", "-o", nullDevice, "main.go"]
                    runGoCommandInDir ctx goArgs tempDir

            case goCheckResult of
                Left err -> throwError err
                Right _  -> liftIO $ logI "     ✓ Go syntax OK"

-- 运行 go 命令（当前目录）
runGoCommand :: CompilerContext -> [String] -> IOResult ()
runGoCommand ctx args = runGoCommandInDir ctx args "."

-- 运行 go 命令（指定目录）
runGoCommandInDir :: CompilerContext -> [String] -> FilePath -> IOResult ()
runGoCommandInDir ctx args dir =
    goRunCommandInDir (contextGoExecutor ctx) args dir
