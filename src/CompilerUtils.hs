module CompilerUtils (
    convertFile, batchConvert, batchCheck, runGoCommand, runGoCommandInDir
) where

import Compiler (compile)
import qualified Parser as P
import Control.Monad (forM, forM_, unless)
import Control.Monad.Except
import Control.Monad.IO.Class (liftIO)
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

type IOResult a = ExceptT String IO a

-- 单文件转换：Typus -> Go 或 Go -> Go
convertFile :: FilePath -> FilePath -> IOResult ()
convertFile input output = do
    exists <- liftIO $ doesFileExist input
    unless exists $ throwError $ "Input file does not exist: " ++ input

    source <- liftIO $ readFile input
    let isGoFile = takeExtension input == ".go"

    -- 如果是Go文件，直接使用源代码；如果是.typus文件，进行解析
    goCode <- if isGoFile
        then do
            liftIO $ putStrLn $ "Go file detected, using original code: " ++ input
            return source
        else do
            typusFile <- case P.parseTypus source of
                Left err   -> throwError $ "Parse error in " ++ input ++ ": " ++ err
                Right file -> return file

            -- Integrated analysis and compilation
            liftIO $ putStrLn $ "Parsing completed for: " ++ input
            liftIO $ putStrLn $ "Running integrated analysis..."

            -- Compile to Go code with enhanced analysis
            case compile typusFile of
                Left err   -> throwError $ "Compilation error: " ++ err
                Right code -> do
                    liftIO $ putStrLn $ "Compilation successful"
                    return code

    -- 打印完整生成代码，便于调试
    let codeLength = length goCode
    liftIO $ putStrLn $ "Generated Go code (" ++ show codeLength ++ " characters):"
    liftIO $ putStrLn $ "----------------------------------------"
    liftIO $ putStrLn goCode
    liftIO $ putStrLn $ "----------------------------------------"

    -- 确保输出目录存在并写入
    let parentDir = takeDirectory output
    liftIO $ createDirectoryIfMissing True parentDir
    liftIO $ writeFile output goCode
    liftIO $ putStrLn $ "Converted: " ++ input ++ " -> " ++ output

-- 批量转换：保持目录结构，并将 .typus 扩展名替换为 .go
batchConvert :: FilePath -> FilePath -> IOResult ()
batchConvert inputDir outputDir = do
    isDir <- liftIO $ doesDirectoryExist inputDir
    unless isDir $ throwError $ "Input is not a directory: " ++ inputDir

    liftIO $ createDirectoryIfMissing True outputDir
    files <- liftIO $ findTypusFiles inputDir

    forM_ files $ \inputFile -> do
        let relPath    = makeRelative inputDir inputFile
            outputPath = outputDir </> replaceExtension relPath "go"
        convertFile inputFile outputPath

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
batchCheck :: FilePath -> IOResult ()
batchCheck inputDir = do
    isDir <- liftIO $ doesDirectoryExist inputDir
    unless isDir $ throwError $ "Input is not a directory: " ++ inputDir

    files <- liftIO $ findTypusFiles inputDir

    -- 函数式地收集每个文件的检查结果
    results <- forM files $ \file -> do
        result <- liftIO $ runExceptT $ checkSingleFile file
        case result of
            Right _ -> do
                liftIO $ putStrLn $ "✓ All checks passed: " ++ file
                return $ Right ()
            Left err -> do
                liftIO $ putStrLn $ "✗ Check failed: " ++ file ++ " - " ++ err
                return $ Left (file, err)

    let (failures, _successes) = partitionEithers results
    if null failures
        then liftIO $ putStrLn $ "\nCheck Summary: " ++ show (length files) ++ " files OK."
        else throwError $ show (length failures) ++ " file(s) failed syntax check."

-- 检查单个文件：Typus 语法、编译为 Go、go build 语法验证
checkSingleFile :: FilePath -> IOResult ()
checkSingleFile file = do
    liftIO $ putStrLn $ "\nChecking file: " ++ file

    -- 1. Typus 语法检查
    liftIO $ putStrLn "  1. Checking Typus syntax..."
    source <- liftIO $ readFile file
    parsed <- case P.parseTypus source of
        Left err -> throwError err
        Right p  -> return p
    liftIO $ putStrLn "     ✓ Typus syntax OK"

    -- 2. 编译为 Go
    liftIO $ putStrLn "  2. Compiling to Go..."
    goCode <- case compile parsed of
        Left err -> throwError err
        Right c  -> return c
    liftIO $ putStrLn "     ✓ Compilation successful"

    -- 3. 调用 Go 编译器做语法检查（在临时目录构建）
    liftIO $ putStrLn "  3. Checking Go syntax..."
    goCheckResult <- liftIO $ withSystemTempDirectory "typus_check" $ \tempDir -> do
        let tempGoPath = tempDir </> "main.go"
        writeFile tempGoPath goCode
        writeFile (tempDir </> "go.mod") "module temp\n\ngo 1.21\n"

        -- 平台相关的空设备
        let nullOutput = if os == "mingw32" then "NUL" else "/dev/null"
        let goArgs = ["build", "-o", nullOutput, "main.go"]

        -- 在 IO 中运行 ExceptT，返回 IO (Either String ())
        runExceptT $ runGoCommandInDir goArgs tempDir

    case goCheckResult of
        Left err -> throwError err
        Right _  -> liftIO $ putStrLn "     ✓ Go syntax OK"

-- 运行 go 命令（当前目录）
runGoCommand :: [String] -> IOResult ()
runGoCommand args = runGoCommandInDir args "."

-- 运行 go 命令（指定目录）
runGoCommandInDir :: [String] -> FilePath -> IOResult ()
runGoCommandInDir args dir = do
    -- 确认 go 可执行文件存在
    goCheck <- liftIO $ findExecutable "go"
    case goCheck of
        Nothing -> throwError "Go is not installed or not in PATH. Please install Go."
        Just _  -> return ()

    let processSpec = (proc "go" args) { cwd = Just dir }
    (exitCode, stdout, stderr) <- liftIO $ readCreateProcessWithExitCode processSpec ""

    case exitCode of
        ExitSuccess ->
            if not (null stdout)
                then liftIO $ putStr stdout
                else return ()
        ExitFailure code -> do
            let cmd = "go " ++ unwords args
            throwError $ cmd ++ " failed with exit code " ++ show code ++ ".\nStdout: " ++ stdout ++ "\nStderr: " ++ stderr