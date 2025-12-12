module CompilerUtils (
    Logger(..), CompilerContext(..),
    defaultLogger, silentLogger, newCompilerContext, newCompilerContextWithExecutor,
    convertFile, batchConvert, batchCheck, runGoCommand, runGoCommandInDir,
    compileWithPackage
) where

import Compiler (checkDependentTypes, compile, typeCheckFailure, typeDiagnosticToCompilerError, ensureSourceIR, CompilerResult)
import qualified Compiler.TypeChecker as TypeChecker (diagnoseTypeErrorsWithPackage)
import qualified Utils as U (trim)
import qualified Compiler.IR as IR (buildSemanticIRWithPackage, emitGo, sourceTypusFile, semanticValueInfo, goSource)
import Compiler.OwnershipChecker (checkOwnershipWithValueInfo)
import qualified Parser as P
import Control.Monad (forM, forM_, unless, when, filterM)
import Control.Monad.Except (runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Either (partitionEithers)
import Data.List (isPrefixOf, partition, sort)
import qualified Data.Map as Map
import GoToolchain
    ( IOResult
    , GoExecutor(..)
    , defaultGoExecutor
    , isEnvVarEnabled
    , nullDevice
    , writeGoModule
    )
import qualified SyntaxValidator as SV
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

-- Convert a single file from Typus to Go, handling both .typus and .go source files.
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
            when debug $ liftIO $ logD $ "Running integrated analysis..."

            -- Compile to Go code
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
    let Logger { logInfo = logI, logDebug = logD } = contextLogger ctx
    
    isDir <- liftIO $ doesDirectoryExist inputDir
    unless isDir $ throwError $ NotADirectory inputDir

    liftIO $ createDirectoryIfMissing True outputDir
    
    -- 收集所有文件并按包分组
    files <- liftIO $ findTypusFiles inputDir
    packageGroups <- groupFilesByPackage files
    
    -- 对每个包进行批量转换
    forM_ packageGroups $ \(packageDir, packageFiles) -> do
        debug <- liftIO $ isEnvVarEnabled "TYPUS_DEBUG"
        when debug $ liftIO $ logD $ "Processing package in directory: " ++ packageDir
        when debug $ liftIO $ logD $ "Files: " ++ show packageFiles
        
        -- 解析所有文件
        parsedFiles <- forM packageFiles $ \f -> do
            source <- liftIO $ readFile f
            case P.parseTypus source of
                Left err -> throwError $ ParserError f err
                Right p -> return (f, p)
        
        -- 对每个文件进行转换（使用包上下文）
        forM_ parsedFiles $ \(inputFile, typusFile) -> do
            let relPath    = makeRelative inputDir inputFile
                outputPath = outputDir </> replaceExtension relPath "go"
            
            -- 使用包上下文编译
            case compileWithPackage typusFile parsedFiles of
                Left errs -> throwError $ CompilationFailed inputFile errs
                Right code -> do
                    liftIO $ logI $ "Converted: " ++ inputFile ++ " -> " ++ outputPath
                    liftIO $ writeFile outputPath code
  where
    -- 将文件按包分组
    groupFilesByPackage :: [FilePath] -> IOResult [(FilePath, [FilePath])]
    groupFilesByPackage files = do
        groups <- forM files $ \file -> do
            content <- liftIO $ readFile file
            let packageName = extractPackageName' content
                dir = takeDirectory file
            return (dir, packageName, file)
        
        -- 按目录分组
        let dirGroups = foldr insertFile Map.empty groups
        return $ Map.toList dirGroups
      where
        insertFile :: Ord k => (k, b, a) -> Map.Map k [a] -> Map.Map k [a]
        insertFile (dir, _, file) groups = Map.insertWith (++) dir [file] groups
        
        extractPackageName' content = 
            let linesList = lines content
                -- 只匹配以"package"开头的非注释行
                packageLines = filter isPackageDeclaration linesList
            in case packageLines of
                (line:_) -> let wordsList = words line
                            in if length wordsList >= 2 then wordsList !! 1 else ""
                [] -> ""
          where
            isPackageDeclaration line = 
                let trimmed = U.trim line
                in "package" `isPrefixOf` trimmed && not ("//" `isPrefixOf` trimmed)

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
    let Logger { logInfo = logI, logWarning = logW } = contextLogger ctx

    liftIO $ logI $ "\nChecking file: " ++ file

    -- 1. Typus 语法检查
    liftIO $ logI "  1. Checking Typus syntax..."
    source <- liftIO $ readFile file
    let syntaxIssues = SV.validateFile source
        (syntaxWarnings, syntaxErrors) = partition ((== SV.SyntaxWarning) . SV.errorType) syntaxIssues
    liftIO $
        forM_ syntaxWarnings $ \warning ->
            logW $ "     ⚠ " ++ SV.formatSyntaxError warning
    unless (null syntaxErrors) $
        throwError (SyntaxValidationFailed file syntaxErrors)
    parsed <- case P.parseTypus source of
        Left err -> throwError (ParserError file err)
        Right p  -> return p
    if null syntaxWarnings
        then liftIO $ logI "     ✓ Typus syntax OK"
        else liftIO $ logI "     ✓ Typus syntax OK (with warnings)"

    -- 2. 检查是否应该跳过Go构建
    let goExec = contextGoExecutor ctx
    skipGo <- liftIO $ goShouldSkip goExec
    
    if skipGo
        then do
            liftIO $ logI "  2. Skipping all checks (TYPUS_SKIP_GO_BUILD is enabled)."
            return ()
        else do
            -- 2. 收集同一包中的所有文件进行类型检查
            liftIO $ logI "  2. Collecting package files for type checking..."
            packageFiles <- liftIO $ findTypusFilesInPackage file
            packageParsed <- forM packageFiles $ \f -> do
                src <- liftIO $ readFile f
                case P.parseTypus src of
                    Left err -> throwError (ParserError f err)
                    Right p -> return (f, p)
            liftIO $ logI $ "     ✓ Found " ++ show (length packageFiles) ++ " files in package"

            -- 3. 编译为 Go（使用包中的所有文件）
            liftIO $ logI "  3. Compiling to Go..."
            goCode <- case compileWithPackage parsed packageParsed of
                Left errs -> throwError (CompilationFailed file errs)
                Right c   -> return c
            liftIO $ logI "     ✓ Compilation successful"

            -- 4. 调用 Go 编译器做语法检查（在临时目录构建）
            liftIO $ logI "  4. Checking Go syntax..."
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

-- 查找同一包中的所有Typus文件
findTypusFilesInPackage :: FilePath -> IO [FilePath]
findTypusFilesInPackage file = do
    let dir = takeDirectory file
    isDir <- doesDirectoryExist dir
    if isDir
        then do
            -- 只查找同一目录中的文件，不递归查找子目录
            allEntries <- listDirectory dir
            let allFiles = map (dir </>) allEntries
            typusFiles <- filterM (\f -> do
                isFile <- doesFileExist f
                return $ isFile && takeExtension f == ".typus"
                ) allFiles
            
            -- 如果目录中只有一个typus文件，则只返回该文件
            if length typusFiles == 1
                then return typusFiles
                else do
                    -- 读取主文件的包名
                    mainContent <- readFile file
                    let mainPackageName = extractPackageName mainContent
                    
                    -- 过滤出相同包名的文件
                    packageFiles <- filterM (\f -> do
                        content <- readFile f
                        return $ extractPackageName content == mainPackageName
                        ) typusFiles
                    return $ sort packageFiles
        else return [file]
  where
    extractPackageName content = 
        let linesList = lines content
            -- 只匹配以"package"开头的非注释行
            packageLines = filter isPackageDeclaration linesList
        in case packageLines of
            (line:_) -> let wordsList = words line
                        in if length wordsList >= 2 then wordsList !! 1 else ""
            [] -> ""
      where
        isPackageDeclaration line = 
            let trimmed = U.trim line
            in "package" `isPrefixOf` trimmed && not ("//" `isPrefixOf` trimmed)

-- 编译单个文件时考虑包中的其他文件
compileWithPackage :: P.TypusFile -> [(FilePath, P.TypusFile)] -> CompilerResult String
compileWithPackage mainFile packageFiles = do
    sourceIR <- ensureSourceIR mainFile
    semanticIR <- IR.buildSemanticIRWithPackage sourceIR packageFiles
    let parsedFile = IR.sourceTypusFile sourceIR
    checkDependentTypes parsedFile
    ensureNoTypeErrorsWithPackage parsedFile packageFiles
    checkOwnershipWithValueInfo parsedFile (IR.semanticValueInfo semanticIR) -- Ownership check
    let goArtifact = IR.emitGo semanticIR
    pure (IR.goSource goArtifact)
  where
    ensureNoTypeErrorsWithPackage file pkgFiles =
        case TypeChecker.diagnoseTypeErrorsWithPackage file pkgFiles of
            Left errs -> Left errs
            Right [] -> Right ()
            Right diagnostics ->
                let detailed = map typeDiagnosticToCompilerError diagnostics
                in Left (typeCheckFailure : detailed)

-- 运行 go 命令（当前目录）
runGoCommand :: CompilerContext -> [String] -> IOResult ()
runGoCommand ctx args = runGoCommandInDir ctx args "."

-- 运行 go 命令（指定目录）
runGoCommandInDir :: CompilerContext -> [String] -> FilePath -> IOResult ()
runGoCommandInDir ctx args dir =
    goRunCommandInDir (contextGoExecutor ctx) args dir
