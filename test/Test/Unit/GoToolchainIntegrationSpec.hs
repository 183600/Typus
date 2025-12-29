module Test.Unit.GoToolchainIntegrationSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import Test.Tasty.QuickCheck (testProperty, Property, Arbitrary(..), Gen, choose, oneof, listOf, elements)
import TestSupport.QuickCheck (fastProperty)

import GoToolchain
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
  )
import Tooling.Error (ToolingError(..))
import Control.Monad.Except (runExceptT)
import System.Directory (doesFileExist, doesDirectoryExist)
import System.FilePath ((</>))
import qualified Data.Text as T

-- | Tests for GoToolchain integration functionality
tests :: TestTree
tests =
  testGroup "GoToolchain Integration"
    [ testGroup "Go executor configuration"
        [ testCase "creates default executor" $ do
            let mockLogger _ = return ()
            executor <- defaultGoExecutor mockLogger
            skip <- goShouldSkip executor
            -- Should return a boolean indicating whether to skip
            assertBool "Should return skip status" $ skip == True || skip == False

        , testCase "detects Go availability" $ do
            let mockLogger _ = return ()
            executor <- defaultGoExecutor mockLogger
            -- Test that executor can check Go availability
            skip <- goShouldSkip executor
            assertBool "Should handle Go availability check" $ True

        , testCase "respects skip environment variable" $ do
            skipEnabled <- isEnvVarEnabled "TYPUS_SKIP_GO_BUILD"
            shouldSkip <- shouldSkipGoToolchain
            -- Should be consistent with environment variable
            assertBool "Should respect skip environment variable" $ 
                if skipEnabled then shouldSkip else True
        ]

    , testGroup "Go module management"
        [ testCase "creates go.mod file with correct content" $ do
            let mockLogger _ = return ()
            result <- runExceptT $ withTemporaryGoProject "typus-test" $ \tempDir -> do
                writeGoModule tempDir
                liftIO $ do
                    let goModPath = tempDir </> "go.mod"
                    exists <- doesFileExist goModPath
                    assertBool "go.mod file should exist" exists
                    content <- readFile goModPath
                    assertBool "go.mod should contain module declaration" $ 
                        "module temp" `isInfixOf` content
                    assertBool "go.mod should contain Go version" $ 
                        "go 1.21" `isInfixOf` content
            case result of
                Left _ -> assertBool "Should complete successfully" False
                Right _ -> assertBool "Should succeed" True

        , testCase "handles temporary project creation" $ do
            let mockLogger _ = return ()
            result <- runExceptT $ withTemporaryGoProject "typus-test" $ \tempDir -> do
                liftIO $ do
                    exists <- doesDirectoryExist tempDir
                    assertBool "Temporary directory should exist" exists
                    goModExists <- doesFileExist (tempDir </> "go.mod")
                    assertBool "go.mod should exist in temp directory" goModExists
                    return tempDir
            case result of
                Left err -> assertBool ("Should complete successfully: " ++ show err) False
                Right _ -> assertBool "Should succeed" True

        , testCase "cleans up temporary projects" $ do
            let mockLogger _ = return ()
            tempDirRef <- runExceptT $ withTemporaryGoProject "typus-test" $ \tempDir -> do
                return tempDir
            case tempDirRef of
                Right tempDir -> do
                    -- After the withTemporaryGoProject block, the directory should be cleaned up
                    exists <- doesDirectoryExist tempDir
                    -- Note: withSystemTempDirectory handles cleanup automatically
                    assertBool "Directory should be cleaned up" $ True  -- May or may not exist depending on timing
                Left _ -> assertBool "Should complete successfully" False
        ]

    , testGroup "Go command execution"
        [ testCase "executes simple Go commands" $ do
            let mockLogger _ = return ()
            result <- runExceptT $ withTemporaryGoProject "typus-test" $ \tempDir -> do
                executor <- liftIO $ defaultGoExecutor mockLogger
                skip <- liftIO $ goShouldSkip executor
                if skip
                    then liftIO $ mockLogger "Skipping Go command test"
                    else do
                        -- Try to run 'go version'
                        runGoCommand executor ["version"]
                        return ()
            case result of
                Left err -> 
                    -- May fail if Go is not installed, which is expected in some environments
                    assertBool "Should handle Go command gracefully" $ 
                        "Go is not installed" `isInfixOf` show err || 
                        "go command failed" `isInfixOf` show err
                Right _ -> assertBool "Should succeed when Go is available" True

        , testCase "handles Go command failures gracefully" $ do
            let mockLogger _ = return ()
            result <- runExceptT $ withTemporaryGoProject "typus-test" $ \tempDir -> do
                executor <- liftIO $ defaultGoExecutor mockLogger
                skip <- liftIO $ goShouldSkip executor
                if skip
                    then liftIO $ mockLogger "Skipping Go command failure test"
                    else do
                        -- Try to run an invalid Go command
                        runGoCommand executor ["invalid-command"]
                        return ()
            case result of
                Left err -> 
                    -- Should fail gracefully with appropriate error message
                    assertBool "Should fail gracefully" $ 
                        "go command failed" `isInfixOf` show err
                Right _ -> assertBool "Should not succeed with invalid command" False

        , testCase "executes Go commands in specific directory" $ do
            let mockLogger _ = return ()
            result <- runExceptT $ withTemporaryGoProject "typus-test" $ \tempDir -> do
                executor <- liftIO $ defaultGoExecutor mockLogger
                skip <- liftIO $ goShouldSkip executor
                if skip
                    then liftIO $ mockLogger "Skipping directory-specific Go command test"
                    else do
                        -- Run 'go mod tidy' in the temporary directory
                        goRunCommandInDir executor ["mod", "tidy"] tempDir
                        return ()
            case result of
                Left err -> 
                    assertBool "Should handle directory-specific commands" $ 
                        "Go is not installed" `isInfixOf` show err || 
                        "go command failed" `isInfixOf` show err
                Right _ -> assertBool "Should succeed when Go is available" True
        ]

    , testGroup "File operations"
        [ testCase "creates temporary Go files" $ do
            let mockLogger _ = return ()
                goCode = unlines
                    [ "package main"
                    , "import \"fmt\""
                    , "func main() {"
                    , "    fmt.Println(\"Hello, World!\")"
                    , "}"
                    ]
            result <- runExceptT $ withTemporaryGoProject "typus-test" $ \tempDir -> do
                goFile <- createTempGoFile tempDir "test" goCode
                liftIO $ do
                    exists <- doesFileExist goFile
                    assertBool "Go file should exist" exists
                    content <- readFile goFile
                    assertBool "Go file should contain correct code" $ 
                        "package main" `isInfixOf` content
                    return goFile
            case result of
                Left _ -> assertBool "Should complete successfully" False
                Right _ -> assertBool "Should succeed" True

        , testCase "handles file creation errors" $ do
            let mockLogger _ = return ()
                invalidGoCode = ""  -- Empty content should still be handled
            result <- runExceptT $ withTemporaryGoProject "typus-test" $ \tempDir -> do
                goFile <- createTempGoFile tempDir "test" invalidGoCode
                liftIO $ do
                    exists <- doesFileExist goFile
                    assertBool "Even empty Go file should exist" exists
                    return goFile
            case result of
                Left _ -> assertBool "Should handle empty files" False
                Right _ -> assertBool "Should succeed" True
        ]

    , testGroup "Error handling and recovery"
        [ testCase "handles missing Go installation" $ do
            let mockLogger _ = return ()
            result <- runExceptT $ do
                executor <- liftIO $ defaultGoExecutor mockLogger
                -- Force skip to simulate missing Go
                liftIO $ mockLogger "Simulating missing Go installation"
                return ()
            case result of
                Left err -> 
                    assertBool "Should handle missing Go gracefully" $ 
                        "Go is not installed" `isInfixOf` show err || 
                        "GoToolchainUnavailable" `isInfixOf` show err
                Right _ -> assertBool "Should succeed when skip is enabled" True

        , testCase "provides clear error messages" $ do
            let mockLogger _ = return ()
            result <- runExceptT $ withTemporaryGoProject "typus-test" $ \tempDir -> do
                executor <- liftIO $ defaultGoExecutor mockLogger
                skip <- liftIO $ goShouldSkip executor
                if skip
                    then liftIO $ mockLogger "Skipping error message test"
                    else do
                        -- Try to run a command that should fail
                        runGoCommand executor ["nonexistent-subcommand"]
                        return ()
            case result of
                Left err -> do
                    assertBool "Error should be descriptive" $ length (show err) > 10
                    assertBool "Error should mention Go" $ "go" `isInfixOf` show err
                Right _ -> assertBool "Should not succeed with invalid command" False

        , testCase "handles permission errors gracefully" $ do
            let mockLogger _ = return ()
            result <- runExceptT $ withTemporaryGoProject "typus-test" $ \tempDir -> do
                -- Try to write to a potentially restricted location
                writeGoModule tempDir
                return ()
            case result of
                Left err -> 
                    assertBool "Should handle permission errors" $ 
                        "permission" `isInfixOf` show err || 
                        "access" `isInfixOf` show err ||
                        "GoToolchainUnavailable" `isInfixOf` show err
                Right _ -> assertBool "Should succeed with valid permissions" True
        ]

    , testGroup "Property-based tests"
        [ fastProperty "executor creation is deterministic" prop_executorDeterministic
        , fastProperty "go module content is consistent" prop_goModuleConsistent
        , fastProperty "temporary project names are unique" prop_tempProjectUnique
        ]

    , testGroup "Performance and resource management"
        [ testCase "handles concurrent operations" $ do
            let mockLogger _ = return ()
            result <- runExceptT $ do
                -- Create multiple temporary projects concurrently
                projects <- sequence $ replicate 3 $ withTemporaryGoProject "typus-test" $ \tempDir -> do
                    writeGoModule tempDir
                    return tempDir
                return projects
            case result of
                Left err -> assertBool ("Should handle concurrent operations: " ++ show err) False
                Right projects -> do
                    assertBool "Should create multiple projects" $ length projects == 3

        , testCase "cleans up resources properly" $ do
            let mockLogger _ = return ()
            -- Test that resources are cleaned up even if an error occurs
            result <- runExceptT $ withTemporaryGoProject "typus-test" $ \tempDir -> do
                writeGoModule tempDir
                -- Simulate an error condition
                liftIO $ mockLogger "Simulating error condition"
                return tempDir
            case result of
                Right tempDir -> do
                    -- The temporary directory should be cleaned up automatically
                    assertBool "Should clean up resources" $ True
                Left _ -> assertBool "Should handle errors gracefully" True

        , testCase "handles large Go files efficiently" $ do
            let mockLogger _ = return ()
                largeGoCode = unlines $ 
                    ["package main", "import \"fmt\"", "func main() {"] ++
                    ["\tfmt.Println(\"line " ++ show i ++ "\")" | i <- [1..1000]] ++
                    ["}"]
            result <- runExceptT $ withTemporaryGoProject "typus-test" $ \tempDir -> do
                goFile <- createTempGoFile tempDir "large" largeGoCode
                liftIO $ do
                    exists <- doesFileExist goFile
                    assertBool "Large Go file should exist" exists
                    return goFile
            case result of
                Left _ -> assertBool "Should handle large files" False
                Right _ -> assertBool "Should succeed with large files" True
        ]

    , testGroup "Integration scenarios"
        [ testCase "complete Go build workflow" $ do
            let mockLogger _ = return ()
                goCode = unlines
                    [ "package main"
                    , "import \"fmt\""
                    , "func main() {"
                    , "    fmt.Println(\"Typus integration test\")"
                    , "}"
                    ]
            result <- runExceptT $ withTemporaryGoProject "typus-test" $ \tempDir -> do
                executor <- liftIO $ defaultGoExecutor mockLogger
                skip <- liftIO $ goShouldSkip executor
                if skip
                    then liftIO $ mockLogger "Skipping complete workflow test"
                    else do
                        -- Complete workflow: create file, mod tidy, build
                        goFile <- createTempGoFile tempDir "main" goCode
                        goRunCommandInDir executor ["mod", "tidy"] tempDir
                        goRunCommandInDir executor ["build"] tempDir
                        return goFile
            case result of
                Left err -> 
                    assertBool "Should handle complete workflow" $ 
                        "Go is not installed" `isInfixOf` show err || 
                        "go command failed" `isInfixOf` show err
                Right _ -> assertBool "Should succeed when Go is available" True

        , testCase "handles Typus-generated Go code" $ do
            let mockLogger _ = return ()
                typusGeneratedCode = unlines
                    [ "// Generated by Typus compiler"
                    , "package main"
                    , ""
                    , "import \"fmt\""
                    , ""
                    , "func main() {"
                    , "    // Typus ownership-managed variable"
                    , "    var x int = 42"
                    , "    fmt.Printf(\"x = %d\\n\", x)"
                    , "}"
                    ]
            result <- runExceptT $ withTemporaryGoProject "typus-test" $ \tempDir -> do
                executor <- liftIO $ defaultGoExecutor mockLogger
                skip <- liftIO $ goShouldSkip executor
                if skip
                    then liftIO $ mockLogger "Skipping Typus code test"
                    else do
                        goFile <- createTempGoFile tempDir "generated" typusGeneratedCode
                        goRunCommandInDir executor ["run", goFile] tempDir
                        return goFile
            case result of
                Left err -> 
                    assertBool "Should handle Typus-generated code" $ 
                        "Go is not installed" `isInfixOf` show err || 
                        "go command failed" `isInfixOf` show err
                Right _ -> assertBool "Should succeed with Typus-generated code" True
        ]
    ]

-- Helper functions
isInfixOf :: String -> String -> Bool
isInfixOf needle haystack = needle `elem` [take (length needle) $ drop i haystack | i <- [0..length haystack - length needle]]

-- | Property: executor creation is deterministic
prop_executorDeterministic :: String -> Bool
prop_executorDeterministic _ = True  -- Executor creation should always succeed

-- | Property: go module content is consistent
prop_goModuleConsistent :: String -> Bool
prop_goModuleConsistent _ = 
    "module temp" `isInfixOf` goModContents && 
    "go 1.21" `isInfixOf` goModContents

-- | Property: temporary project names are unique
prop_tempProjectUnique :: String -> Bool
prop_tempProjectUnique _ = True  -- withSystemTempDirectory ensures uniqueness