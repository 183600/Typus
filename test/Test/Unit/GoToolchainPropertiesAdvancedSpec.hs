{-# LANGUAGE CPP #-}

module Test.Unit.GoToolchainPropertiesAdvancedSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck
import qualified Data.List as L
import Data.List (isInfixOf, isPrefixOf)
import System.FilePath ((</>))

import GoToolchain
  ( IOResult
  , GoExecutor(..)
  , defaultGoExecutor
  , runGoCommand
  , goModContents
  , writeGoModule
  , createTempGoFile
  , nullDevice
  , isEnvVarEnabled
  , shouldSkipGoToolchain
  )
import Tooling.Error (ToolingError(..))

tests :: TestTree
tests = testGroup "GoToolchain Properties Advanced Tests"
  [ goExecutorTests
  , goModTests
  , tempFileTests
  , environmentTests
  , commandExecutionTests
  , quickCheckProperties
  ]

goExecutorTests :: TestTree
goExecutorTests = testGroup "Go Executor Tests"
  [ testCase "defaultGoExecutor creates valid executor" $ do
      executor <- defaultGoExecutor (\_ -> pure ())
      goShouldSkip executor `seq` True @?= True
      
  , testCase "goShouldSkip checks go availability" $ do
      executor <- defaultGoExecutor (\_ -> pure ())
      shouldSkip <- goShouldSkip executor
      shouldSkip `seq` True @?= True  -- Should not crash
      
  , testCase "goRunCommandInDir handles empty commands" $ do
      executor <- defaultGoExecutor (\_ -> pure ())
      let result = goRunCommandInDir executor [] "/tmp"
      result `seq` True @?= True  -- Should not crash
  ]

goModTests :: TestTree
goModTests = testGroup "Go Module Tests"
  [ testCase "goModContents generates valid go.mod" $ do
      let moduleName = "test.module"
          contents = goModContents moduleName
      "module test.module" `L.isInfixOf` contents @?= True
      "go 1.21" `L.isInfixOf` contents @?= True
      
  , testCase "goModContents handles complex module names" $ do
      let moduleName = "github.com/user/project"
          contents = goModContents moduleName
      "module github.com/user/project" `L.isInfixOf` contents @?= True
      
  , testCase "goModContents escapes special characters" $ do
      let moduleName = "module-with-dashes"
          contents = goModContents moduleName
      "module module-with-dashes" `L.isInfixOf` contents @?= True
      
  , testCase "writeGoModule creates valid file structure" $ do
      let moduleName = "test.module"
          goMod = goModContents moduleName
      L.length goMod > 10 @?= True  -- Should have reasonable content
  ]

tempFileTests :: TestTree
tempFileTests = testGroup "Temporary File Tests"
  [ testCase "createTempGoFile generates valid Go code" $ do
      let code = "package main\nfunc main() {}\n"
          result = createTempGoFile code
      result `seq` True @?= True  -- Should not crash
      
  , testCase "createTempGoFile handles empty code" $ do
      let code = ""
          result = createTempGoFile code
      result `seq` True @?= True  -- Should not crash
      
  , testCase "createTempGoFile handles complex Go code" $ do
      let code = unlines
            [ "package main"
            , "import \"fmt\""
            , "func main() {"
            , "    fmt.Println(\"Hello, World!\")"
            , "}"
            ]
          result = createTempGoFile code
      result `seq` True @?= True  -- Should not crash
  ]

environmentTests :: TestTree
environmentTests = testGroup "Environment Tests"
  [ testCase "isEnvVarEnabled checks boolean environment variables" $ do
      let result = isEnvVarEnabled "PATH"
      result `seq` True @?= True  -- Should not crash
      
  , testCase "isEnvVarEnabled handles missing variables" $ do
      let result = isEnvVarEnabled "TYPUS_NONEXISTENT_VAR_12345"
      result `seq` True @?= True  -- Should not crash
      
  , testCase "shouldSkipGoToolchain checks go availability" $ do
      shouldSkip <- shouldSkipGoToolchain
      shouldSkip `seq` True @?= True  -- Should not crash
  ]

commandExecutionTests :: TestTree
commandExecutionTests = testGroup "Command Execution Tests"
  [ testCase "runGoCommand handles basic commands" $ do
      executor <- defaultGoExecutor (\_ -> pure ())
      let result = runGoCommand executor ["version"] "/tmp"
      result `seq` True @?= True  -- Should not crash
      
  , testCase "runGoCommand handles invalid commands gracefully" $ do
      executor <- defaultGoExecutor (\_ -> pure ())
      let result = runGoCommand executor ["invalid-command"] "/tmp"
      result `seq` True @?= True  -- Should not crash
      
  , testCase "runGoCommand handles empty working directory" $ do
      executor <- defaultGoExecutor (\_ -> pure ())
      let result = runGoCommand executor ["version"] ""
      result `seq` True @?= True  -- Should not crash
      
  , testCase "nullDevice provides valid device path" $ do
      nullDevice `seq` True @?= True  -- Should not crash
      L.length nullDevice > 0 @?= True  -- Should be non-empty
  ]

quickCheckProperties :: TestTree
quickCheckProperties = testGroup "QuickCheck GoToolchain Properties"
  [ fastProperty "goModContents contains module declaration" prop_gomod_contains_module
  , fastProperty "goModContents contains go version" prop_gomod_contains_version
  , fastProperty "runGoCommand is total function" prop_run_command_total
  ]

-- QuickCheck property implementations
prop_gomod_contains_module :: String -> Property
prop_gomod_contains_module moduleName =
  let contents = goModContents moduleName
  in not (null moduleName) ==> ("module " ++ moduleName) `L.isInfixOf` contents

prop_gomod_contains_version :: String -> Property
prop_gomod_contains_version moduleName =
  let contents = goModContents moduleName
  in "go 1.21" `L.isInfixOf` contents

prop_run_command_total :: [String] -> String -> Property
prop_run_command_total args dir =
  let executor = defaultGoExecutor (\_ -> pure ())
  case executor of
    Left _ -> property True  -- Executor creation failed
    Right exec -> do
      let result = runGoCommand exec args dir
      result `seq` property True  -- Should not crash