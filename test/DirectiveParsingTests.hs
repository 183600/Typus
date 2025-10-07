module DirectiveParsingTests (
    directiveParsingTestSuite,
    runDirectiveParsingTests
) where

import Test.Tasty
import Test.Tasty.HUnit as HU
import System.Exit (ExitCode(..))
import System.Directory (doesFileExist, removeFile, createDirectoryIfMissing)
import System.FilePath ((</>))
import System.Process (readProcessWithExitCode)
import Control.Exception (try, SomeException)
import Control.Monad (when)
import System.IO (hPutStrLn, stderr)
import qualified System.IO as IO

-- Main test function
runDirectiveParsingTests :: IO ()
runDirectiveParsingTests = do
    putStrLn "=== Running Directive Parsing Tests ==="
    defaultMain directiveParsingTestSuite

-- Test suite definition
directiveParsingTestSuite :: TestTree
directiveParsingTestSuite = testGroup "Directive Parsing Tests"
    [ testCase "Test file-level ownership directive" testFileLevelOwnershipDirective
    , testCase "Test file-level constraints directive" testFileLevelConstraintsDirective
    , testCase "Test file-level dependent_types directive" testFileLevelDependentTypesDirective
    , testCase "Test block-level ownership directive" testBlockLevelOwnershipDirective
    , testCase "Test block-level constraints directive" testBlockLevelConstraintsDirective
    , testCase "Test block-level dependent_types directive" testBlockLevelDependentTypesDirective
    , testCase "Test combined directives in block" testCombinedDirectivesInBlock
    ]

-- Test file-level ownership directive
testFileLevelOwnershipDirective :: IO ()
testFileLevelOwnershipDirective = do
    let tempFile = "test_directive_ownership.typus"
    let content = unlines
            [ "//! ownership: on"
            , "package main"
            , "import \"fmt\""
            , ""
            , "type MyString struct {"
            , "    data string"
            , "}"
            , ""
            , "func NewMyString(s string) MyString {"
            , "    return MyString{data: s}"
            , "}"
            , ""
            , "func main() {"
            , "    s := NewMyString(\"hello\")"
            , "    t := s // Ownership transferred"
            , "    fmt.Println(t.data)"
            , "}"
            ]
    
    writeFile tempFile content
    
    -- Test that the file can be parsed/converted
    (exitCode, stdout, stderr) <- readProcessWithExitCode "stack" ["exec", "--", "typus", "check", tempFile] ""
    
    removeFile tempFile
    
    case exitCode of
        ExitSuccess -> return () -- File with ownership directive should be parseable
        ExitFailure code -> 
            assertFailure $ "File with ownership directive failed to parse: " ++ show code

-- Test file-level constraints directive
testFileLevelConstraintsDirective :: IO ()
testFileLevelConstraintsDirective = do
    let tempFile = "test_directive_constraints.typus"
    let content = unlines
            [ "//! constraints: on"
            , "package main"
            , "import \"fmt\""
            , ""
            , "type Vector struct {"
            , "    length int"
            , "    data   []float64"
            , "}"
            , ""
            , "func NewVector(length int, data []float64) *Vector {"
            , "    if len(data) != length {"
            , "        panic(\"Vector data length doesn't match dimension\")"
            , "    }"
            , "    return &Vector{length: length, data: data}"
            , "}"
            , ""
            , "func main() {"
            , "    v_data := []float64{1.0, 2.0, 3.0}"
            , "    v := NewVector(3, v_data)"
            , "    fmt.Printf(\"Vector length: %d\\n\", v.length)"
            , "}"
            ]
    
    writeFile tempFile content
    
    -- Test that the file can be parsed/converted
    (exitCode, stdout, stderr) <- readProcessWithExitCode "stack" ["exec", "--", "typus", "check", tempFile] ""
    
    removeFile tempFile
    
    case exitCode of
        ExitSuccess -> return () -- File with constraints directive should be parseable
        ExitFailure code -> 
            assertFailure $ "File with constraints directive failed to parse: " ++ show code

-- Test file-level dependent_types directive
testFileLevelDependentTypesDirective :: IO ()
testFileLevelDependentTypesDirective = do
    let tempFile = "test_directive_dependent_types.typus"
    let content = unlines
            [ "//! dependent_types: on"
            , "package main"
            , "import \"fmt\""
            , ""
            , "type Vector struct {"
            , "    length int"
            , "    data   []float64"
            , "}"
            , ""
            , "func NewVector(length int, data []float64) *Vector {"
            , "    if len(data) != length {"
            , "        panic(\"Vector data length doesn't match dimension\")"
            , "    }"
            , "    return &Vector{length: length, data: data}"
            , "}"
            , ""
            , "func main() {"
            , "    v_data := []float64{1.0, 2.0, 3.0}"
            , "    v := NewVector(3, v_data)"
            , "    fmt.Printf(\"Vector length: %d\\n\", v.length)"
            , "}"
            ]
    
    writeFile tempFile content
    
    -- Test that the file can be parsed/converted
    (exitCode, stdout, stderr) <- readProcessWithExitCode "stack" ["exec", "--", "typus", "check", tempFile] ""
    
    removeFile tempFile
    
    case exitCode of
        ExitSuccess -> return () -- File with dependent_types directive should be parseable
        ExitFailure code -> 
            assertFailure $ "File with dependent_types directive failed to parse: " ++ show code

-- Test block-level ownership directive
testBlockLevelOwnershipDirective :: IO ()
testBlockLevelOwnershipDirective = do
    let tempFile = "test_block_ownership.typus"
    let content = unlines
            [ "package main"
            , "import \"fmt\""
            , ""
            , "type MyString struct {"
            , "    data string"
            , "}"
            , ""
            , "func NewMyString(s string) MyString {"
            , "    return MyString{data: s}"
            , "}"
            , ""
            , "func main() {"
            , "    // Regular Go code"
            , "    fmt.Println(\"Hello, Typus!\")"
            , "    "
            , "    {//! ownership: on"
            , "        // Ownership-enabled block"
            , "        s := NewMyString(\"hello\")"
            , "        t := s // Ownership transferred"
            , "        fmt.Println(t.data)"
            , "    }"
            , "}"
            ]
    
    writeFile tempFile content
    
    -- Test that the file can be parsed/converted
    (exitCode, stdout, stderr) <- readProcessWithExitCode "stack" ["exec", "--", "typus", "check", tempFile] ""
    
    removeFile tempFile
    
    case exitCode of
        ExitSuccess -> return () -- File with block-level ownership directive should be parseable
        ExitFailure code -> 
            assertFailure $ "File with block-level ownership directive failed to parse: " ++ show code

-- Test block-level constraints directive
testBlockLevelConstraintsDirective :: IO ()
testBlockLevelConstraintsDirective = do
    let tempFile = "test_block_constraints.typus"
    let content = unlines
            [ "package main"
            , "import \"fmt\""
            , ""
            , "type Vector struct {"
            , "    length int"
            , "    data   []float64"
            , "}"
            , ""
            , "func NewVector(length int, data []float64) *Vector {"
            , "    if len(data) != length {"
            , "        panic(\"Vector data length doesn't match dimension\")"
            , "    }"
            , "    return &Vector{length: length, data: data}"
            , "}"
            , ""
            , "func main() {"
            , "    // Regular Go code"
            , "    fmt.Println(\"Hello, Typus!\")"
            , "    "
            , "    {//! constraints: on"
            , "        // Dependent types block"
            , "        v_data := []float64{1.0, 2.0, 3.0}"
            , "        v := NewVector(3, v_data)"
            , "        fmt.Printf(\"Vector length: %d\\n\", v.length)"
            , "    }"
            , "}"
            ]
    
    writeFile tempFile content
    
    -- Test that the file can be parsed/converted
    (exitCode, stdout, stderr) <- readProcessWithExitCode "stack" ["exec", "--", "typus", "check", tempFile] ""
    
    removeFile tempFile
    
    case exitCode of
        ExitSuccess -> return () -- File with block-level constraints directive should be parseable
        ExitFailure code -> 
            assertFailure $ "File with block-level constraints directive failed to parse: " ++ show code

-- Test block-level dependent_types directive
testBlockLevelDependentTypesDirective :: IO ()
testBlockLevelDependentTypesDirective = do
    let tempFile = "test_block_dependent_types.typus"
    let content = unlines
            [ "package main"
            , "import \"fmt\""
            , ""
            , "type Vector struct {"
            , "    length int"
            , "    data   []float64"
            , "}"
            , ""
            , "func NewVector(length int, data []float64) *Vector {"
            , "    if len(data) != length {"
            , "        panic(\"Vector data length doesn't match dimension\")"
            , "    }"
            , "    return &Vector{length: length, data: data}"
            , "}"
            , ""
            , "func main() {"
            , "    // Regular Go code"
            , "    fmt.Println(\"Hello, Typus!\")"
            , "    "
            , "    {//! dependent_types: on"
            , "        // Dependent types block"
            , "        v_data := []float64{1.0, 2.0, 3.0}"
            , "        v := NewVector(3, v_data)"
            , "        fmt.Printf(\"Vector length: %d\\n\", v.length)"
            , "    }"
            , "}"
            ]
    
    writeFile tempFile content
    
    -- Test that the file can be parsed/converted
    (exitCode, stdout, stderr) <- readProcessWithExitCode "stack" ["exec", "--", "typus", "check", tempFile] ""
    
    removeFile tempFile
    
    case exitCode of
        ExitSuccess -> return () -- File with block-level dependent_types directive should be parseable
        ExitFailure code -> 
            assertFailure $ "File with block-level dependent_types directive failed to parse: " ++ show code

-- Test combined directives in block
testCombinedDirectivesInBlock :: IO ()
testCombinedDirectivesInBlock = do
    let tempFile = "test_combined_directives.typus"
    let content = unlines
            [ "package main"
            , "import \"fmt\""
            , ""
            , "type MyString struct {"
            , "    data string"
            , "}"
            , ""
            , "func NewMyString(s string) MyString {"
            , "    return MyString{data: s}"
            , "}"
            , ""
            , "type Vector struct {"
            , "    length int"
            , "    data   []float64"
            , "}"
            , ""
            , "func NewVector(length int, data []float64) *Vector {"
            , "    if len(data) != length {"
            , "        panic(\"Vector data length doesn't match dimension\")"
            , "    }"
            , "    return &Vector{length: length, data: data}"
            , "}"
            , ""
            , "func main() {"
            , "    // Regular Go code"
            , "    fmt.Println(\"Hello, Typus!\")"
            , "    "
            , "    {//! ownership: on, constraints: on"
            , "        // Block with both ownership and dependent types"
            , "        s := NewMyString(\"hello\")"
            , "        t := s // Ownership transferred"
            , "        fmt.Println(t.data)"
            , "        "
            , "        v_data := []float64{1.0, 2.0, 3.0}"
            , "        v := NewVector(3, v_data)"
            , "        fmt.Printf(\"Vector length: %d\\n\", v.length)"
            , "    }"
            , "}"
            ]
    
    writeFile tempFile content
    
    -- Test that the file can be parsed/converted
    (exitCode, stdout, stderr) <- readProcessWithExitCode "stack" ["exec", "--", "typus", "check", tempFile] ""
    
    removeFile tempFile
    
    case exitCode of
        ExitSuccess -> return () -- File with combined directives should be parseable
        ExitFailure code -> 
            assertFailure $ "File with combined directives failed to parse: " ++ show code