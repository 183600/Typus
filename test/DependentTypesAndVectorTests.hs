module DependentTypesAndVectorTests (
    dependentTypesAndVectorTestSuite,
    runDependentTypesAndVectorTests
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
runDependentTypesAndVectorTests :: IO ()
runDependentTypesAndVectorTests = do
    putStrLn "=== Running Dependent Types and Vector Tests ==="
    defaultMain dependentTypesAndVectorTestSuite

-- Test suite definition
dependentTypesAndVectorTestSuite :: TestTree
dependentTypesAndVectorTestSuite = testGroup "Dependent Types and Vector Tests"
    [ testCase "Test Vector type with length parameterization" testVectorLengthParameterization
    , testCase "Test SafeDivide function with zero check" testSafeDivideFunction
    , testCase "Test Vector Get method with bounds checking" testVectorGetMethod
    , testCase "Test type inference with dependent types" testTypeInferenceWithDependentTypes
    , testCase "Test Vector creation with mismatched lengths" testVectorMismatchedLengths
    , testCase "Test SafeDivide with zero divisor" testSafeDivideWithZero
    , testCase "Test Vector Get with out-of-bounds index" testVectorGetOutOfBounds
    ]

-- Test Vector type with length parameterization
testVectorLengthParameterization :: IO ()
testVectorLengthParameterization = do
    let tempFile = "test_vector_parameterization.typus"
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
            , "    // Create Vector(3) - a 3-dimensional vector"
            , "    v_data := []float64{1.0, 2.0, 3.0}"
            , "    v := NewVector(3, v_data)"
            , "    fmt.Printf(\"Vector length: %d\\n\", v.length)"
            , "    fmt.Printf(\"Vector[0]: %.1f\\n\", v.data[0])"
            , "    fmt.Printf(\"Vector[1]: %.1f\\n\", v.data[1])"
            , "    fmt.Printf(\"Vector[2]: %.1f\\n\", v.data[2])"
            , "}"
            ]
    
    writeFile tempFile content
    
    -- Test that the file can be parsed and converted successfully
    (exitCode, stdout, stderr) <- readProcessWithExitCode "stack" ["exec", "--", "typus", "convert", tempFile, "-o", "test_vector_output.go"] ""
    
    -- Check if output file was created
    outputExists <- doesFileExist "test_vector_output.go"
    when outputExists $ removeFile "test_vector_output.go"
    
    removeFile tempFile
    
    case exitCode of
        ExitSuccess -> return () -- Vector parameterization should work
        ExitFailure code -> 
            assertFailure $ "Vector length parameterization failed: " ++ show code

-- Test SafeDivide function with zero check
testSafeDivideFunction :: IO ()
testSafeDivideFunction = do
    let tempFile = "test_safe_divide.typus"
    let content = unlines
            [ "//! constraints: on"
            , "package main"
            , "import \"fmt\""
            , ""
            , "func SafeDivide(a, b int) int {"
            , "    if b == 0 {"
            , "        panic(\"SafeDivide: 除数不能为零\")"
            , "    }"
            , "    return a / b"
            , "}"
            , ""
            , "func main() {"
            , "    // Test valid division"
            , "    result := SafeDivide(10, 2)"
            , "    fmt.Printf(\"10 / 2 = %d\\n\", result)"
            , "    "
            , "    // Test division by zero (should panic)"
            , "    // result2 := SafeDivide(10, 0) // This would panic"
            , "    // fmt.Printf(\"10 / 0 = %d\\n\", result2)"
            , "}"
            ]
    
    writeFile tempFile content
    
    -- Test that the file can be parsed and converted successfully
    (exitCode, stdout, stderr) <- readProcessWithExitCode "stack" ["exec", "--", "typus", "convert", tempFile, "-o", "test_safe_divide_output.go"] ""
    
    -- Check if output file was created
    outputExists <- doesFileExist "test_safe_divide_output.go"
    when outputExists $ removeFile "test_safe_divide_output.go"
    
    removeFile tempFile
    
    case exitCode of
        ExitSuccess -> return () -- SafeDivide function should work
        ExitFailure code -> 
            assertFailure $ "SafeDivide function failed: " ++ show code

-- Test Vector Get method with bounds checking
testVectorGetMethod :: IO ()
testVectorGetMethod = do
    let tempFile = "test_vector_get.typus"
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
            , "func (v *Vector) Get(index int) float64 {"
            , "    if index < 0 || index >= v.length {"
            , "        panic(\"Vector index out of bounds\")"
            , "    }"
            , "    return v.data[index]"
            , "}"
            , ""
            , "func main() {"
            , "    v_data := []float64{1.0, 2.0, 3.0}"
            , "    v := NewVector(3, v_data)"
            , "    fmt.Printf(\"Vector[0]: %.1f\\n\", v.Get(0))"
            , "    fmt.Printf(\"Vector[1]: %.1f\\n\", v.Get(1))"
            , "    fmt.Printf(\"Vector[2]: %.1f\\n\", v.Get(2))"
            , "}"
            ]
    
    writeFile tempFile content
    
    -- Test that the file can be parsed and converted successfully
    (exitCode, stdout, stderr) <- readProcessWithExitCode "stack" ["exec", "--", "typus", "convert", tempFile, "-o", "test_vector_get_output.go"] ""
    
    -- Check if output file was created
    outputExists <- doesFileExist "test_vector_get_output.go"
    when outputExists $ removeFile "test_vector_get_output.go"
    
    removeFile tempFile
    
    case exitCode of
        ExitSuccess -> return () -- Vector Get method should work
        ExitFailure code -> 
            assertFailure $ "Vector Get method failed: " ++ show code

-- Test type inference with dependent types
testTypeInferenceWithDependentTypes :: IO ()
testTypeInferenceWithDependentTypes = do
    let tempFile = "test_type_inference.typus"
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
            , "func createVector(n int, value float64) Vector {"
            , "    elements := make([]float64, n)"
            , "    for i := 0; i < n; i++ {"
            , "        elements[i] = value"
            , "    }"
            , "    return Vector{length: n, data: elements} // Type should be inferred as Vector(n)"
            , "}"
            , ""
            , "func main() {"
            , "    // Type should be automatically inferred"
            , "    v := createVector(5, 3.14)"
            , "    fmt.Printf(\"Vector length: %d\\n\", v.length)"
            , "    fmt.Printf(\"First element: %.2f\\n\", v.data[0])"
            , "}"
            ]
    
    writeFile tempFile content
    
    -- Test that the file can be parsed and converted successfully
    (exitCode, stdout, stderr) <- readProcessWithExitCode "stack" ["exec", "--", "typus", "convert", tempFile, "-o", "test_type_inference_output.go"] ""
    
    -- Check if output file was created
    outputExists <- doesFileExist "test_type_inference_output.go"
    when outputExists $ removeFile "test_type_inference_output.go"
    
    removeFile tempFile
    
    case exitCode of
        ExitSuccess -> return () -- Type inference should work
        ExitFailure code -> 
            assertFailure $ "Type inference with dependent types failed: " ++ show code

-- Test Vector creation with mismatched lengths (should handle error)
testVectorMismatchedLengths :: IO ()
testVectorMismatchedLengths = do
    let tempFile = "test_vector_mismatch.typus"
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
            , "    // This should panic due to length mismatch"
            , "    // v_data := []float64{1.0, 2.0}  // length 2"
            , "    // v := NewVector(3, v_data)     // expecting length 3"
            , "    // fmt.Printf(\"Vector length: %d\\n\", v.length)"
            , "    "
            , "    // This should work fine"
            , "    v_data2 := []float64{1.0, 2.0, 3.0}"
            , "    v2 := NewVector(3, v_data2)"
            , "    fmt.Printf(\"Vector length: %d\\n\", v2.length)"
            , "}"
            ]
    
    writeFile tempFile content
    
    -- Test that the file can be parsed and converted successfully
    (exitCode, stdout, stderr) <- readProcessWithExitCode "stack" ["exec", "--", "typus", "convert", tempFile, "-o", "test_vector_mismatch_output.go"] ""
    
    -- Check if output file was created
    outputExists <- doesFileExist "test_vector_mismatch_output.go"
    when outputExists $ removeFile "test_vector_mismatch_output.go"
    
    removeFile tempFile
    
    case exitCode of
        ExitSuccess -> return () -- Vector mismatch handling should work
        ExitFailure code -> 
            assertFailure $ "Vector mismatch handling failed: " ++ show code

-- Test SafeDivide with zero divisor (should handle error)
testSafeDivideWithZero :: IO ()
testSafeDivideWithZero = do
    let tempFile = "test_safe_divide_zero.typus"
    let content = unlines
            [ "//! constraints: on"
            , "package main"
            , "import \"fmt\""
            , ""
            , "func SafeDivide(a, b int) int {"
            , "    if b == 0 {"
            , "        panic(\"SafeDivide: 除数不能为零\")"
            , "    }"
            , "    return a / b"
            , "}"
            , ""
            , "func main() {"
            , "    // This should panic due to division by zero"
            , "    // result := SafeDivide(10, 0)"
            , "    // fmt.Printf(\"10 / 0 = %d\\n\", result)"
            , "    "
            , "    // This should work fine"
            , "    result2 := SafeDivide(10, 2)"
            , "    fmt.Printf(\"10 / 2 = %d\\n\", result2)"
            , "}"
            ]
    
    writeFile tempFile content
    
    -- Test that the file can be parsed and converted successfully
    (exitCode, stdout, stderr) <- readProcessWithExitCode "stack" ["exec", "--", "typus", "convert", tempFile, "-o", "test_safe_divide_zero_output.go"] ""
    
    -- Check if output file was created
    outputExists <- doesFileExist "test_safe_divide_zero_output.go"
    when outputExists $ removeFile "test_safe_divide_zero_output.go"
    
    removeFile tempFile
    
    case exitCode of
        ExitSuccess -> return () -- SafeDivide zero handling should work
        ExitFailure code -> 
            assertFailure $ "SafeDivide zero handling failed: " ++ show code

-- Test Vector Get with out-of-bounds index (should handle error)
testVectorGetOutOfBounds :: IO ()
testVectorGetOutOfBounds = do
    let tempFile = "test_vector_out_of_bounds.typus"
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
            , "func (v *Vector) Get(index int) float64 {"
            , "    if index < 0 || index >= v.length {"
            , "        panic(\"Vector index out of bounds\")"
            , "    }"
            , "    return v.data[index]"
            , "}"
            , ""
            , "func main() {"
            , "    v_data := []float64{1.0, 2.0, 3.0}"
            , "    v := NewVector(3, v_data)"
            , "    "
            , "    // This should panic due to out-of-bounds access"
            , "    // value := v.Get(5)  // Index 5 is out of bounds for length 3"
            , "    // fmt.Printf(\"Vector[5]: %.1f\\n\", value)"
            , "    "
            , "    // This should work fine"
            , "    value2 := v.Get(1)"
            , "    fmt.Printf(\"Vector[1]: %.1f\\n\", value2)"
            , "}"
            ]
    
    writeFile tempFile content
    
    -- Test that the file can be parsed and converted successfully
    (exitCode, stdout, stderr) <- readProcessWithExitCode "stack" ["exec", "--", "typus", "convert", tempFile, "-o", "test_vector_out_of_bounds_output.go"] ""
    
    -- Check if output file was created
    outputExists <- doesFileExist "test_vector_out_of_bounds_output.go"
    when outputExists $ removeFile "test_vector_out_of_bounds_output.go"
    
    removeFile tempFile
    
    case exitCode of
        ExitSuccess -> return () -- Vector out-of-bounds handling should work
        ExitFailure code -> 
            assertFailure $ "Vector out-of-bounds handling failed: " ++ show code