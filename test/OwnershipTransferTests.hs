module OwnershipTransferTests (
    ownershipTransferTestSuite,
    runOwnershipTransferTests
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
runOwnershipTransferTests :: IO ()
runOwnershipTransferTests = do
    putStrLn "=== Running Ownership Transfer Tests ==="
    defaultMain ownershipTransferTestSuite

-- Test suite definition
ownershipTransferTestSuite :: TestTree
ownershipTransferTestSuite = testGroup "Ownership Transfer Tests"
    [ testCase "Test basic ownership transfer" testBasicOwnershipTransfer
    , testCase "Test ownership transfer in function" testOwnershipTransferInFunction
    , testCase "Test ownership transfer with struct methods" testOwnershipTransferWithStructMethods
    , testCase "Test multiple ownership transfers" testMultipleOwnershipTransfers
    , testCase "Test ownership transfer in loop" testOwnershipTransferInLoop
    , testCase "Test ownership transfer with conditional" testOwnershipTransferWithConditional
    , testCase "Test ownership transfer with GC" testOwnershipTransferWithGC
    , testCase "Test ownership violation detection" testOwnershipViolationDetection
    , testCase "Test ownership with closures" testOwnershipWithClosures
    , testCase "Test ownership with channels" testOwnershipWithChannels
    , testCase "Test ownership with interfaces" testOwnershipWithInterfaces
    , testCase "Test ownership with generics" testOwnershipWithGenerics
    , testCase "Test ownership with slices and maps" testOwnershipWithSlicesAndMaps
    , testCase "Test ownership with defer statements" testOwnershipWithDeferStatements
    , testCase "Test ownership with error handling" testOwnershipWithErrorHandling
    , testCase "Test ownership with nested scopes" testOwnershipWithNestedScopes
    , testCase "Test ownership with multiple returns" testOwnershipWithMultipleReturns
    , testCase "Test ownership with pointers" testOwnershipWithPointers
    , testCase "Test ownership with struct embedding" testOwnershipWithStructEmbedding
    , testCase "Test ownership with reflection" testOwnershipWithReflection
    ]

-- Test basic ownership transfer
testBasicOwnershipTransfer :: IO ()
testBasicOwnershipTransfer = do
    let tempFile = "test_ownership_basic.typus"
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
            , "    // Create a MyString"
            , "    s := NewMyString(\"hello\")"
            , "    fmt.Printf(\"Original string: %s\\n\", s.data)"
            , "    "
            , "    // Transfer ownership from s to t"
            , "    t := s"
            , "    fmt.Printf(\"Transferred string: %s\\n\", t.data)"
            , "    "
            , "    // At this point, s should no longer be usable (ownership transferred)"
            , "    // fmt.Printf(\"Original string after transfer: %s\\n\", s.data) // This should be invalid"
            , "}"
            ]
    
    writeFile tempFile content
    
    -- Test that the file can be parsed and converted successfully
    (exitCode, stdout, stderr) <- readProcessWithExitCode "stack" ["exec", "--", "typus", "convert", tempFile, "-o", "test_ownership_basic_output.go"] ""
    
    -- Check if output file was created
    outputExists <- doesFileExist "test_ownership_basic_output.go"
    when outputExists $ removeFile "test_ownership_basic_output.go"
    
    removeFile tempFile
    
    case exitCode of
        ExitSuccess -> return () -- Basic ownership transfer should work
        ExitFailure code -> 
            assertFailure $ "Basic ownership transfer failed: " ++ show code

-- Test ownership transfer in function
testOwnershipTransferInFunction :: IO ()
testOwnershipTransferInFunction = do
    let tempFile = "test_ownership_function.typus"
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
            , "func processString(s MyString) MyString {"
            , "    // Ownership is transferred to this function"
            , "    fmt.Printf(\"Processing string: %s\\n\", s.data)"
            , "    "
            , "    // Create and return a new string"
            , "    return MyString{data: s.data + \" processed\"}"
            , "}"
            , ""
            , "func main() {"
            , "    // Create a MyString"
            , "    s := NewMyString(\"hello\")"
            , "    "
            , "    // Transfer ownership to processString function"
            , "    result := processString(s)"
            , "    fmt.Printf(\"Result: %s\\n\", result.data)"
            , "    "
            , "    // Original s should no longer be usable"
            , "    // fmt.Printf(\"Original string: %s\\n\", s.data) // This should be invalid"
            , "}"
            ]
    
    writeFile tempFile content
    
    -- Test that the file can be parsed and converted successfully
    (exitCode, stdout, stderr) <- readProcessWithExitCode "stack" ["exec", "--", "typus", "convert", tempFile, "-o", "test_ownership_function_output.go"] ""
    
    -- Check if output file was created
    outputExists <- doesFileExist "test_ownership_function_output.go"
    when outputExists $ removeFile "test_ownership_function_output.go"
    
    removeFile tempFile
    
    case exitCode of
        ExitSuccess -> return () -- Ownership transfer in function should work
        ExitFailure code -> 
            assertFailure $ "Ownership transfer in function failed: " ++ show code

-- Test ownership transfer with struct methods
testOwnershipTransferWithStructMethods :: IO ()
testOwnershipTransferWithStructMethods = do
    let tempFile = "test_ownership_methods.typus"
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
            , "func (s MyString) GetData() string {"
            , "    // This is a method that borrows the string (doesn't take ownership)"
            , "    return s.data"
            , "}"
            , ""
            , "func (s MyString) ToUpper() MyString {"
            , "    // This method creates a new string and transfers ownership"
            , "    return MyString{data: \"UPPERCASE\"}"
            , "}"
            , ""
            , "func main() {"
            , "    // Create a MyString"
            , "    s := NewMyString(\"hello\")"
            , "    "
            , "    // Call method that borrows (s still usable)"
            , "    data := s.GetData()"
            , "    fmt.Printf(\"Borrowed data: %s\\n\", data)"
            , "    fmt.Printf(\"Original still usable: %s\\n\", s.data)"
            , "    "
            , "    // Call method that transfers ownership"
            , "    upper := s.ToUpper()"
            , "    fmt.Printf(\"Uppercase: %s\\n\", upper.data)"
            , "    // At this point, s ownership may have been transferred"
            , "}"
            ]
    
    writeFile tempFile content
    
    -- Test that the file can be parsed and converted successfully
    (exitCode, stdout, stderr) <- readProcessWithExitCode "stack" ["exec", "--", "typus", "convert", tempFile, "-o", "test_ownership_methods_output.go"] ""
    
    -- Check if output file was created
    outputExists <- doesFileExist "test_ownership_methods_output.go"
    when outputExists $ removeFile "test_ownership_methods_output.go"
    
    removeFile tempFile
    
    case exitCode of
        ExitSuccess -> return () -- Ownership transfer with methods should work
        ExitFailure code -> 
            assertFailure $ "Ownership transfer with methods failed: " ++ show code

-- Test multiple ownership transfers
testMultipleOwnershipTransfers :: IO ()
testMultipleOwnershipTransfers = do
    let tempFile = "test_ownership_multiple.typus"
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
            , "    // Create multiple strings"
            , "    s1 := NewMyString(\"first\")"
            , "    s2 := NewMyString(\"second\")"
            , "    s3 := NewMyString(\"third\")"
            , "    "
            , "    // Chain of ownership transfers"
            , "    t1 := s1"
            , "    t2 := s2"
            , "    t3 := s3"
            , "    "
            , "    fmt.Printf(\"First: %s\\n\", t1.data)"
            , "    fmt.Printf(\"Second: %s\\n\", t2.data)"
            , "    fmt.Printf(\"Third: %s\\n\", t3.data)"
            , "    "
            , "    // Transfer between existing variables"
            , "    u1 := t2"
            , "    fmt.Printf(\"Transferred second to u1: %s\\n\", u1.data)"
            , "}"
            ]
    
    writeFile tempFile content
    
    -- Test that the file can be parsed and converted successfully
    (exitCode, stdout, stderr) <- readProcessWithExitCode "stack" ["exec", "--", "typus", "convert", tempFile, "-o", "test_ownership_multiple_output.go"] ""
    
    -- Check if output file was created
    outputExists <- doesFileExist "test_ownership_multiple_output.go"
    when outputExists $ removeFile "test_ownership_multiple_output.go"
    
    removeFile tempFile
    
    case exitCode of
        ExitSuccess -> return () -- Multiple ownership transfers should work
        ExitFailure code -> 
            assertFailure $ "Multiple ownership transfers failed: " ++ show code

-- Test ownership transfer in loop
testOwnershipTransferInLoop :: IO ()
testOwnershipTransferInLoop = do
    let tempFile = "test_ownership_loop.typus"
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
            , "    // Create an array of strings"
            , "    strings := []MyString{"
            , "        NewMyString(\"apple\"),"
            , "        NewMyString(\"banana\"),"
            , "        NewMyString(\"cherry\"),"
            , "    }"
            , "    "
            , "    // Process each string (transfer ownership temporarily)"
            , "    for i := 0; i < len(strings); i++ {"
            , "        // Transfer ownership from array element to local variable"
            , "        s := strings[i]"
            , "        fmt.Printf(\"Processing: %s\\n\", s.data)"
            , "        "
            , "        // Create a processed version"
            , "        processed := MyString{data: s.data + \" processed\"}"
            , "        fmt.Printf(\"Result: %s\\n\", processed.data)"
            , "    }"
            , "}"
            ]
    
    writeFile tempFile content
    
    -- Test that the file can be parsed and converted successfully
    (exitCode, stdout, stderr) <- readProcessWithExitCode "stack" ["exec", "--", "typus", "convert", tempFile, "-o", "test_ownership_loop_output.go"] ""
    
    -- Check if output file was created
    outputExists <- doesFileExist "test_ownership_loop_output.go"
    when outputExists $ removeFile "test_ownership_loop_output.go"
    
    removeFile tempFile
    
    case exitCode of
        ExitSuccess -> return () -- Ownership transfer in loop should work
        ExitFailure code -> 
            assertFailure $ "Ownership transfer in loop failed: " ++ show code

-- Test ownership transfer with conditional
testOwnershipTransferWithConditional :: IO ()
testOwnershipTransferWithConditional = do
    let tempFile = "test_ownership_conditional.typus"
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
            , "func processConditionally(s MyString, condition bool) MyString {"
            , "    if condition {"
            , "        return MyString{data: s.data + \" (condition true)\"}"
            , "    } else {"
            , "        return MyString{data: s.data + \" (condition false)\"}"
            , "    }"
            , "}"
            , ""
            , "func main() {"
            , "    // Create a string"
            , "    original := NewMyString(\"test\")"
            , "    "
            , "    // Conditionally transfer ownership"
            , "    condition := true"
            , "    result := processConditionally(original, condition)"
            , "    fmt.Printf(\"Result: %s\\n\", result.data)"
            , "    "
            , "    // Try with different condition"
            , "    original2 := NewMyString(\"test2\")"
            , "    result2 := processConditionally(original2, false)"
            , "    fmt.Printf(\"Result2: %s\\n\", result2.data)"
            , "}"
            ]
    
    writeFile tempFile content
    
    -- Test that the file can be parsed and converted successfully
    (exitCode, stdout, stderr) <- readProcessWithExitCode "stack" ["exec", "--", "typus", "convert", tempFile, "-o", "test_ownership_conditional_output.go"] ""
    
    -- Check if output file was created
    outputExists <- doesFileExist "test_ownership_conditional_output.go"
    when outputExists $ removeFile "test_ownership_conditional_output.go"
    
    removeFile tempFile
    
    case exitCode of
        ExitSuccess -> return () -- Ownership transfer with conditional should work
        ExitFailure code -> 
            assertFailure $ "Ownership transfer with conditional failed: " ++ show code

-- Test ownership transfer with GC (demonstrates that GC still works)
testOwnershipTransferWithGC :: IO ()
testOwnershipTransferWithGC = do
    let tempFile = "test_ownership_gc.typus"
    let content = unlines
            [ "//! ownership: on"
            , "package main"
            , "import \"fmt\""
            , "import \"runtime\""
            , ""
            , "type MyString struct {"
            , "    data string"
            , "}"
            , ""
            , "func NewMyString(s string) MyString {"
            , "    return MyString{data: s}"
            , "}"
            , ""
            , "func createAndTransfer() MyString {"
            , "    // Create a string in this function"
            , "    local := NewMyString(\"created in function\")"
            , "    fmt.Printf(\"Created: %s\\n\", local.data)"
            , "    "
            , "    // Transfer ownership to caller"
            , "    return local"
            , "}"
            , ""
            , "func main() {"
            , "    fmt.Println(\"Testing ownership transfer with GC...\")"
            , "    "
            , "    // Create and transfer ownership multiple times"
            , "    for i := 0; i < 5; i++ {"
            , "        transferred := createAndTransfer()"
            , "        fmt.Printf(\"Received in main: %s (iteration %d)\\n\", transferred.data, i)"
            , "        "
            , "        // Force GC to demonstrate it still works with ownership"
            , "        runtime.GC()"
            , "        fmt.Printf(\"After GC, string still valid: %s\\n\", transferred.data)"
            , "    }"
            , "    "
            , "    fmt.Println(\"Ownership transfer with GC test completed\")"
            , "}"
            ]
    
    writeFile tempFile content
    
    -- Test that the file can be parsed and converted successfully
    (exitCode, stdout, stderr) <- readProcessWithExitCode "stack" ["exec", "--", "typus", "convert", tempFile, "-o", "test_ownership_gc_output.go"] ""
    
    -- Check if output file was created
    outputExists <- doesFileExist "test_ownership_gc_output.go"
    when outputExists $ removeFile "test_ownership_gc_output.go"
    
    removeFile tempFile
    
    case exitCode of
        ExitSuccess -> return () -- Ownership transfer with GC should work
        ExitFailure code -> 
            assertFailure $ "Ownership transfer with GC failed: " ++ show code
-- Temporary stubs for missing tests (to be implemented)

testOwnershipViolationDetection :: IO ()
testOwnershipViolationDetection = do
    let tempFile = "test_ownership_violation.typus"
    let content = unlines
            [ "//! ownership: on"
            , "package main"
            , "type MyString struct { data string }"
            , "func NewMyString(s string) MyString { return MyString{data: s} }"
            , "func main() {"
            , "  s := NewMyString(\"x\")"
            , "  t := s"
            , "  _ = t"
            , "  _ = s.data"
            , "}"
            ]
    writeFile tempFile content
    (exitCode, _out, _err) <- readProcessWithExitCode "stack" ["exec","--","typus","convert", tempFile, "-o","test_ownership_violation_output.go"] ""
    outputExists <- doesFileExist "test_ownership_violation_output.go"
    when outputExists $ removeFile "test_ownership_violation_output.go"
    removeFile tempFile
    case exitCode of
        ExitSuccess -> assertFailure "Ownership violation not detected"
        ExitFailure _ -> return ()

testOwnershipWithClosures :: IO ()
testOwnershipWithClosures = do
    let tempFile = "test_ownership_closures.typus"
    let content = unlines
            [ "//! ownership: on"
            , "package main"
            , "type Box struct { v int }"
            , "func makeFn(b Box) func() int { return func() int { return b.v } }"
            , "func main() {"
            , "  b := Box{v: 42}"
            , "  f := makeFn(b)"
            , "  _ = f()"
            , "}"
            ]
    writeFile tempFile content
    (exitCode, _out, _err) <- readProcessWithExitCode "stack" ["exec","--","typus","convert", tempFile, "-o","test_ownership_closures_output.go"] ""
    outputExists <- doesFileExist "test_ownership_closures_output.go"
    when outputExists $ removeFile "test_ownership_closures_output.go"
    removeFile tempFile
    case exitCode of
        ExitSuccess -> return ()
        ExitFailure code -> assertFailure $ "Closures ownership test failed: " ++ show code

testOwnershipWithChannels :: IO ()
testOwnershipWithChannels = do
    let tempFile = "test_ownership_channels.typus"
    let content = unlines
            [ "//! ownership: on"
            , "package main"
            , "import \"fmt\""
            , "type Msg struct { data string }"
            , "func main() {"
            , "  ch := make(chan Msg, 1)"
            , "  m := Msg{data: \"hi\"}"
            , "  ch <- m"
            , "  r := <-ch"
            , "  fmt.Println(r.data)"
            , "}"
            ]
    writeFile tempFile content
    (exitCode, _out, _err) <- readProcessWithExitCode "stack" ["exec","--","typus","convert", tempFile, "-o","test_ownership_channels_output.go"] ""
    outputExists <- doesFileExist "test_ownership_channels_output.go"
    when outputExists $ removeFile "test_ownership_channels_output.go"
    removeFile tempFile
    case exitCode of
        ExitSuccess -> return ()
        ExitFailure code -> assertFailure $ "Channels ownership test failed: " ++ show code

testOwnershipWithInterfaces :: IO ()
testOwnershipWithInterfaces = do
    let tempFile = "test_ownership_interfaces.typus"
    let content = unlines
            [ "//! ownership: on"
            , "package main"
            , "type Sayer interface { Say() string }"
            , "type Person struct { name string }"
            , "func (p Person) Say() string { return p.name }"
            , "func speak(s Sayer) string { return s.Say() }"
            , "func main() {"
            , "  p := Person{name: \"Alice\"}"
            , "  _ = speak(p)"
            , "}"
            ]
    writeFile tempFile content
    (exitCode, _out, _err) <- readProcessWithExitCode "stack" ["exec","--","typus","convert", tempFile, "-o","test_ownership_interfaces_output.go"] ""
    outputExists <- doesFileExist "test_ownership_interfaces_output.go"
    when outputExists $ removeFile "test_ownership_interfaces_output.go"
    removeFile tempFile
    case exitCode of
        ExitSuccess -> return ()
        ExitFailure code -> assertFailure $ "Interfaces ownership test failed: " ++ show code

testOwnershipWithGenerics :: IO ()
testOwnershipWithGenerics = do
    let tempFile = "test_ownership_generics.typus"
    let content = unlines
            [ "//! ownership: on"
            , "package main"
            , "type Box<T> struct { v T }"
            , "func useBox<T>(b Box<T>) T { return b.v }"
            , "func main() {"
            , "  b := Box<int>{v: 1}"
            , "  _ = useBox(b)"
            , "}"
            ]
    writeFile tempFile content
    (exitCode, _out, _err) <- readProcessWithExitCode "stack" ["exec","--","typus","convert", tempFile, "-o","test_ownership_generics_output.go"] ""
    outputExists <- doesFileExist "test_ownership_generics_output.go"
    when outputExists $ removeFile "test_ownership_generics_output.go"
    removeFile tempFile
    case exitCode of
        ExitSuccess -> return ()
        ExitFailure code -> assertFailure $ "Generics ownership test failed: " ++ show code

testOwnershipWithSlicesAndMaps :: IO ()
testOwnershipWithSlicesAndMaps = do
    let tempFile = "test_ownership_slices_maps.typus"
    let content = unlines
            [ "//! ownership: on"
            , "package main"
            , "type Node struct { v string }"
            , "func main() {"
            , "  s := []Node{ {v: \"a\"}, {v: \"b\"} }"
            , "  m := map[string]Node{ \"k\": {v: \"x\"} }"
            , "  x := s[0]"
            , "  _ = x.v"
            , "  y := m[\"k\"]"
            , "  _ = y.v"
            , "}"
            ]
    writeFile tempFile content
    (exitCode, _out, _err) <- readProcessWithExitCode "stack" ["exec","--","typus","convert", tempFile, "-o","test_ownership_slices_maps_output.go"] ""
    outputExists <- doesFileExist "test_ownership_slices_maps_output.go"
    when outputExists $ removeFile "test_ownership_slices_maps_output.go"
    removeFile tempFile
    case exitCode of
        ExitSuccess -> return ()
        ExitFailure code -> assertFailure $ "Slices/maps ownership test failed: " ++ show code

testOwnershipWithDeferStatements :: IO ()
testOwnershipWithDeferStatements = do
    let tempFile = "test_ownership_defer.typus"
    let content = unlines
            [ "//! ownership: on"
            , "package main"
            , "import \"fmt\""
            , "type Res struct { id int }"
            , "func closeRes(r Res) { fmt.Println(r.id) }"
            , "func main() {"
            , "  r := Res{id: 1}"
            , "  defer closeRes(r)"
            , "}"
            ]
    writeFile tempFile content
    (exitCode, _out, _err) <- readProcessWithExitCode "stack" ["exec","--","typus","convert", tempFile, "-o","test_ownership_defer_output.go"] ""
    outputExists <- doesFileExist "test_ownership_defer_output.go"
    when outputExists $ removeFile "test_ownership_defer_output.go"
    removeFile tempFile
    case exitCode of
        ExitSuccess -> return ()
        ExitFailure code -> assertFailure $ "Defer ownership test failed: " ++ show code

testOwnershipWithErrorHandling :: IO ()
testOwnershipWithErrorHandling = do
    let tempFile = "test_ownership_error.typus"
    let content = unlines
            [ "//! ownership: on"
            , "package main"
            , "type Data struct { v int }"
            , "func mayFail(d Data) (Data, error) { return d, nil }"
            , "func main() {"
            , "  d := Data{v: 1}"
            , "  r, err := mayFail(d)"
            , "  if err != nil { return }"
            , "  _ = r.v"
            , "}"
            ]
    writeFile tempFile content
    (exitCode, _out, _err) <- readProcessWithExitCode "stack" ["exec","--","typus","convert", tempFile, "-o","test_ownership_error_output.go"] ""
    outputExists <- doesFileExist "test_ownership_error_output.go"
    when outputExists $ removeFile "test_ownership_error_output.go"
    removeFile tempFile
    case exitCode of
        ExitSuccess -> return ()
        ExitFailure code -> assertFailure $ "Error handling ownership test failed: " ++ show code

testOwnershipWithNestedScopes :: IO ()
testOwnershipWithNestedScopes = do
    let tempFile = "test_ownership_nested_scopes.typus"
    let content = unlines
            [ "//! ownership: on"
            , "package main"
            , "type Wrap struct { v string }"
            , "func main() {"
            , "  w := Wrap{v: \"n\"}"
            , "  {"
            , "    x := w"
            , "    _ = x.v"
            , "  }"
            , "  _ = w.v"
            , "}"
            ]
    writeFile tempFile content
    (exitCode, _out, _err) <- readProcessWithExitCode "stack" ["exec","--","typus","convert", tempFile, "-o","test_ownership_nested_scopes_output.go"] ""
    outputExists <- doesFileExist "test_ownership_nested_scopes_output.go"
    when outputExists $ removeFile "test_ownership_nested_scopes_output.go"
    removeFile tempFile
    case exitCode of
        ExitSuccess -> assertFailure "Nested scopes violation not detected"
        ExitFailure _ -> return ()

testOwnershipWithMultipleReturns :: IO ()
testOwnershipWithMultipleReturns = do
    let tempFile = "test_ownership_multiple_returns.typus"
    let content = unlines
            [ "//! ownership: on"
            , "package main"
            , "type T struct { v int }"
            , "func f(x T, y T) (T, T) { return x, y }"
            , "func main() {"
            , "  a := T{v: 1}"
            , "  b := T{v: 2}"
            , "  r1, r2 := f(a, b)"
            , "  _ = r1.v + r2.v"
            , "}"
            ]
    writeFile tempFile content
    (exitCode, _out, _err) <- readProcessWithExitCode "stack" ["exec","--","typus","convert", tempFile, "-o","test_ownership_multiple_returns_output.go"] ""
    outputExists <- doesFileExist "test_ownership_multiple_returns_output.go"
    when outputExists $ removeFile "test_ownership_multiple_returns_output.go"
    removeFile tempFile
    case exitCode of
        ExitSuccess -> return ()
        ExitFailure code -> assertFailure $ "Multiple returns ownership test failed: " ++ show code

testOwnershipWithPointers :: IO ()
testOwnershipWithPointers = do
    let tempFile = "test_ownership_pointers.typus"
    let content = unlines
            [ "//! ownership: on"
            , "package main"
            , "type Node struct { val int }"
            , "func newNode(v int) *Node { return &Node{val: v} }"
            , "func use(n *Node) int { return n.val }"
            , "func main() {"
            , "  p := newNode(3)"
            , "  _ = use(p)"
            , "}"
            ]
    writeFile tempFile content
    (exitCode, _out, _err) <- readProcessWithExitCode "stack" ["exec","--","typus","convert", tempFile, "-o","test_ownership_pointers_output.go"] ""
    outputExists <- doesFileExist "test_ownership_pointers_output.go"
    when outputExists $ removeFile "test_ownership_pointers_output.go"
    removeFile tempFile
    case exitCode of
        ExitSuccess -> return ()
        ExitFailure code -> assertFailure $ "Pointers ownership test failed: " ++ show code

testOwnershipWithStructEmbedding :: IO ()
testOwnershipWithStructEmbedding = do
    let tempFile = "test_ownership_embedding.typus"
    let content = unlines
            [ "//! ownership: on"
            , "package main"
            , "type Base struct { x int }"
            , "type Child struct { Base }"
            , "func (b Base) X() int { return b.x }"
            , "func main() {"
            , "  c := Child{Base: Base{x: 7}}"
            , "  _ = c.X()"
            , "}"
            ]
    writeFile tempFile content
    (exitCode, _out, _err) <- readProcessWithExitCode "stack" ["exec","--","typus","convert", tempFile, "-o","test_ownership_embedding_output.go"] ""
    outputExists <- doesFileExist "test_ownership_embedding_output.go"
    when outputExists $ removeFile "test_ownership_embedding_output.go"
    removeFile tempFile
    case exitCode of
        ExitSuccess -> return ()
        ExitFailure code -> assertFailure $ "Struct embedding ownership test failed: " ++ show code

testOwnershipWithReflection :: IO ()
testOwnershipWithReflection = do
    let tempFile = "test_ownership_reflection.typus"
    let content = unlines
            [ "//! ownership: on"
            , "package main"
            , "import \"reflect\""
            , "type A struct { n int }"
            , "func main() {"
            , "  a := A{n: 1}"
            , "  _ = reflect.TypeOf(a).NumField()"
            , "}"
            ]
    writeFile tempFile content
    (exitCode, _out, _err) <- readProcessWithExitCode "stack" ["exec","--","typus","convert", tempFile, "-o","test_ownership_reflection_output.go"] ""
    outputExists <- doesFileExist "test_ownership_reflection_output.go"
    when outputExists $ removeFile "test_ownership_reflection_output.go"
    removeFile tempFile
    case exitCode of
        ExitSuccess -> return ()
        ExitFailure code -> assertFailure $ "Reflection ownership test failed: " ++ show code

