import qualified Parser as P
import qualified Compiler as C
import qualified Compiler.OwnershipChecker as OC

-- Test with package declaration
testOwnershipWithClosuresFixed :: IO ()
testOwnershipWithClosuresFixed = do
  let input = "//! ownership: on\npackage main\nfunc a() { s := NewMyString(\"hello\"); f := func() { fmt.Println(s.data) } }"
  putStrLn $ "Testing input: " ++ input
  case P.parseTypus input of
    Left err -> putStrLn $ "Parse failed: " ++ err
    Right parsed -> do
      putStrLn "Parse succeeded"
      putStrLn $ "Parsed file directives: " ++ show (P.tfDirectives parsed)
      putStrLn $ "Parsed blocks: " ++ show (length $ P.tfBlocks parsed)
      let ownershipResult = OC.checkOwnership parsed
      putStrLn $ "Ownership check result: " ++ show ownershipResult
      case C.compile parsed of
        Left errs -> putStrLn $ "Compilation failed (expected): " ++ show errs
        Right _ -> putStrLn "ERROR: Compilation should have failed!"

main :: IO ()
main = do
  putStrLn "=== Testing Ownership with Closures (Fixed) ==="
  testOwnershipWithClosuresFixed