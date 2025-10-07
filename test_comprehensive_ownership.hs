import qualified OwnershipAdvanced
import System.IO

testCases :: [(String, String, Int -> [OwnershipAdvanced.OwnershipError] -> Bool)]
testCases =
  [ ( "Basic immutable borrow"
    , unlines
      [ "//! ownership: on"
      , "func main() {"
      , "  x := 42"
      , "  y := &x"
      , "  println(x)  // Should be OK"
      , "  println(y)  // Should be OK"
      , "}"
      ]
    , \count errors -> count == 0
    )
  
  , ( "Function parameter move"
    , unlines
      [ "//! ownership: on"
      , "func take_value(s string) {"
      , "  println(s)"
      , "}"
      , "func main() {"
      , "  data := \"hello\""
      , "  take_value(data)  // Moves data"
      , "  println(data)     // Should be UseAfterMove error"
      , "}"
      ]
    , \count errors -> 
        count > 0 && any (\e -> case e of OwnershipAdvanced.UseAfterMove "data" -> True; _ -> False) errors
    )
  
  , ( "Mutable borrow conflict"
    , unlines
      [ "//! ownership: on"
      , "func take_mut_borrow(s &mut string) {"
      , "  *s = \"modified\""
      , "}"
      , "func main() {"
      , "  data := \"hello\""
      , "  take_mut_borrow(&mut data)"
      , "  println(data)  // Should be UseWhileMutBorrowed error"
      , "}"
      ]
    , \count errors -> 
        count > 0 && any (\e -> case e of OwnershipAdvanced.UseWhileMutBorrowed "data" -> True; _ -> False) errors
    )
  
  , ( "Cross-function ownership transfer"
    , unlines
      [ "//! ownership: on"
      , "func process(s string) string {"
      , "  return s + \"_processed\""
      , "}"
      , "func main() {"
      , "  original := \"test\""
      , "  result := process(original)  // original is moved"
      , "  println(original)  // Should be UseAfterMove error"
      , "  println(result)    // Should be OK"
      , "}"
      ]
    , \count errors -> 
        count > 0 && any (\e -> case e of OwnershipAdvanced.UseAfterMove "original" -> True; _ -> False) errors
    )
  
  , ( "Multiple immutable borrows"
    , unlines
      [ "//! ownership: on"
      , "func main() {"
      , "  data := \"shared\""
      , "  ref1 := &data"
      , "  ref2 := &data"
      , "  println(ref1)  // Should be OK"
      , "  println(ref2)  // Should be OK"
      , "  println(data)  // Should be OK"
      , "}"
      ]
    , \count errors -> count == 0
    )
  
  , ( "Mixed parameter types"
    , unlines
      [ "//! ownership: on"
      , "func mixed_func(move_param string, borrow_param &string, mut_param &mut string) {"
      , "  println(move_param)"
      , "  println(*borrow_param)"
      , "  *mut_param = \"changed\""
      , "}"
      , "func main() {"
      , "  val := \"moved\""
      , "  ref := \"borrowed\""
      , "  mut := \"mutable\""
      , "  mixed_func(val, &ref, &mut mut)"
      , "  println(val)  // Should be UseAfterMove error"
      , "  println(ref)  // Should be OK"
      , "  println(mut)  // Should be UseWhileMutBorrowed error"
      , "}"
      ]
    , \count errors -> 
        count >= 2 && 
        any (\e -> case e of OwnershipAdvanced.UseAfterMove "val" -> True; _ -> False) errors &&
        any (\e -> case e of OwnershipAdvanced.UseWhileMutBorrowed "mut" -> True; _ -> False) errors
    )
  
  , ( "Borrow after move"
    , unlines
      [ "//! ownership: on"
      , "func main() {"
      , "  data := \"test\""
      , "  moved := data  // data is moved"
      , "  ref := &data   // Should be BorrowWhileMoved error"
      , "}"
      ]
    , \count errors -> 
        count > 0 && any (\e -> case e of OwnershipAdvanced.BorrowWhileMoved "data" -> True; _ -> False) errors
    )
  
  , ( "Move after borrow"
    , unlines
      [ "//! ownership: on"
      , "func main() {"
      , "  data := \"test\""
      , "  ref := &data  // data is borrowed"
      , "  moved := data  // Should be error - cannot move borrowed value"
      , "}"
      ]
    , \count errors -> count > 0
    )
  ]

runTest :: (String, String, Int -> [OwnershipAdvanced.OwnershipError] -> Bool) -> IO Bool
runTest (name, code, validator) = do
  putStrLn $ "\n=== " ++ name ++ " ==="
  putStrLn $ "Code:\n" ++ code
  let errors = OwnershipAdvanced.analyzeOwnership code
  putStrLn $ "Errors found: " ++ show (length errors)
  putStrLn $ "Error details: " ++ OwnershipAdvanced.formatOwnershipErrors errors
  
  let passed = validator (length errors) errors
  putStrLn $ if passed then "✓ PASSED" else "✗ FAILED"
  return passed

main :: IO ()
main = do
  putStrLn "=== Comprehensive Ownership Analysis Test Suite ==="
  
  results <- mapM runTest testCases
  
  let passed = length (filter id results)
      total = length results
  
  putStrLn $ "\n=== Test Summary ==="
  putStrLn $ "Passed: " ++ show passed ++ "/" ++ show total
  putStrLn $ "Success Rate: " ++ show (round (fromIntegral passed / fromIntegral total * 100) :: Int) ++ "%"
  
  if passed == total
    then putStrLn "🎉 All tests passed! Ownership mechanism is working correctly."
    else putStrLn "❌ Some tests failed. Ownership mechanism needs further refinement."