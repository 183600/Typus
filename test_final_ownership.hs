import qualified OwnershipComprehensive
import System.IO

testCases :: [(String, String, Int -> [OwnershipComprehensive.OwnershipError] -> Bool)]
testCases =
  [ ( "Basic immutable borrow"
    , unlines
      [ "//! ownership: on"
      , "func main() {"
      , "  x := 42"
      , "  y := &x"
      , "  println(x)"
      , "  println(y)"
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
      , "  take_value(data)"
      , "  println(data)"
      , "}"
      ]
    , \count errors -> 
        count > 0 && any (\e -> case e of OwnershipComprehensive.UseAfterMove "data" -> True; _ -> False) errors
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
      , "  println(data)"
      , "}"
      ]
    , \count errors -> 
        count > 0 && any (\e -> case e of OwnershipComprehensive.UseWhileMutBorrowed "data" -> True; _ -> False) errors
    )
  
  , ( "Cross-function ownership transfer"
    , unlines
      [ "//! ownership: on"
      , "func process(s string) string {"
      , "  return s"
      , "}"
      , "func main() {"
      , "  original := \"test\""
      , "  result := process(original)"
      , "  println(original)"
      , "  println(result)"
      , "}"
      ]
    , \count errors -> 
        count > 0 && any (\e -> case e of OwnershipComprehensive.UseAfterMove "original" -> True; _ -> False) errors
    )
  
  , ( "Multiple immutable borrows"
    , unlines
      [ "//! ownership: on"
      , "func main() {"
      , "  data := \"shared\""
      , "  ref1 := &data"
      , "  ref2 := &data"
      , "  println(ref1)"
      , "  println(ref2)"
      , "  println(data)"
      , "}"
      ]
    , \count errors -> count == 0
    )
  
  , ( "Borrow after move"
    , unlines
      [ "//! ownership: on"
      , "func main() {"
      , "  data := \"test\""
      , "  moved := data"
      , "  ref := &data"
      , "}"
      ]
    , \count errors -> 
        count > 0 && any (\e -> case e of OwnershipComprehensive.BorrowWhileMoved "data" -> True; _ -> False) errors
    )
  
  , ( "Move after borrow"
    , unlines
      [ "//! ownership: on"
      , "func main() {"
      , "  data := \"test\""
      , "  ref := &data"
      , "  moved := data"
      , "}"
      ]
    , \count errors -> count > 0
    )
  ]

runTest :: (String, String, Int -> [OwnershipComprehensive.OwnershipError] -> Bool) -> IO Bool
runTest (name, code, validator) = do
  putStrLn $ "\n=== " ++ name ++ " ==="
  putStrLn $ "Code:\n" ++ code
  let errors = OwnershipComprehensive.analyzeOwnership code
  putStrLn $ "Errors found: " ++ show (length errors)
  putStrLn $ "Error details: " ++ OwnershipComprehensive.formatOwnershipErrors errors
  
  let passed = validator (length errors) errors
  putStrLn $ if passed then "✓ PASSED" else "✗ FAILED"
  return passed

main :: IO ()
main = do
  putStrLn "=== Final Comprehensive Ownership Analysis Test Suite ==="
  
  results <- mapM runTest testCases
  
  let passed = length (filter id results)
      total = length results
  
  putStrLn $ "\n=== Test Summary ==="
  putStrLn $ "Passed: " ++ show passed ++ "/" ++ show total
  putStrLn $ "Success Rate: " ++ show (round (fromIntegral passed / fromIntegral total * 100) :: Int) ++ "%"
  
  if passed == total
    then putStrLn "🎉 All tests passed! Ownership mechanism is working correctly."
    else putStrLn "❌ Some tests failed. Ownership mechanism needs further refinement."