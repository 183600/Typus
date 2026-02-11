import qualified Utils as U
import Test.QuickCheck

main :: IO ()
main = do
    -- Simulate the Moderate memory level test settings
    let maxTestSize = 5
    let numTests = 20
    
    putStrLn $ "Testing with Moderate memory level settings:"
    putStrLn $ "MaxSize: " ++ show maxTestSize
    putStrLn $ "NumTests: " ++ show numTests
    
    -- Test the property with these settings
    result <- quickCheckWithResult stdArgs 
        { maxSuccess = numTests
        , maxSize = maxTestSize
        } prop_is_problematic_unclosed_string
    
    putStrLn $ "Test result: " ++ show result
    
    -- Also test the specific failing case
    let failingInput = "a\""
    putStrLn $ "\nTesting specific failing case: " ++ show failingInput
    putStrLn $ "isProblematicUnclosedString: " ++ show (U.isProblematicUnclosedString failingInput)
    
    -- Test the property logic for this specific case
    let s = "a"
    let closed = "\"" ++ s ++ "\""
    let unclosed = "\"" ++ s
    let withEscaped = "\"" ++ s ++ "\\\""
    
    putStrLn $ "\nProperty logic for s = " ++ show s
    putStrLn $ "not (U.isProblematicUnclosedString closed): " ++ show (not (U.isProblematicUnclosedString closed))
    putStrLn $ "U.isProblematicUnclosedString unclosed: " ++ show (U.isProblematicUnclosedString unclosed)
    putStrLn $ "U.isCompleteStringLiteral withEscaped: " ++ show (U.isCompleteStringLiteral withEscaped)
    putStrLn $ "Overall property: " ++ show (not (U.isProblematicUnclosedString closed) && 
                                      U.isProblematicUnclosedString unclosed &&
                                      U.isCompleteStringLiteral withEscaped)

prop_is_problematic_unclosed_string :: String -> Property
prop_is_problematic_unclosed_string s =
  let closed = "\"" ++ s ++ "\""
      unclosed = "\"" ++ s
      withEscaped = "\"" ++ s ++ "\\\""
  in property $ not (U.isProblematicUnclosedString closed) && 
                U.isProblematicUnclosedString unclosed &&
                U.isCompleteStringLiteral withEscaped