import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)
import Data.List (isInfixOf)

tests :: TestTree
tests =
  testGroup "Regex tests"
    [ testCase "test expected.*int pattern" $ do
        let message = "expected int, got string"
        assertBool "should match expected.*int" ("expected.*int" `isInfixOf` message)
        assertBool "should match got.*string" ("got.*string" `isInfixOf` message)
    ]

main :: IO ()
main = do
    putStrLn "Running regex tests..."
    -- This would need tasty to run, but let's just test the logic
    let message = "expected int, got string"
    putStrLn $ "Message: " ++ message
    putStrLn $ "Matches expected.*int: " ++ show ("expected.*int" `isInfixOf` message)
    putStrLn $ "Matches got.*string: " ++ show ("got.*string" `isInfixOf` message)