import Test.Tasty
import Test.Tasty.QuickCheck
import Utils

-- 测试 removeLineComments 对特定输入的行为
testSpecificCase :: TestTree
testSpecificCase = testCase "Remove line comments specific case" $ do
    let s = "'"
    let stringWithComment = s ++ " // comment"
    let result = removeLineComments stringWithComment
    putStrLn $ "s = " ++ show s
    putStrLn $ "stringWithComment = " ++ show stringWithComment
    putStrLn $ "result = " ++ show result
    putStrLn $ "expected = " ++ show (s ++ " // comment")
    assertEqual "should preserve string literal" (s ++ " // comment") result

main :: IO ()
main = do
    defaultMain $ testGroup "Tests" [testSpecificCase]