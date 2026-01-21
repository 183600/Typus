import Utils
import Data.List (isInfixOf)

-- 测试prop_removeComments_properties
prop_removeComments_properties :: String -> Bool
prop_removeComments_properties s = 
  let result = removeComments s
      hasLineComment = "//" `isInfixOf` result
      hasBlockComment = "/*" `isInfixOf` result
  in not (hasLineComment || hasBlockComment)

main :: IO ()
main = do
    -- 测试一些特定的案例
    let testCases = 
            [ "code // comment"
            , "\"string // not comment\" // real comment"
            , "'c' // comment"
            , "\"string // not comment\""
            , "'c // not comment'"
            , "/* block comment */"
            , "\"string /* not comment */\" /* real comment */"
            ]
    
    mapM_ (\testCase -> do
        let result = removeComments testCase
            passed = prop_removeComments_properties testCase
        putStrLn $ "Input: " ++ show testCase
        putStrLn $ "Output: " ++ show result
        putStrLn $ "Test passed: " ++ show passed
        if not passed
            then putStrLn $ "ERROR: Comments still present!"
            else putStrLn $ "OK"
        putStrLn "---"
        ) testCases