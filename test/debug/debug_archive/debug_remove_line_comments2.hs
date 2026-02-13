import Utils
import Data.List

main :: IO ()
main = do
    let s = ""
    let testString = "\"" ++ s ++ "// not comment\""
    putStrLn "Test string alone:"
    putStrLn $ show testString
    let processed = removeSingleLineComments testString
    putStrLn "Processed string alone:"
    putStrLn $ show processed
    
    putStrLn "\nFull test case:"
    let withStringComment = "code // comment\n\"" ++ s ++ "// not comment\"\ncode"
    putStrLn $ "Input: " ++ show withStringComment
    let lines = lines withStringComment
    putStrLn $ "Lines: " ++ show lines
    let processedLines = map removeSingleLineComments lines
    putStrLn $ "Processed lines: " ++ show processedLines
    let final = intercalate "\n" processedLines
    putStrLn $ "Final result: " ++ show final
    putStrLn $ "Expected substring: " ++ show ("\"" ++ s ++ "// not comment\"")
    putStrLn $ "Contains expected: " ++ show (("\"" ++ s ++ "// not comment\"") `isInfixOf` final)
  where
    removeSingleLineComments = undefined  -- 我们需要从Utils中导入这个函数