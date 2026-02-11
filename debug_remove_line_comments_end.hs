import qualified Utils as U
import Data.List (isInfixOf, elem)

main :: IO ()
main = do
    let s = "a/"
    let withComment = s ++ "// comment"
    let processed = U.removeLineComments withComment
    
    putStrLn $ "Input: " ++ show withComment
    putStrLn $ "Expected: " ++ show s
    putStrLn $ "Actual: " ++ show processed
    putStrLn $ "Test passes: " ++ show (processed == s)
    
    -- 测试breakOn的行为
    putStrLn "\nTesting breakOn behavior:"
    putStrLn $ "U.breakOn \"//\" \"a/// comment\" = " ++ show (U.breakOn "//" "a/// comment")
    
    -- 测试条件
    putStrLn "\nTesting conditions:"
    putStrLn $ "// `isInfixOf` \"a/// comment\" = " ++ show ("//" `isInfixOf` "a/// comment")
    putStrLn $ "not (\"'\" `isInfixOf` \"a/// comment\") = " ++ show (not ("'" `isInfixOf` "a/// comment"))
    putStrLn $ "not ('\\n' `elem` \"a/// comment\") = " ++ show (not ('\n' `elem` "a/// comment"))