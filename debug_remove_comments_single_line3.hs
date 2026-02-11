-- Test various cases to understand expected behavior

import qualified Utils as U

main :: IO ()
main = do
    -- Test the specific cases from the test
    let testCases = 
            [ ("\n", "\n")  -- Should preserve newline
            , ("\"", "\"")  -- Should preserve quote
            , ("a\n", "a\n")  -- Should preserve a\n
            , ("\na", "\na")  -- Should preserve \na
            , ("\nb", "\nb")  -- Should preserve \nb
            , ("x", "")  -- Should remove comment
            , ("\n\983220", "")  -- Should remove comment (failing case)
            ]
    
    mapM_ (\(input, expected) -> do
        let withSingle = "//" ++ input
        let result = U.removeComments withSingle
        let status = if result == expected then "PASS" else "FAIL"
        putStrLn $ status ++ ": Input " ++ show input ++ " -> Got " ++ show result ++ ", Expected " ++ show expected
        ) testCases