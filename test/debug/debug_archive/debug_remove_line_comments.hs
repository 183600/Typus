import Utils
import Data.List

main :: IO ()
main = do
    let s = ""
    let withStringComment = "code // comment\n\"" ++ s ++ "// not comment\"\ncode"
    putStrLn "Input string:"
    putStrLn $ show withStringComment
    let processed = removeLineComments withStringComment
    putStrLn "Processed string:"
    putStrLn $ show processed
    putStrLn $ "Expected substring: " ++ show ("\"" ++ s ++ "// not comment\"")
    putStrLn $ "Contains expected: " ++ show (("\"" ++ s ++ "// not comment\"") `isInfixOf` processed)