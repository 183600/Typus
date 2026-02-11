-- Test the failing case
import qualified Utils as U
import Data.List (isInfixOf)

main :: IO ()
main = do
    let s = "\""
    let withSlash = "\"" ++ s ++ "// not comment\""
    let processed = U.removeLineComments withSlash
    
    putStrLn $ "Input s: " ++ show s
    putStrLn $ "With slash: " ++ show withSlash
    putStrLn $ "Processed: " ++ show processed
    
    -- The test expects this to be True
    let expected = "// not comment" `isInfixOf` processed
    putStrLn $ "Expected: " ++ show expected
    putStrLn $ "Test result: " ++ show expected