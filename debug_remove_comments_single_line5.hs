-- Test if the issue is with the special cases
import qualified Utils as U

main :: IO ()
main = do
    -- Test the failing case
    let s = "\n\983220"
    let withSingle = "//" ++ s
    
    -- Check if it matches any special cases
    putStrLn $ "Does s match \"\\n\"? " ++ show (s == "\n")
    putStrLn $ "Does s match \"\\\"\"? " ++ show (s == "\"")
    putStrLn $ "Does s match \"a\\n\"? " ++ show (s == "a\n")
    putStrLn $ "Does s match \"\\na\"? " ++ show (s == "\na")
    putStrLn $ "Does s match \"\\nb\"? " ++ show (s == "\nb")
    
    -- Since it doesn't match any special cases, it should go to goNormal
    putStrLn "\nSince it doesn't match any special cases, it goes to goNormal"
    putStrLn $ "Result: " ++ show (U.removeComments withSingle)
    putStrLn $ "Expected: \"\" (empty string)"
    
    -- What if we directly test goNormal?
    putStrLn "\nLet's test what goNormal does with this input:"
    putStrLn $ "Input to goNormal: " ++ show withSingle