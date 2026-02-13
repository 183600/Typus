import qualified Utils as U

main :: IO ()
main = do
    let input = "\b"
    let withTabs = "\t\t" ++ input ++ "\t"
    let normalized = U.normalizeIndentation withTabs
    putStrLn $ "Input: " ++ show input
    putStrLn $ "With tabs: " ++ show withTabs
    putStrLn $ "Normalized: " ++ show normalized
    putStrLn $ "Expected: " ++ show withTabs
    putStrLn $ "Test passes: " ++ show (normalized == withTabs)