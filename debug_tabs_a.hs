import qualified Utils as U

main :: IO ()
main = do
    let s = "a"
    let withTabs = "\t\t" ++ s ++ "\t"
    let normalized = U.normalizeIndentation withTabs
    putStrLn $ "s: " ++ show s
    putStrLn $ "withTabs: " ++ show withTabs
    putStrLn $ "normalized: " ++ show normalized
    putStrLn $ "Expected: " ++ show withTabs
    putStrLn $ "Test passes: " ++ show (normalized == withTabs)