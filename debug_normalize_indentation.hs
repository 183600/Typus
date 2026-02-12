import Utils (normalizeIndentation)
import Data.List (isPrefixOf)

main :: IO ()
main = do
    let s = "\n\1007127"
    let withTabs = "\t\t" ++ s ++ "\t"
    let normalized = normalizeIndentation withTabs
    
    putStrLn $ "Input s: " ++ show s
    putStrLn $ "With tabs: " ++ show withTabs
    putStrLn $ "Normalized: " ++ show normalized
    putStrLn $ "Starts with \\t\\t: " ++ show ("\t\t" `isPrefixOf` normalized)
    putStrLn $ "Expected: False"