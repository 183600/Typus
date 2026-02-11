import Utils
import Data.List (isPrefixOf)

main :: IO ()
main = do
    putStrLn $ "normalizeIndentation \" \" = " ++ show (normalizeIndentation " ")
    putStrLn $ "Expected: \"    \""
    
    let withTabs = "\t\ta\t"
    let normalized = normalizeIndentation withTabs
    putStrLn $ "normalizeIndentation \"\\t\\ta\\t\" = " ++ show normalized
    putStrLn $ "Starts with tabs: " ++ show ("\t\t" `isPrefixOf` normalized)
    
    let lines' = [""]
    let withMixed = map ("\t  " ++) lines'
    let normalized2 = normalizeIndentation (unlines withMixed)
    putStrLn $ "normalizeIndentation (unlines [\"\\t  \"]) = " ++ show normalized2
    putStrLn $ "Expected: \"\\t  \\n\""