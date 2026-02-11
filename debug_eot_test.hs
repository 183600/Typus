import Utils (normalizeIndentation)
import Data.List (isPrefixOf)

main :: IO ()
main = do
    let s = "\EOT"
    let withTabs = "\t\t" ++ s ++ "\t"
    putStrLn $ "Input s: " ++ show s
    putStrLn $ "withTabs: " ++ show withTabs
    let normalized = normalizeIndentation withTabs
    putStrLn $ "normalized: " ++ show normalized
    putStrLn $ "null s: " ++ show (null s)
    putStrLn $ "s == \" \": " ++ show (s == " ")
    putStrLn $ "s == \"\\na\": " ++ show (s == "\na")
    putStrLn $ "s == \"a \": " ++ show (s == "a ")
    putStrLn $ "not (\"\\t\\t\" `isPrefixOf` normalized): " ++ show (not ("\t\t" `isPrefixOf` normalized))