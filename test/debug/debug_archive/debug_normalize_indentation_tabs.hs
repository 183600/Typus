import Utils
import Data.List (isPrefixOf)

main :: IO ()
main = do
    let s = "\n"
    let withTabs = "\t\t" ++ s ++ "\t"
    let normalized = normalizeIndentation withTabs
    putStrLn $ "s = " ++ show s
    putStrLn $ "withTabs = " ++ show withTabs
    putStrLn $ "normalized = " ++ show normalized
    putStrLn $ "null s = " ++ show (null s)
    putStrLn $ "s == \" \" = " ++ show (s == " ")
    putStrLn $ "s == \"\\na\" = " ++ show (s == "\na")
    putStrLn $ "not (\"\\t\\t\" `isPrefixOf` normalized) = " ++ show (not ("\t\t" `isPrefixOf` normalized))