import Utils
import Data.List (isPrefixOf)

main :: IO ()
main = do
    let s = " f"
    let withTabs = "\t\t" ++ s ++ "\t"
    let normalized = Utils.normalizeIndentation withTabs
    putStrLn $ "s = " ++ show s
    putStrLn $ "withTabs = " ++ show withTabs
    putStrLn $ "normalized = " ++ show normalized
    putStrLn $ "not (\"\\t\\t\" `isPrefixOf` normalized) = " ++ show (not ("\t\t" `isPrefixOf` normalized))