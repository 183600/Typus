import Utils
import Data.List (isPrefixOf)
import Data.Char (isSpace)

main :: IO ()
main = do
    let s = "\v"
    let withTabs = "\t\t" ++ s ++ "\t"
    let normalized = normalizeIndentation withTabs
    putStrLn $ "s=" ++ show s
    putStrLn $ "withTabs=" ++ show withTabs
    putStrLn $ "normalized=" ++ show normalized
    putStrLn $ "startsWithTabs=" ++ show ("\t\t" `isPrefixOf` normalized)
    
    -- Test the relative test
    let lines' = lines s
    let normLines = lines normalized
    putStrLn $ "lines'=" ++ show lines'
    putStrLn $ "normLines=" ++ show normLines
    putStrLn $ "length lines' <= 1=" ++ show (length lines' <= 1)
    putStrLn $ "all isSpace s && not (null s)=" ++ show (all isSpace s && not (null s))