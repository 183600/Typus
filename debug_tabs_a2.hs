import qualified Utils as U
import Data.Char (isControl)
import Data.List (isPrefixOf)

main :: IO ()
main = do
    let s = "a"
    let withTabs = "\t\t" ++ s ++ "\t"
    let normalized = U.normalizeIndentation withTabs
    putStrLn $ "s: " ++ show s
    putStrLn $ "withTabs: " ++ show withTabs
    putStrLn $ "normalized: " ++ show normalized
    putStrLn $ "isControl s: " ++ show (isControl (head s))
    putStrLn $ "startsWithTwoTabs: " ++ show ("\t\t" `isPrefixOf` normalized)
    putStrLn $ "Test expects not (\"\\t\\t\" `isPrefixOf` normalized): " ++ show (not ("\t\t" `isPrefixOf` normalized))
    
    -- 根据测试，对于普通字符，前导制表符应该被转换为空格
    let expected = "  a\t"
    putStrLn $ "Expected (based on test logic): " ++ show expected
    putStrLn $ "Test passes: " ++ show (not ("\t\t" `isPrefixOf` normalized))