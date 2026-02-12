import Utils
import Data.List (isPrefixOf)

main :: IO ()
main = do
  let s = " "
  let withTabs = "\t\t" ++ s ++ "\t"
  let normalized = Utils.normalizeIndentation withTabs
  putStrLn $ "s: " ++ show s
  putStrLn $ "withTabs: " ++ show withTabs
  putStrLn $ "normalized: " ++ show normalized
  putStrLn $ "startsWithTabs: " ++ show ("\t\t" `isPrefixOf` normalized)
  putStrLn $ "Test passes: " ++ show (not ("\t\t" `isPrefixOf` normalized))