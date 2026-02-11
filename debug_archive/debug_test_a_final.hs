import Utils
import Data.List (isPrefixOf)

main :: IO ()
main = do
  let s = "a"
      withTabs = "\t\t" ++ s ++ "\t"
      normalized = normalizeIndentation withTabs
  putStrLn $ "s: " ++ show s
  putStrLn $ "withTabs: " ++ show withTabs
  putStrLn $ "normalized: " ++ show normalized
  putStrLn $ "normalized starts with tabs: " ++ show ("\t\t" `isPrefixOf` normalized)
  putStrLn $ "not (\"\\t\\t\" `isPrefixOf` normalized): " ++ show (not ("\t\t" `isPrefixOf` normalized))