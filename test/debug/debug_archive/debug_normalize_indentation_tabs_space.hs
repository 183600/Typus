import Utils (normalizeIndentation)
import Data.List (isPrefixOf)

main :: IO ()
main = do
  -- Test the specific failing case
  let s = " "
      withTabs = "\t\t" ++ s ++ "\t"
      normalized = normalizeIndentation withTabs
  putStrLn $ "s: " ++ show s
  putStrLn $ "withTabs: " ++ show withTabs
  putStrLn $ "normalized: " ++ show normalized
  putStrLn $ "normalized == \" \": " ++ show (normalized == " ")
  putStrLn $ "not (\"\\t\\t\" `isPrefixOf` normalized): " ++ show (not ("\t\t" `isPrefixOf` normalized))